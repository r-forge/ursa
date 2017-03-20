'value_xy' <- function(obj,...) .getValue(obj,.getIndex(obj,...))
'value_ll' <- function(obj,...) {
   if (!is.ursa(obj))
      return(NULL)
   arglist <- list(...)
   lon <- .getPrm(arglist,name="^lon",default=NA_real_)
   lat <- .getPrm(arglist,name="^lat",default=NA_real_)
   stopifnot(length(lon)==length(lat))
   xy <- .project(cbind(lon,lat),ursa_proj(obj),inv=FALSE)
   .getValue(obj,.getIndex(obj,xy[,1],xy[,2]))
}
'value_cr' <- function(obj,...) {
   arglist <- list(...)
   n <- length(arglist)
   if (!n)
      return(NULL)
   if (n==1)
      ind <- arglist[[1]]
   else {
      lc <- .getPrm(arglist,name="^c",default=NA_real_)
      lr <- .getPrm(arglist,name="^r",default=NA_real_)
      ind <- (lr-1L)*n+lc
   }
   .getValue(obj=obj,ind=ind)
}
'coord_xy' <- function(obj,...)
{
   arglist <- list(...)
   n <- length(arglist)
   if (!n)
      return(NULL)
   if (n==1)
      ind <- arglist[[1]]
   else {
      x <- .getPrm(arglist,name="^x",default=NA_real_)
      y <- .getPrm(arglist,name="^y",default=NA_real_)
      ind <- .getIndex(obj,x,y)
   }
   n <- obj$grid$columns
   res <- matrix(NA,nrow=2,ncol=length(ind)
                ,dimnames=list(c("c","r"),ind))
   if ((!missing(ind))&&(length(ind)>0))
   {
      res["r",] <- (ind-1L)%/%n+1L
      res["c",] <- (ind-1L)%%n+1L
   }
   res
}
'coord_cr' <- function(obj,...)
{
   if (!is.ursa(obj))
      return(NULL)
   arglist <- list(...)
   n <- length(arglist)
   if (!n)
      return(NULL)
   nc <- obj$grid$columns
   if (n==1) {
      ind <- arglist[[1]]
      row <- (ind-1L)%/%nc+1L
      col <- (ind-1L)%%nc+1L
   }
   else {
      col <- .getPrm(arglist,name="^c",default=NA_integer_)
      row <- .getPrm(arglist,name="^r",default=NA_integer_)
     # ind <- (lr-1L)*nc+lc
   }
   res <- matrix(NA,nrow=2,ncol=length(ind)
                ,dimnames=list(c("x","y"),ind))
   res["x",] <- with(ursa_grid(obj),minx+(col-0.5)*resx)
   res["y",] <- with(ursa_grid(obj),miny+(rows-row+0.5)*resy)
   res
}
'.getValue' <- function(obj,ind,col,row)
{
   if (!is.ursa(obj))
      return(NULL)
   n <- obj$grid$columns
   if ((!missing(ind))&&(length(ind)!=-11L))
   {
      row <- (ind-1L)%/%n+1L
      col <- (ind-1L)%%n+1L
   }
   else {
      if (length(col)!=length(row))
         return(NULL)
      ind <- n*(row-1L)+col
   }
  # str(obj[row,])
   if ((FALSE)&&(length(obj$value))&&(is.na(obj$value)))
   {
      print(str(obj))
      print(c(ind=ind,c=col,r=row))
   }
   nc <- length(col)
   res <- matrix(NA,ncol=nc,nrow=nband(obj),dimnames=list(bandname(obj),ind))
   for (i in seq(nc))
      res[,i] <- obj[,row[i]]$value[col[i],]
  # obj$value[ind,] ## incorrect if use "open_envi" construction
   res
}
'.getIndex' <- function(obj,x,y)
{
   if (!is.ursa(obj))
      return(NULL)
   if (missing(y))
   {
      if (length(x)==2)
      {
         y <- x[2]
         x <- x[1]
      }
      else if (ncol(x)==2)
      {
         y <- x[,2]
         x <- x[,1]
      }
      else
         stop("specify 'y'")
   }
   if (TRUE) {
      columns <- obj$grid$columns
      rows <- obj$grid$rows
   }
   else if ((!is.na(obj$con$samples))&&(!is.na(obj$con$lines)))
   {
      columns <- obj$con$samples
      rows <- obj$con$lines
   }
   else
   {
      columns <- obj$grid$columns
      rows <- obj$grid$rows
   }
   nx <- length(x)
   ny <- length(y)
   if (nx!=-11)
   {
      x2 <- with(obj$grid,(seq(minx,maxx,resx)-0.5*resx)[-1])
      whichx <- numeric(nx)
      for (i in seq(nx))
         whichx[i] <- which.min(abs(x2-x[i]))
   }
   else
      stop("TODO#1-X")
   if (ny!=-11)
   {
      y2 <- with(obj$grid,(seq(miny,maxy,resy)-0.5*resy)[-1])
      whichy <- numeric(ny)
      for (i in seq(ny))
         whichy[i] <- rows-which.min(abs(y2-y[i]))+1
   }
   else
      stop("TODO#1-Y")
   ind <- as.integer((whichy-1)*columns+(whichx-1)+1)
   ind
}
