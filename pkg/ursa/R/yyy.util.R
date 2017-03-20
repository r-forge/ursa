'.elapsedTime' <- function(message="",reset=FALSE,toPrint=FALSE)
{
   startTime <- getOption("ursaTimeStart")
   deltaTime <- getOption("ursaTimeDelta")
   if (message=="")
      message <- paste(as.character(Sys.time()),"***")
   else
      message <- paste(message,":",sep="")
   mytext <- sprintf("*** %s: %s %.2f(%.2f) seconds ***"
                   # ,as.character(Sys.time())
                    ,basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
                    ,message,(proc.time()-startTime)[3]
                    ,(proc.time()-deltaTime)[3])
   if (reset)
      options(ursaTimeStart=proc.time())
   options(ursaTimeDelta=proc.time())
   if (toPrint)
      print(mytext)
   return (message(mytext))
}
'.round' <- function(x,digits=0,eps=.Machine$double.eps*9)
{
   round(x+sign(x)*eps,digits=digits)
}
'.try' <- function(...)
{
   a <- try(...)
   if ((is.character(a))&&(class(a)=="try-error"))
      return(FALSE)
   return(TRUE)
}
'.dir' <- function(pattern=NULL,path=".",all.files=FALSE,full.names=FALSE
                ,recursive=FALSE,ignore.case=TRUE,include.dirs=FALSE)
{
   a <- dir(path=path,pattern=NULL,all.files=all.files,full.names=full.names
           ,recursive=recursive,ignore.case=ignore.case,include.dirs=include.dirs)
   if (is.null(pattern))
      return (a)
   if (!.try(b <- basename(a)))
   {
      b <- a
      for (i in seq(along=a))
         if (!.try(b[i] <- basename(a[i])))
            b[i] <- NA
   }
   a[grep(pattern,b,perl=TRUE,ignore.case=ignore.case)]
}
'.grep' <- function(pattern,x,ignore.case=TRUE,perl=TRUE
                  ,value=FALSE,fixed=FALSE,useBytes=FALSE,invert=FALSE)
{
   grep(pattern,x,ignore.case=ignore.case,perl=perl
       ,value=value,fixed=fixed,useBytes=useBytes,invert=invert)
}
'.gsub' <- function(pattern,replacement,x,ignore.case=TRUE
                  ,perl=TRUE,fixed=FALSE,useBytes=FALSE)
{
   gsub(pattern,replacement,x,ignore.case=ignore.case
              ,perl=perl,fixed=fixed,useBytes=useBytes)
}
'.gsub2' <- function(pattern,replacement,x,ignore.case=TRUE
                  ,perl=TRUE,fixed=FALSE,useBytes=FALSE)
{
   mypattern <- sprintf("^.*%s.*$",pattern)
   gsub(mypattern,replacement,x,ignore.case=ignore.case
       ,perl=perl,fixed=fixed,useBytes=useBytes)
}
'.lgrep' <- function(pattern,x,ignore.case=TRUE,perl=TRUE
                  ,value=FALSE,fixed=FALSE,useBytes=FALSE,invert=FALSE)
{
   length(grep(pattern,x,ignore.case=ignore.case,perl=perl
               ,value=value,fixed=fixed,useBytes=useBytes,invert=invert))
}
'.dirname' <- function(x)
{
   a <- gregexpr("(/|\\\\)",x,ignore.case=TRUE,perl=TRUE)
   .gsub("^$",".",substr(x,1,max(a[[1]]-1)))
}
'.basename' <- function(x)
{
   a <- gregexpr("(/|\\\\)",x,ignore.case=TRUE,perl=TRUE)
   substr(x,max(a[[1]])+1,nchar(x))
}
'.gc' <- function(verbose=FALSE)
{
   a1 <- gc()
   a2 <- gc(reset=TRUE)
   if (verbose)
   {
      print(a1)
      print(a2)
   }
   NULL
}
# '.paste' <- function(...,sep="",collapse=NULL) paste(...,sep=sep,collapse=collapse)
'.maketmp' <- function(n=1,ext="",prefix="") 
{
   if (!nchar(prefix)) {
      prefix <- basename(tempfile("","."))
      k <- nchar(prefix)
      prefix <- substr(prefix,k-3,k)
   }
   tcount <- getOption("ursaTempFileCount")
   if (is.null(tcount))
      tcount <- 0L
   list1 <- vector("character",length=n)
   for (i in seq(along=list1))
   {
      list1[i] <- sprintf("tmp%02d_%s",tcount+i,prefix)
     # list1[i] <- sprintf("tmp%s_%02d",prefix,tcount+i)
   }
   if (nchar(ext))
   {
      ext <- .gsub("^\\.","",ext)
      list1 <- paste(list1,ext,sep=".")
   }
   options(ursaTempFileCount=tcount+n)
   paste0("___",list1)
}
'.args2list' <- function() {
   args <- commandArgs(TRUE)
   if (!length(args))
      return(NULL)
   if (FALSE)
      a <- strsplit(args,"=")
   else {
      a <- vector("list",length(args))
      for (i in seq_along(args)) {
         ind <- .grep("=",unlist(strsplit(args[i],"")))
         if (length(ind))
            a[[i]] <- c(substr(args[i],1,ind[1]-1)
                       ,substr(args[i],ind[1]+1,nchar(args[i])))
         else
            a[[i]] <- args[i]
      }
   }
   aname <- sapply(a,function(x){if (length(x)==1) "" else x[1]})
   opE <- options(warn=-1,show.error.messages=FALSE)
   a <- lapply(a,function(x){
      n <- length(x)
      y <- x[n]
      if (TRUE) {
        # if (n>1) ## only if named?
         if (y=="NULL")
            y <- NULL
         else if (.try(z <- eval(parse(text=y)))) {
            if (!is.null(z))
               y <- z
         }
      }
      else {
         z <- as.logical(y)
         if (!is.na(z))
            return(z)
         if (.lgrep("^(-)*\\d+$",y))
            return(as.integer(y))
         if (.lgrep("^(-)*\\d+\\.\\d+$",y))
            return(as.numeric(y))
      }
      y
   })
   options(opE)
   names(a) <- aname
   a
}
'.is.integer' <- function(x,tolerance=1e-11) {
   if (is.ursa(x))
      x <- c(x$value)
   if (any(abs(x)>1e9))
      return(FALSE)
   y <- abs(x-as.integer(round(x)))
   if (all(x>100)) {
      y <- y/x
   }
   all(y<tolerance)
}
'.is.rgb' <- function(obj) {
   if (.is.colortable(obj))
      return(FALSE)
   if (storage.mode(obj$value)!="integer")
      return(FALSE)
   if (!(nband(obj) %in% c(3,4)))
      return(FALSE)
   minv <- min(obj$value,na.rm=TRUE)
   maxv <- max(obj$value,na.rm=TRUE)
   if ((minv>=0)&&(maxv>=200)&&(maxv<=255))
      return(TRUE)
   FALSE
}
'.ursaOptions' <- function() {
   op <- options()
   op <- op[.grep("^ursaPng.+",names(op))]
   str(op)
}
'.skipPlot' <- function(onPanel=TRUE) {
   toPlot <- getOption("ursaPngPlot")
   if ((!is.logical(toPlot))||(!toPlot))
      return(TRUE)
   if (!onPanel)
      return(FALSE)
   getOption("ursaPngSkip")
}
'.dist2' <- function(df,xy,summarize=!FALSE,positive=FALSE,verbose=TRUE)
{
   if (identical(df,xy))
      positive <- TRUE
   '.modal2' <- function(x,...)
   {
      z <- density(x,...)
      y <- z$x[match(max(z$y),z$y)]
      y
   }
   '.modal3' <- function(x) {
      res <- NA
      if (requireNamespace("locfit",quietly=TRUE))
         try(res <- x[which.max(predict(locfit::locfit(~x),newdata=x))])
      res
   }
   d1 <- dim(df)
   d2 <- dim(xy)
   if ((length(d1)<2)||(d1[2]<2)||(length(d2)<2)||(d2[2]<2))
      return(NULL)
   b1 <- .Cursa("dist2dist",x1=as.numeric(xy[,"x"]),y1=as.numeric(xy[,"y"])
                       ,x2=as.numeric(df[,"x"]),y2=as.numeric(df[,"y"])
                       ,nxy=nrow(xy),ndf=nrow(df),positive=as.integer(positive)
                       ,verb=as.integer(verbose)
                       ,dist=numeric(nrow(df)),ind=integer(nrow(df)))
   b1 <- data.frame(ind=b1$ind+1L,dist=b1$dist)
   if (summarize)
   {
      d <- b1$dist
      if (!.try(m <- .modal3(d)))
         m <- NA
      print(c(avg=mean(d),median=median(d),mode2=.modal2(d),mode3=m))
   }
   b1
}
'.is.eq' <- function(x,value) {
   if (abs(value)<1)
      abs(x-value)<1e-27
   else
      abs(x/value-1)<1e-6
}
'.is.ge' <- function(x,value) x>value | .is.eq(x,value)
'.is.le' <- function(x,value) x<value | .is.eq(x,value)
'.is.gt' <- function(x,value) x>value
'.is.lt' <- function(x,value) x<value
'.is.near' <- function(x1,x2) {
   m1 <- match(x1,x2)
   if (all(!is.na(m1))) ## 20161222 add 'all', removed 'any'
      return(m1)
   n1 <- length(x1)
   n2 <- length(x2)
   b1 <- .Cursa("isNear",x1=as.numeric(x1),x2=as.numeric(x2),n1=n1,n2=n2
           ,res=integer(n1),NAOK=TRUE)$res
   b1[b1==0] <- NA
   b1
}
'.getMajorSemiAxis' <- function(proj4) {
   ell <- .gsub(".*\\+ellps=(\\S+)\\s.*","\\1",proj4)
   if (ell=="WGS84")
      B <- 6378137
   else if (ell==proj4) {
      B <- .gsub(".*\\+a=(\\S+)\\s.*","\\1",proj4)
      if (B!=proj4)
         B <- as.numeric(B)
      else {
         opW <- options(warn=-1)
         warning("Supposed that this projection is not supported yet")
         options(opW)
         B <- 6378137
      }
   }
   else {
      opW <- options(warn=-1)
      warning("Supposed that this projection is not supported yet")
      options(opW)
      B <- 6378137
   }
   B
}
'.gdalwarp' <- function(src,dst=NULL,crs=NULL,resample="near",nodata=NA
                       ,resetGrid=FALSE,verbose=1L) {
  # a <- open_envi(src)
  # ct <- ursa_colortable(a)
  # close(a)
   if (is.ursa(src)) {
      removeSrc <- TRUE
      .src <- src
      nodata <- ignorevalue(src)
      src <- .maketmp(ext=".")
      write_envi(.src,src)
   }
   else {
      removeSrc <- FALSE
     # nodata <- NA
   }
   inMemory <- is.null(dst)
   if (inMemory)
      dst <- .maketmp(ext=".")
   if (is.null(crs)) {
      crs <- getOption("ursaSessionGrid")
   }
   else
      crs <- ursa_grid(crs)
   if (verbose)
      print(c(inMemory=inMemory,removeSrc=removeSrc,isNullCrs=is.null(crs)))
   if (is.null(crs))
      cmd <- paste("gdalwarp -overwrite -of ENVI"
                  ,ifelse(is.na(nodata),"",paste("-srcnodata",nodata,"-dstnodata",nodata))
                  ,"-r",resample,src,dst)
   else
      cmd <- with(crs,paste("gdalwarp -overwrite -of ENVI"
                        ,"-t_srs",dQuote(proj4),"-tr",resx,resy,"-te",minx,miny,maxx,maxy
                        ,ifelse(is.na(nodata),"",paste("-srcnodata",nodata,"-dstnodata",nodata))
                        ,"-r",resample,src,dst))
   if (verbose)
      message(cmd)
   if (verbose>1)
      return(NULL)
   system(cmd)
   session_grid(NULL)
   if (inMemory)
      ret <- read_envi(dst)
   else
      ret <- open_envi(dst)
   if (!is.na(nodata)) {
      ignorevalue(ret) <- nodata
      if (inMemory)
         ret[ret==nodata] <- NA
   }
   if (inMemory)
      envi_remove(dst)
   if (removeSrc)
      envi_remove(src)
   if (resetGrid)
      session_grid(ret)
   ret
}
'.isRscript' <- function() {
   ret <- !is.na(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
  # print(c(isRscript=ret))
   ret
}
