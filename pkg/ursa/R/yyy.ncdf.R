'.open_ncdf' <- function(fname,var="",level=1,grid=FALSE,verbose=FALSE) {
   if (!is.character(fname))
      return(NULL)
   requireNamespace("ncdf4",quietly=.isPackageInUse())
  # opW <- options(warn=0-!verbose) ## to prevent 'GeoTransform values not available'
   nc <- try(ncdf4::nc_open(fname))
   if (inherits(nc,"try-error")) {
     # cat(geterrmessage())
      return(NULL) 
   }
   print(object.size(nc))
   varList <- names(nc$var)
   ##~ a <- ncvar_get(nc,"Times")
   ##~ print(a)
   ##~ q()
   if (length(varList)==1)
      varName <- varList
   else if (!is.na(var))
      varName <- .grep(var,varList,value=TRUE)[1]
   else if (!grid)
      stop(paste("","-----","Please specify variable:"
                ,paste(paste(seq(along=varList),". ",varList,sep=""),collapse="\n")
                ,"-----",sep="\n"))
   else
      varName <- varList[1]
   a <- nc$var[[varName]]$dim
   if (is.null(a))
      a <- nc$dim[varName]
   b <- vector("list",length(a))
   names(b) <- lapply(a,function(x) x$name)
   b[] <- lapply(a,function(x){
      isTime <- ((length(x$units)>0)&&(.lgrep("\\ssince\\s",x$units)))
      y <- x$val
      if ((!FALSE)&&(isTime)) {
         or <- .gsub2("\\D(\\d+-\\d+-\\d+)\\D+(\\d+:\\d+:\\d+(\\.\\d+)*)","\\1 \\2",x$units)
         if (or==x$units)
            or <- .gsub2("\\D(\\d+-\\d+-\\d+)","\\1",x$units)
         if (or==x$units)
            stop(paste("incorrect parsing for 'origin' detection:",dQuote(x$units)))
         if (.lgrep("second(s)* since",x$units))
            NULL
         else if (.lgrep("hour(s)* since",x$units))
            y <- y*60*60
         else if (.lgrep("day(s)* since",x$units))
            y <- y*60*60*24
         else if (.lgrep("since",x$units))
            stop(paste("incorrect parsing for 'origin' detection:",dQuote(x$units)))
         y <- as.POSIXct(y,origin=or,tz="UTC")
      }
      y
   })
   b$proj4 <- "" #ncdf4.helpers::nc.get.proj4.string(nc,varName)
   if (grid) {
      ncdf4::nc_close(nc)
      return(b)
   }
   con <- .con.skeleton()
   dima <- nc$var[[varName]]$size
   str(b)
   q()
   con$driver <- "NCDF"
   con$offset <- dima
   con$seek <- FALSE
   bname <- names(b)
   isColRow <- !((.lgrep("^(x|y)$",names(b))==2) |
                 (.lgrep("^(lon|lat)",names(b))==2) |
                 (.lgrep("^(west.+east|south.+north)",names(b))==2) |
                 (.lgrep("^(east|north)",names(b))==2))
  # print(isColRow)
  # att <- ncdf4::ncatt_get(nc,varName,attname=NA,verbose=FALSE)
  # str(att)
   b2 <- b[[2]]
   if ((is.numeric(b2))&&((all(diff(b2)<0))||(isColRow))) {
      dima[2] <- -dima[2]
      b[[2]] <- rev(b2)
      ##~ if (length(dima)==2)
         ##~ res[] <- res[,rev(seq(dima[2])),drop=FALSE]
      ##~ else if (length(dima)==3)
         ##~ res[] <- res[,rev(seq(dima[2])),,drop=FALSE]
      ##~ else if (length(dima)==4)
         ##~ res[] <- res[,rev(seq(dima[2])),,,drop=FALSE]
      ##~ else
         ##~ stop(dima)
   }
   if ((.lgrep("^(lat|y$|south.+north|north)",bname[1]))&&
       (.lgrep("^(lon|x$|west.+east|east)",bname[2]))) {
      print("transpose coordinates")
      names(b)[c(1,2)] <- bname[c(2,1)]
      b[c(1,2)] <- b[c(2,1)]
      con$swap <- TRUE
      str(b)
      q()
      ##~ if (length(dima)==2)
         ##~ res <- aperm(res,c(2,1))
      ##~ else if (length(dima)==3)
         ##~ res <- aperm(res,c(2,1,3))
      ##~ else if (length(dima)==4)
         ##~ res <- aperm(res,c(2,1,3,4))
      ##~ else
         ##~ stop(dima)
   }
   else
      con$swap <- FALSE
  # con$handle <- nc ## temporal hiding
   str(con)
   q()
  # res <- ncdf4::ncvar_get(nc,varName,collapse_degen=FALSE)
   str(b)
   q()
   a1 <- as.numeric(a)
   g1 <- regrid()
   g1$rows <- as.integer(a1[1])
   g1$columns <- as.integer(a1[2])
   nl <- as.integer(a1[3])
   g1$minx <- a1[4]
   g1$miny <- a1[5]
   g1$resx <- a1[6]
   g1$resy <- a1[7]
   g1$maxx <- with(g1,minx+resx*columns)
   g1$maxy <- with(g1,miny+resy*rows)
   g1$proj4 <- attr(a,"projection")
   if (is.na(g1$proj4))
      g1$proj4 <- ""
   b1 <- attr(a,"mdata")
  # if (!is.null(b1))
   bname <- .gsub("^Band_\\d+=\\t*(.+)$","\\1",.grep("band",b1,value=TRUE))
   c1 <- attr(a,"df")
   hasndv <- unique(c1$hasNoDataValue)
   nodata <- unique(c1$NoDataValue)
   nodata <- if ((length(hasndv)==1)&&(length(nodata)==1)&&(hasndv)) nodata
             else NA
  # print(length(attr(a,"ColorTable")))
   ct <- attr(a,"ColorTable")
   ca <- attr(a,"CATlist")
   if ((length(ct))&&(!is.null(ct[[1]]))) {
      ct <- ct[[1]]
      if ((length(ca))&&(!is.null(ca[[1]]))) {
         nval <- ca[[1]]
         ct <- ct[seq(length(nval))]
      }
      else
         nval <- NULL #seq(length(ct))
      names(ct) <- nval
   }
   else if ((length(ca))&&(!is.null(ca[[1]]))) {
      nval <- ca[[1]]
      ct <- rep(NA,length(nval))
      names(ct) <- nval
   }
   else
      ct <- character()
   class(ct) <- "ursaColorTable"
   dset <- methods::new("GDALReadOnlyDataset",fname)
   dima <- dim(dset)
   if (length(dima)==2)
      dima <- c(dima,1L)
   if (!length(bname)) {
      bname <- paste("Band",if (length(dima)==3) seq(dima[3]) else 1L)
   }
   session_grid(g1)
   res <- .raster.skeleton()
   res$dim <- c(dima[1]*dima[2],dima[3])
   con <- .con.skeleton()
   con$driver <- "GDAL"
   con$samples <- g1$columns
   con$lines <- g1$rows
   con$bands <- length(bname)
   con$indexC <- seq(g1$columns)
   con$indexR <- seq(g1$rows)
   con$indexZ <- seq_along(bname)
   con$seek <- FALSE
   con$fname <- fname
   con$handle <- dset
   res$con <- con
   ursa_grid(res) <- g1
   ursa_colortable(res) <- ct
   class(res$value) <- ifelse(length(ct),"ursaCategory","ursaNumeric")
   ursa_nodata(res) <- nodata
   names(res) <- bname
   res
}
