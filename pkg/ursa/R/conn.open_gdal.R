'open_gdal' <- function(fname,verbose=FALSE) {
  ## 20170116 removed '...' argument
   if (!is.character(fname))
      return(NULL)
   requireNamespace("rgdal",quietly=.isPackageInUse())
  # if (verbose)
  #    .elapsedTime("rgdal has been loaded")
   opW <- options(warn=0-!verbose) ## to prevent 'GeoTransform values not available'
   a <- try(rgdal::GDALinfo(fname,returnStats=FALSE,returnRAT=FALSE
                ,returnColorTable=TRUE,returnCategoryNames=TRUE),silent=TRUE)
   if (inherits(a,"try-error")) {
      if ((!.lgrep("\\.(rds)$",fname))&&(file.exists(fname))) # 20170529 patch for failure with 'rgdal'
         return(ursa_new()) 
      return(NULL) 
   }
   else
      isFail <- FALSE
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
