'as_ursa' <- function(obj,...) as.ursa(obj=obj,...)
'as.ursa' <- function(obj,...) {
   if (missing(obj))
      return(ursa_new(...))
   if (is.ursa(obj))
      return(obj)
   if (.is.ursa_stack(obj))
      return(ursa_brick(obj)) ## 20170122 removed 'return(obj)'
   if (is.data.frame(obj))
      return(allocate(obj,...))
   if (inherits(obj,c("SpatialGridDataFrame"))) {
      requireNamespace("sp")
     # str(obj)
      cs <- sp::getGridTopology(obj)@cellsize
      bb <- c(bbox(obj))
      pr <- sp::proj4string(obj)
     # g1 <- regrid(bbox=bb,res=cd,proj=p)
      g1 <- regrid(ursa_grid(),bbox=bb,res=cs,proj=pr)
      session_grid(g1)
      res <- vector("list",ncol(obj@data))
      names(res) <- colnames(obj@data)
      for (i in seq_along(ncol(obj@data)))
         res[[i]] <- ursa_new(obj@data[,i])#,bandname=names(res)[i])
      return(res)
   }
   if (inherits(obj,c("SpatialPointsDataFrame","SpatialPixelsDataFrame"))) {
      return(allocate(obj,...))
   }
   if (inherits(obj,c("RasterBrick","RasterStack","RasterLayer"))) {
      g1 <- .grid.skeleton()
      e <- raster::extent(obj)
      a <- e@ymax
      ##~ g1$minx <- round(e@xmin,5)
      ##~ g1$maxx <- round(e@xmax,5)
      ##~ g1$miny <- round(e@ymin,5)
      ##~ g1$maxy <- round(e@ymax,5)
      g1$minx <- e@xmin
      g1$maxx <- e@xmax
      g1$miny <- e@ymin
      g1$maxy <- e@ymax
      g1$columns <- raster::ncol(obj)
      g1$rows <- raster::nrow(obj)
      g1$proj4 <- raster::projection(obj)
      if (is.na(g1$proj4))
         g1$proj4 <- ""
      g1$resx <- with(g1,(maxx-minx)/columns)
      g1$resy <- with(g1,(maxy-miny)/rows)
      session_grid(g1)
      if (all(is.factor(obj))) {
         cname <- raster::levels(obj)[[1]]$code
         if (is.null(cname))
            cname <- raster::levels(obj)[[1]]$category
         ct <- raster::colortable(obj)
         if (!length(ct))
            ct <- rep(NA,length(cname))
         names(ct) <- cname
         class(ct) <- "ursaColorTable"
      }
      else {
         if (!inherits(obj,"RasterStack"))
            ct <- ursa_colortable(as.character(raster::colortable(obj)))
         else {
            ct <- lapply(obj@layers,function(x) {
               ursa_colortable(as.character(raster::colortable(x)))
            })
         }
      }
      if (inherits(obj,"RasterStack")) {
         res <- vector("list",dim(obj)[3])
         for (i in seq_along(res)) {
            res[[i]] <- ursa_new(value=raster::values(obj[[i]])
                                ,bandname=names(obj[[i]]))
            ursa_colortable(res[[i]]) <- ct[[i]]
         }
      }
      else if (inherits(obj,"RasterBrick")) {
         res <- ursa_new(value=raster::values(obj))
         ursa_colortable(res) <- ct
      }
      else if (inherits(obj,"RasterLayer")) {
         if (length(ct))
            res <- ursa_new(raster::values(obj),colortable=ct)
         else
            res <- ursa_new(raster::values(obj))
      }
      return(res)
   }
   if (is.list(obj)) {
      if ((length(obj$x)==length(obj$z))&&(length(obj$y)==length(obj$z))) 
         return(allocate(obj,...))
      g <- .grid.skeleton()
      g$resx <- mean(diff(obj$x))
      g$resy <- mean(diff(obj$y))
      g$minx <- min(obj$x)-g$resx/2
      g$miny <- min(obj$y)-g$resy/2
      g$maxx <- max(obj$x)+g$resx/2
      g$maxy <- max(obj$y)+g$resy/2
      g$columns <- with(g,(maxx-minx)/resx)
      g$rows <- with(g,(maxy-miny)/resy)
      tolerance <- 1e-11
      if ((!.is.integer(g$columns,tolerance))||(!.is.integer(g$rows,tolerance)))
         stop(paste("Unable to calculate integer dim size."
                   ,"Try to change 'tolerance'",paste0("(",tolerance,")")))
      g$columns <- as.integer(round(g$columns))
      g$rows <- as.integer(round(g$rows))
      p <- attr(obj,"proj")
      if ((is.character(p))&&(nchar(p)))
         g$proj4 <- attr(obj,"proj")
      g2 <- getOption("ursaSessionGrid")
      session_grid(g)
      arglist <- list(...)
      ind <- .grep("^reset(Grid)*",names(arglist))
      if ((length(ind)==1)&&(is.logical(arglist[[ind]]))&&(arglist[[ind]]))
         resetGrid <- TRUE
      else
         resetGrid <- FALSE
     # res <- ursa_new(value=obj$z[,rev(seq(ncol(obj$z)))],bandname="z")
      res <- ursa_new(value=obj$z,bandname="z")
      ursa_grid(res) <- g
      if ((.is.grid(g2))&&(resetGrid))
         session_grid(g2)
      else
         session_grid(g)
      return(res)
   }
   if (inherits(obj,"ggmap")) {
      B <- 6378137*pi
      .epsg3857 <- paste("","+proj=merc +a=6378137 +b=6378137"
                        ,"+lat_ts=0.0 +lon_0=0.0"
                        ,"+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null"
                        ,"+wktext  +no_defs")
      ll <- matrix(as.numeric(attr(obj,"bb")),ncol=2,byrow=TRUE)
      bbox <- .project(list(x=ll[,2],y=ll[,1]),.epsg3857)
     # bbox <- c(bbox$x[1],bbox$y[1],bbox$x[2],bbox$y[2])
      if (is.list(bbox))
         bbox <- with(bbox,c(x[1],y[1],x[2],y[2]))
      else
         bbox <- c(bbox)[c(1,3,2,4)]
      g1 <- .grid.skeleton()
      g1$columns <- dim(obj)[2]
      g1$rows <- dim(obj)[1]
     # str(obj)
      if (ll[2,2]>180)
         bbox[3] <-bbox[3]+2*B
      if (ll[1,2]<(-180))
         bbox[1] <-bbox[1]-2*B
     # print(bbox)
      if (bbox[1]>bbox[3]) {
         if ((ll[1,2]<0)&&(bbox[1]>0))
           bbox[1] <- bbox[1]-2*B
         if ((ll[2,2]>0)&&(bbox[3]<0))
           bbox[3] <- bbox[3]+2*B
        # else
        #    print(bbox)
      }
      g1$minx <- bbox[1]
      g1$miny <- bbox[2]
      g1$maxx <- bbox[3]
      g1$maxy <- bbox[4]
     # print(ll)
      if (FALSE) {
         if (g1$maxx<g1$minx) {
            if ((g1$minx>0)&&(g1$maxx<0)) {
               if ((ll[1,2]<(-180))&&(ll[1,2]<(-180)))
                  g1$minx <- g1$minx-2*B
               g1$maxx <- g1$maxx+2*B
            }
            else {
               print(g1)
               stop("AS:1")
               g1$maxx <- 2*B+g1$maxx
            }
         }
      }
      g1$proj4 <- .epsg3857
      g1$resx <- with(g1,(maxx-minx)/columns)
      g1$resy <- with(g1,(maxy-miny)/rows)
      stopifnot(with(g1,abs((resx-resy)/(resx+resy)))<1e-3)
      session_grid(g1)
      a1 <- factor(obj)
      v1 <- levels(a1)
     # names(v1) <- seq(length(v1))-1L
      class(v1) <- "ursaColorTable"
      a1 <- as.integer(a1)-1
      dim(a1) <- rev(dim(obj))
      res <- ursa_new(value=a1[,rev(seq(dim(a1)[2]))])
      res$colortable <- v1
      class(res$value) <- "ursaCategory"
      return(res)
   }
   if (inherits(obj,"cimg")) { ## require(imager)
      dima <- dim(obj)
      if (dima[3]==1) {
         res <- ursa_new(value=obj[,rev(seq(dima[2])),1,])
         return(res)
      }
      if (dima[4]!=1) {
         res <- ursa_new(value=obj[,rev(seq(dima[2])),,1])
         return(res)
      }
   }
  # isMatrix <- is.matrix
  # if ((is.matrix(obj))||(is.array(obj)))
   isArray <- !is.null(dim)
   if ((!isArray)&&(is.numeric(obj))) {
     # print("NOT-ARRAY")
      res <- ursa_new(obj,...)
      return(res)
   }
   if (is.array(obj)) {
     # print("ARRAY")
      arglist <- list(...)
      bname <- NULL
      if (!is.null(b1 <- attr(obj,"grid"))) {
         g1 <- .grid.skeleton()
         aname <- names(b1)
         indx <- .grep("^(x$|lon)",aname)
         indy <- .grep("^(y$|lat)",aname)
         proj4 <- .grep("proj4",aname)
         aname <- .grep("proj4",aname,value=TRUE,invert=TRUE)
         indz <- which(is.na(match(seq_along(aname),c(indx,indy))))
         if ((length(indx))&&(length(indy))) {
            x <- b1[[indx]]
            y <- b1[[indy]]
            g1$resx <- mean(unique(diff(x)))
            g1$resy <- mean(unique(diff(y)))
            g1$minx <- min(x)-g1$resx/2
            g1$maxx <- max(x)+g1$resx/2
            g1$miny <- min(y)-g1$resy/2
            g1$maxy <- max(y)+g1$resy/2
            g1$columns <- length(x)
            g1$rows <- length(y)
            if ((length(proj4))&&(nchar(b1[[proj4]]))) {
               g1$proj4 <- p
            }
            else if (.lgrep("(lon|lat)",aname)==2)
               g1$proj4 <- " +proj=longlat +datum=WGS84 +no_defs"
            session_grid(g1)
         }
         if (length(indz)==1)
            bname <- as.character(b1[[indz]])
      }
      res <- ursa_new(value=obj,bandname=bname,...)
      return(res)
   }
   if (is.character(obj)) {
      if ((FALSE)&&(envi_exists(obj))) ## FALSE to keep alternative to read ENVI using GDAL 
         return(read_envi(obj,...))
      if (file.exists(obj))
         return(read_gdal(obj,...))
      if (isURL <- .lgrep("^(http://|https://|ftp://|file:///)",obj)>0) {
         arglist <- list(...)
         fname <- tempfile()
        # download.file(obj,fname,...)
         ind <- .grep("(method|mode|cache|extra)",names(arglist))
         args2 <- c(url=obj,destfile=fname,arglist[ind])
         do.call("download.file",args2)
         on.exit(try(file.remove(fname)))
         return(read_gdal(fname,...))
      }
   }
   ursa_new(obj,...)
  # str(obj)
  # print(class(obj))
  # stop("Unsupportes type of source object")
}
