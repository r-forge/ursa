## http://leaflet-extras.github.io/leaflet-providers/preview/index.html
'glance' <- function(...) {
   arglist <- list(...)
   if (!length(arglist)) {
      viewer <- session_pngviewer(TRUE)
      on.exit(session_pngviewer(viewer))
      arglist <- .args2list()
   }
   if (!length(arglist)) {
      return(display())
   }
   if (!is.character(arglist[[1]])) {
      a <- do.call(".glance",arglist)
      return(invisible(a))
   }
   if (!nchar(arglist[[1]])) {
      return(invisible(10L))
   }
  # if (.lgrep("\\.rds$",basename(arglist[[1]]))) {
  #    arglist[[1]] <- readRDS(arglist[[1]])
  #   # return(do.call(".glance",arglist))
  # }
   if (is.character(arglist[[1]])) {
      if (envi_exists(arglist[[1]])) {
         return(do.call("display",arglist))
      }
      else if (.lgrep("\\.(tif|tiff|img|png|bmp|dat)$",arglist[[1]]))
         return(do.call("display",arglist))
      else if (.lgrep("\\.(gpkg|tab|json|geojson|mif|sqlite|shp|shp\\.zip)$"
                     ,arglist[[1]]))
         return(do.call(".glance",arglist))
      else {
         b <- open_gdal(arglist[[1]])
         if (!is.null(b)) {
            close(b)
            do.call("display",arglist)
         }
         else if (.lgrep("\\.rds$",basename(arglist[[1]]))) {
            obj <- readRDS(arglist[[1]])
           # print(class(obj))
           # print(inherits(obj,"Spatial"))
            if ((inherits(obj,"Spatial"))||
                (.lgrep("Spatial(Points|Lines|Polygons)DataFrame",class(obj)))) {
               arglist[[1]] <- quote(obj) ## 'GADM' distributes 'rds'
               arglist$engine <- "sp"
               return(do.call(".glance",arglist))
            }
            if (inherits(obj,"may be GDAL???")) { ## not good idea
               arglist[[1]] <- quote(obj)
               return(do.call("display",arglist))
            }
         }
        # else if (requireNamespace("sf",quietly=.isPackageInUse())) {
        #    do.call(".glance",arglist)
        # }
         else {
           # message("Cannot complete without suggested package 'sf'.")
            do.call(".glance",arglist)
            return(invisible(1L))
         }
      }
   }
   invisible(0L)
}
'.glance' <- function(dsn,layer=".*",grid=NULL,attr=".+",size=NA,expand=1.05
                        ,border=0,lat0=NA,lon0=NA,resetProj=FALSE
                        ##~ ,proj=c("auto","stere","laea","merc","longlat"#,"internal"
                               ##~ ,"google","osm","cycle","transport","mapsurfer"
                               ##~ ,"sputnik")
                        ,style="auto"
                        ,feature=c("auto","attribute","geometry"),alpha=NA
                        ,basemap.order=c("after","before"),basemap.alpha=NA
                        ,engine=c("native","sp","sf")
                        ,geocode=c("nominatim","google"),place=""
                        ,zoom=NA,rasterize=FALSE
                        ,verbose=FALSE,...) {
   a <- as.list(match.call())
  # feature <- "geometry"
   if (TRUE) {
      for (i in seq_along(geocode))
         geocode[i] <- match.arg(geocode[i],c("nominatim","google"))
   }
   else
      geocode <- match.arg(geocode)
   projClass <- c("longlat","stere","laea","merc")
   projPatt <- paste0("(",paste(projClass,collapse="|"),")")
   staticMap <- c("openstreetmap","google","sputnik")
   tilePatt <- paste0("(",paste0(unique(c(staticMap,.untile())),collapse="|"),")")
   basemap.order <- match.arg(basemap.order)
   after <- basemap.order %in% "after"
   before <- basemap.order %in% "before"
   feature <- match.arg(feature)
   engine <- match.arg(engine)
  # print(c(dsn=class(dsn)))
  # obj <- .read_ogr(dsn)
   obj <- .read_ogr(dsn=dsn,engine=engine,layer=layer,attr=attr,geocode=geocode
                   ,place=place,grid=grid,size=size
                   ,expand=expand,border=border
                   ,lat0=lat0,lon0=lon0,resetProj=resetProj,style=style#,zoom=NA
                   ,verbose=verbose)
   if (inherits(obj,"NULL"))
      return(invisible(NULL))
   isSF <- inherits(obj,c("sfc","sf"))
   isSP <- !isSF
   g0 <- attr(obj,"grid")
   toUnloadMethods <- attr(obj,"toUnloadMethods")
   dname <- attr(obj,"colnames")
   style <- attr(obj,"style")
   geocodeStatus <- attr(obj,"geocodeStatus")
   if (!.lgrep(projPatt,style))
      proj <- "auto"
   else
      proj <- .gsub2(projPatt,"\\1",style)
   if (!.lgrep(tilePatt,style))
      art <- "none"
   else {
      art <- .gsub2(tilePatt,"\\1",style)
      proj <- "merc"
   }
   isWeb <- .lgrep(tilePatt,art)
  # proj <- match.arg(proj)
  # attr(obj,"grid") <- NULL
  # attr(obj,"toUnloadMethods") <- NULL
  # attr(obj,"dname") <- NULL
   #attr(obj,"geocodeStatus") <- NULL
   if (isWeb) {
      bbox <- with(g0,c(minx,miny,maxx,maxy))
      lim <- c(.project(matrix(bbox,ncol=2,byrow=TRUE)
                                               ,g0$proj4,inv=TRUE))[c(1,3,2,4)]
      for (i in seq(3)) {
         basemap <- try(.geomap(lim,style=style,size=size,zoom=zoom
                       ,verbose=verbose))
         if (!inherits(basemap,"try-error"))
            break
         message(paste("failed to get map; trying another service"))
         if ((.lgrep("sputnik",style))&&(.lgrep("static",style)))
            style <- .gsub("sputnik","openstreetmap",style)
         else if ((.lgrep("sputnik",style))&&(.lgrep("tile",style)))
            style <- .gsub("sputnik","mapnik",style)
         else if (.lgrep("openstreetmap",style))
            style <- .gsub("openstreetmap","mapnik",style)
         else if (.lgrep("google",style))
            style <- .gsub("google","openstreetmap",style)
         else if (.lgrep("mapsurfer",style))
            style <- .gsub("mapsurfer","mapnik",style)
         else if (.lgrep("mapnik",style))
            style <- .gsub("mapnik","mapsurfer",style)
      }
     # str(basemap);q()
      g0 <- ursa(basemap,"grid")
   }
   else {
      if (border>0)
         g0 <- regrid(g0,border=border)
      basemap <- NULL
   }
   attr(obj,"grid") <- g0
  # str(attr(basemap,"copyright"))
   session_grid(g0)
   if (verbose)
      print(c(sf=isSF,sp=isSP))
   if (isSF) {
      geoType <- unique(as.character(sf::st_geometry_type(obj)))
      obj_geom <- sf::st_geometry(obj)
     # bbox <- c(sf::st_bbox(obj))
     # proj4 <- sf::st_crs(obj)$proj4string
   }
   if (isSP) {
      geoType <- switch(class(sp::geometry(obj))
                       ,SpatialPolygons="POLYGON"
                       ,SpatialPoints="POINT"
                       ,SpatialLines="LINE")
      obj_geom <- switch(geoType,POLYGON=sp::geometry(obj)@polygons
                                   ,LINE=sp::geometry(obj)@lines
                                  ,POINT=sp::geometry(obj))
     # bbox <- c(sp::bbox(obj))
     # names(bbox) <- c("xmin","ymin","xmax","ymax")
     # proj4 <- sp::proj4string(obj)
   }
   toCoast <- !isWeb | isWeb & .getPrm(list(...),name="coast",default=FALSE)
   if ((FALSE)&&(.lgrep("POLYGON",geoType))&&(isSF)) {
      valid <- .try(ov <- sf::st_covers(obj_geom,sparse=!FALSE))
      if (!valid)
         isOverlap <- FALSE
      else 
         isOverlap <- length(which(sapply(ov,function(x) length(x)>1)))>0
   }
   else
      isOverlap <- FALSE
   if (feature=="auto")
      feature <- ifelse(isOverlap,"geometry","attribute")
   if ((!toUnloadMethods)&&(isSP)&&(!"package:methods" %in% search())) {
     # I've read "R-exts 1.1.3.1",
     # but sp::plot() is failed for 'requireNamespace("methods")
     # .require("methods")
      opW <- options(warn=-1)
      warning("Package 'methods' is required for S4 object coercion.")
      options(opW)
      toUnloadMethods <- TRUE
   }
   if ((rasterize)&&(nchar(Sys.which("gdal_rasterize")))) {
      res <- .rasterize(obj,feature=feature,verbose=verbose)
   }
   else if (FALSE) { ## remove this branch after tests
      '.removeShapefile' <- function(bn) {
         file.remove(.dir(path=dirname(bn)
                     ,pattern=paste0(basename(bn),"\\.(cpg|dbf|prj|shp|shx)$")
                     ,full.names=TRUE))
      }
      ftemp <- .maketmp() #tempfile(pattern="") #tempfile(pattern="") ## ".\\tmp1"
      shpname <- paste0(ftemp,".shp")
      .removeShapefile(ftemp)
      if (isSF) {
         sf::st_write(obj_geom,shpname,quiet=TRUE)
      }
      if (isSP) {
         da <- obj@data
         obj@data <- data.frame(id=seq(nrow(da)))
         rgdal::writeOGR(obj,dirname(ftemp),basename(ftemp),driver="ESRI Shapefile")
         obj@data <- da
         rm(da)
      }
      if (feature=="attribute") {
         cmd <- with(g0,paste("gdal_rasterize","-a id"
                             ,"-tr",resx,resy
                             ,"-te",minx,miny,maxx,maxy
                             ,"-of ENVI -ot Int16",ifelse(verbose,"","-q")
                             ,shpname,ftemp))
         if (verbose)
            message(cmd)
         system(cmd)
         va <- read_envi(ftemp)
         .removeShapefile(ftemp)
         envi_remove(ftemp)
         va[va==0] <- NA
        # res <- vector("list",length(dname))
         res <- lapply(dname,function(x){
            if (isSF)
               y <- reclass(va,src=seq(nrow(obj)),dst=obj[[x]])
            if (isSP)
               y <- reclass(va,src=seq(nrow(obj)),dst=obj[[x]])
            names(y) <- x
            y
         })
      }
      else if (feature=="geometry") {
         nr <- ifelse(isSF,nrow(obj),nrow(obj))
         res <- lapply(seq(nr),function(i){
            cmd <- with(g0,paste("gdal_rasterize","-a id"
                                ,"-where",paste0(.dQuote("id"),"=",i)
                                ,"-tr",resx,resy
                                ,"-te",minx,miny,maxx,maxy
                                ,"-of ENVI -ot Int16",ifelse(verbose,"","-q")
                                ,shpname,ftemp))
            if (verbose)
               message(cmd)
            system(cmd)
            va <- read_envi(ftemp)
            envi_remove(ftemp)
            va[va==0] <- NA
            va
         })
         .removeShapefile(ftemp)
      }
     # if (isSF)
     #    isSF <- FALSE
     # if (isSP)
     #    isSP <- FALSE
   }
   ##~ print(session_grid())
   if (isSF)
      geoType <- unique(as.character(sf::st_geometry_type(obj)))
   if (isSP) {
     # geoType <- geoType ## no change
   }
  # dname <- names(attr(obj,"relation_to_geometry"))
  # ct <- colorize(obj[,dname[4],drop=TRUE][,1],alpha=1)
  # print(ct)
  # print(unname(ct$colortable[ct$ind]))
  # require(methods)
   if (geocodeStatus) { ## move to 'visualization' block
      if (style=="auto") {
         if (geocode=="google")
            style <- "google static color"
         else if (geocode=="nominatim")
            style <- "openstreetmap static color"
         else
            style <- "google static color"
      }
      basemap.alpha <- 1
      alpha <- 0
   }
   if ((isWeb)&&(is.na(basemap.alpha)))
      basemap.alpha <- ifelse(before,0.5,0.35)
   if (is.na(alpha))
      alpha <- ifelse(isWeb,ifelse(before,0.75,1),1)
   if (feature=="attribute") {
      ct <- vector("list",length(dname))
      cpg <- "1251"
      for (i in seq_along(dname)) {
        # print(i)
        # print(dname[i])
        # str(obj[,dname[i]][,1])
         if (isSF)
            val <- obj[,dname[i],drop=TRUE][,1]
         if (isSP)
            val <- obj@data[,dname[i],drop=TRUE]
         if ((is.character(cpg))&&(is.character(val)))
            val <- iconv(val,"UTF-8","1251")
        # print(all(is.na(val)))
        # print(obj[,dname[i],drop=TRUE][,1])
         ct[[i]] <- colorize(val,alpha=alpha,...)
        # print(names(ct[[i]]$colortable),quote=FALSE)
        # print(ct[[i]])
        # print("-----------------------------------------")
      }
      if (!rasterize) {
         if (length(dname))
            res <- lapply(rep(NA,length(ct)),ursa_new)
         else
            res <- list(geometry=ursa_new())
      }
      if (isWeb) {
         compose_open(res,scale=1,...)
      }
      else
         compose_open(res,...)
      gline <- compose_graticule(...)
      if (toCoast)
         cline <- compose_coastline(...)
      pb <- ursaProgressBar(min=0,max=length(res))
      for (i in seq_along(res)) {
         if (isWeb)
            panel_new(fill="transparent",...)
         else
            panel_new(...) #fill=ifelse(isWeb,"transparent","chessboard"))
         if (before) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
        # if ((!length(ct))||(all(is.na(ct[[i]]$index)))) {
         if (!length(ct)) {
            if (isSF)
               panel_plot(obj_geom)
            if (isSP)
               panel_plot(obj,add=TRUE)
         }
         else if (rasterize){
            ct[[i]] <- panel_raster(colorize(res[[i]]),alpha=alpha)
           # ct[[i]] <- panel_raster(res[[i]],alpha=alpha,verbose=TRUE)
         }
         else {
            col <- unname(ct[[i]]$colortable[ct[[i]]$ind])
            ind <- which(is.na(col))
            bg.line <- rep("#0000007F",length(col))
            bg.point <- rep("#000000FF",length(col))
            bg.polygon <- rep("#000000FF",length(col))
            if (length(ind)) {
               col[ind] <- "#FFFFFF00"
               bg.line[ind] <- "#0000000F"
               bg.point[ind] <- "#0000002F"
               bg.polygon[ind] <- "#0000002F"
            }
            if (!isSP) {
               if (.lgrep("polygon",geoType)) {
                  panel_plot(obj_geom,col=col,border=bg.polygon,lwd=0.1,lty="blank")
                  panel_plot(obj_geom,col="transparent",border=bg.polygon,lwd=0.1)
               }
               if (.lgrep("point",geoType)) {
                  panel_plot(obj_geom
                            ,col=bg.point,bg=col,pch=21,lwd=0.25,cex=1)
               }
               if (.lgrep("line",geoType)) {
                  panel_plot(obj_geom,lwd=3,col=bg.line)
                  panel_plot(obj_geom,lwd=2,col=col)
               }
            }
            else {
               if (.lgrep("polygon",geoType)) {
                  panel_plot(obj,col=col,border=bg.polygon,lwd=0.1,lty="blank"
                            ,add=TRUE)
                  panel_plot(obj,col="transparent",border=bg.polygon,lwd=0.1
                            ,add=TRUE)
               }
               if (.lgrep("point",geoType)) {
                  panel_plot(obj,col=bg.point,bg=col,pch=21,lwd=0.25,cex=1
                            ,add=TRUE)
               }
               if (.lgrep("line",geoType)) {
                  panel_plot(obj,lwd=3,col=bg.line,add=TRUE)
                  panel_plot(obj,lwd=2,col=col,add=TRUE)
               }
            }
         }
         if (after) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
         if (toCoast)
            panel_coastline(cline)
         if (geocodeStatus)
            panel_graticule(gline,margin=c(T,T,F,F))
         else
            panel_graticule(gline)
         if (proj %in% c("merc")) {
            if ((g0$miny<0)&&(g0$maxy>0)) {
               if ((-g0$miny)>(g0$maxy))
                  y <- c(g0$miny,0)
               else
                  y <- c(0,g0$maxy)
            }
            else
               y <- c(g0$miny,g0$maxy)
            sc <- 1/cos(.project(cbind((g0$minx+g0$maxx)/2,y)
                                ,g0$proj4,inv=TRUE)[,2]*pi/180)
           # x <- 0#ifelse(((isWeb)&&(isStatic)&&(isGoogle)),0.5,0)
           # print(art)
            x <- ifelse(art %in% "google",0.5,0)
            if (max(sc)/min(sc)>1.2) {
               y <- (y-g0$miny)/(g0$maxy-g0$miny)
               panel_scalebar(pos=c(x,min(y)),...)
               panel_scalebar(pos=c(x,max(y)),...)
            }
            else # if (isWeb)
               panel_scalebar(pos=c(x,0),...)
           # else
           #    panel_scalebar(pos="bottomleft",...)
         }
         else
            panel_scalebar(pos="bottomleft",...)
         setUrsaProgressBar(pb)
      }
      close(pb)
      if (length(ct)) {
         if (!rasterize) {
            ct <- lapply(ct,function(x) {
               y <- ursa(x,"colortable") ## y <- x$colortable
               if ((TRUE)&&(isWeb)&&(after)) {
                  alpha2 <- 0.65
                  y[] <- paste0(substr(y,1,7),toupper(as.hexmode(round(alpha2*255))))
               }
               y
            })
         }
         names(ct) <- dname
         compose_legend(ct,las=2,trim=2L)
      }
      compose_close(...)#res)
   }
   else if (feature=="geometry") {
     # print(geoType)
      if (isSF)
         n <- length(obj_geom)
      if (isSP)
         n <- length(obj_geom)
      if (isSF) {
        # da <- obj[,dname,drop=TRUE][,1] ## wrong
         da <- obj[,dname,drop=TRUE]#[,dname,drop=TRUE]
         names(da) <- dname
      }
      if (isSP)
         da <- obj@data[,dname,drop=FALSE]
      da <- rbind(format(da),paste0(names(da),":"))
     # print(format(da))
     # print(da)
     # e <- format(t(da),justify="right")
      e <- .gsub("^\\s+","",t(da))
     # e1 <- paste(apply(e[,c(n+1,1)],1,paste,collapse=" "),collapse="\n")
     # print(e)
     # message(e1)
     # q()
      if (!rasterize)
         res <- ursa_new(nband=n)
      ct <- lapply(seq(n),function(i) colorize(0L))
      compose_open(res,legend=NULL,...)
      gline <- compose_graticule(...)
      if (toCoast)
         cline <- compose_coastline(...)
      pb <- ursaProgressBar(min=0,max=length(res))
     # geoType <- "skip"
      for (i in seq_along(res)) {
         if (isWeb)
            panel_new(fill="transparent",...)
         else
            panel_new(...)
         if (before) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
         if (!rasterize) {
           # panel_plot(obj_geom[[i]])
            if (!isSP) {
               if (.lgrep("polygon",geoType)) {
                  panel_plot(obj_geom[[i]]
                            ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]),lwd=0.1)
               }
               if (.lgrep("point",geoType)) {
                  panel_plot(obj_geom[[i]]
                            ,col="black",bg=unname(ct[[i]]$colortable[ct[[i]]$ind])
                            ,pch=21,lwd=0.25,cex=1)
               }
               if (.lgrep("line",geoType)) {
                  panel_plot(obj_geom[[i]],lwd=3,col="#0000007F")
                  panel_plot(obj_geom[[i]],lwd=2
                            ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]))
               }
            }
            else {
               if (.lgrep("polygon",geoType)) {
                  panel_plot(obj[i,],col=unname(ct[[i]]$colortable[ct[[i]]$ind])
                            ,lwd=0.1,add=TRUE)
               }
               if (.lgrep("point",geoType)) {
                  panel_plot(obj[i,]
                            ,col="black",bg=unname(ct[[i]]$colortable[ct[[i]]$ind])
                            ,pch=21,lwd=0.25,cex=1,add=TRUE)
               }
               if (.lgrep("line",geoType)) {
                  panel_plot(obj[i,],lwd=3,col="#0000007F",add=TRUE)
                  panel_plot(obj[i,],lwd=2
                            ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]),add=TRUE)
               }
            }
         }
         else
            panel_raster(res[i])
         if (after) {
            panel_plot(basemap,alpha=basemap.alpha)
         }
         if (toCoast)
            panel_coastline(cline)
         panel_graticule(gline)
         panel_scalebar(pos=ifelse(isWeb,"bottomleft","bottomleft"),...)
         e1 <- paste(apply(e[,c(n+1,i),drop=FALSE],1,paste,collapse=" ")
                    ,collapse="\n")
         panel_annotation(text=e1,adj=0,...) # pos="topleft")
         setUrsaProgressBar(pb)
      }
      close(pb)
      compose_close(...)
     # str(n)
   }
   if ((toUnloadMethods)&&("package:methods" %in% search())) {
     # print(search())
      detach("package:methods",unload=FALSE) 
     # but namespace "methods" is not unloaded, because namespace "sp" is loaded
     # 'as' is not found now
   }
   invisible(0L)
}
'.cmd.glance' <- function() {
   a <- .args2list()
   do.call(".glance",a)
   NULL
}
