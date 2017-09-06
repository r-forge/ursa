'.spatialize' <- function(dsn,engine=c("native","sp","sf"),layer=".*",attr=".+"
                       ,geocode="",place=""
                       ,grid=NULL,size=NA,cell=NA,expand=1,border=NA
                       ,lat0=NA,lon0=NA,resetProj=FALSE,style="auto"#,zoom=NA
                       ,subset="",verbose=FALSE,...) {
   engine <- match.arg(engine)
  # print(c(expand=expand,border=border))
  # geocode <- match.arg(geocode)
   geocodeStatus <- FALSE
   hasOpened <- FALSE
   toUnloadMethods <- FALSE
   if (is.na(resetProj))
      resetProj <- TRUE
   cpg <- NULL
   if (engine=="sp") {
      isSF <- FALSE
      isSP <- TRUE
   }
   else if (engine=="sf") {
      isSF <- requireNamespace("sf",quietly=.isPackageInUse())
      isSP <- !isSF
   }
   else {
      isSF <- NA
      isSP <- NA
   }
   isEPSG <- FALSE
   if (length(style)) {
      if (is.numeric(style))
         isEPSG <- TRUE
      else if ((is.character(style))&&(!nchar(.gsub("\\d","",style))))
         isEPSG <- TRUE
   }
   if ((isEPSG)&&(is.numeric(style)))
      style <- as.character(style)
   isNative <- engine=="native"
   if (!((is.character(dsn))&&(length(dsn)==1))) {
      nextCheck <- TRUE
      spcl <- paste0("Spatial",c("Points","Lines","Polygons"))
      spcl <- c(spcl,paste0(spcl,"DataFrame"))
      if ((nextCheck)&&(inherits(dsn,spcl))) {
         if ((!toUnloadMethods)&&(!("package:methods" %in% search()))) {
            if (FALSE) {
              # .require("methods")
               opW <- options(warn=1)
               warning("Package 'methods' is required for S4 object coercion.")
               options(opW)
            }
            toUnloadMethods <- TRUE
         }
         if ((!isNative)&&(isSF))
            obj <- sf::st_as_sf(dsn)
         else {
            isSP <- TRUE
            isSF <- !isSP
            obj <- dsn
         }
         rm(dsn)
         nextCheck <- FALSE
      }
      if ((nextCheck)&&(inherits(dsn,"sf"))) {
         if (isNative) {
            isSF <- TRUE
            isSP <- !isSF
         }
         obj <- dsn
         rm(dsn)
         nextCheck <- FALSE
      }
      if ((nextCheck)&&(is.array(dsn))) {
         return(display(dsn,...))
      }
      if ((nextCheck)&&(is.ursa(dsn))) {
         return(display(dsn,...))
      }
      if ((nextCheck)&&((isSF)||(isNative))) {
         if (isNative) {
            isSF <- requireNamespace("sf",quietly=.isPackageInUse())
            isSP <- !isSF
         }
        # nextCheck <- isSP
      }
      if ((nextCheck)&&(isSF)) {
         if (is.array(dsn)) {
            message("process 'array' by 'sf' -- TODO (dilemma: raster is array)")
         }
         else if (is.numeric(dsn)) {
            if (length(dsn)==2) { ## point
              # obj <- sf::st_sfc(sf::st_point(dsn))
               obj <- sf::st_as_sf(data.frame(lon=dsn[1],lat=dsn[2])
                                  ,coords=c("lon","lat"),crs=4326)
            }
            else if (length(dsn)==4) { ## boundary
               if (dsn[1]>dsn[3])
                  dsn[1] <- dsn[1]-360
               obj <- matrix(dsn[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
               if (TRUE) {
                  x <- obj[,1]
                  y <- obj[,2]
                  n <- 256
                  x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                        ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
                  y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                        ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
                  obj <- cbind(x,y)
               }
              # colnames(obj) <- c("lon","lat")
               ##~ obj <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(obj)))
                               ##~ ,coords=c("lon","lat"),crs=4326)
               obj <- sf::st_sfc(sf::st_multilinestring(list(obj)),crs=4326)
              # obj <- sf::st_sf(obj)
            }
            else
               obj <- NULL
            if (!is.null(obj)) {
               sf::st_crs(obj) <- 4326
            }
            else
               rm(obj)
         }
         else if (inherits(dsn,c("sfc","sf"))) {
            obj <- dsn
            dsn <- class(obj)
         }
         else {
            obj <- try(sf::st_as_sfc(dsn))
            if (inherits(obj,"try-error")) {
               message(paste("(#32) unable to process object of class"
                            ,.sQuote(paste(class(dsn),collapse=" | "))))
               return(NULL) ## 32L
            }
         }
      }
      else if ((nextCheck)&&(isSP)) { # if (isSP)
         if (is.array(dsn))
            message("process 'array' by 'sp' -- TODO (dilemma: raster is array)")
         else if (is.numeric(dsn)) {
            if (length(dsn)==2) { ## point
               obj <- data.frame(lon=dsn[1],lat=dsn[2])
               sp::coordinates(obj) <- ~lon+lat
               sp::proj4string(obj) <- sp::CRS("+init=epsg:4326")
            }
            else if (length(dsn)==4) { ## boundary
               if (dsn[1]>dsn[3])
                  dsn[1] <- dsn[1]-360
               obj <- matrix(dsn[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
               if (TRUE) {
                  x <- obj[,1]
                  y <- obj[,2]
                  n <- 256
                  x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                        ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
                  y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                        ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
                  obj <- cbind(x,y)
               }
               obj <- sp::SpatialLines(list(sp::Lines(sp::Line(obj),1L))
                                      ,proj4string=sp::CRS("+init=epsg:4326"))
            }
           # else
           #    obj <- NULL
         }
         else
            obj <- try(methods::as(dsn,"Spatial"))
         if (inherits(obj,"try-error")) {
            message(paste("(#33) unable to process object of class"
                   ,.sQuote(class(dsn))))
            return(NULL) ## 33L
         }
         else {
            isSP <- TRUE
            isSF <- !isSP
         }
      }
   }
   else {
      if (engine=="native") {
         isSF <- requireNamespace("sf",quietly=.isPackageInUse())
         isSP <- !isSF
      }
      if (!file.exists(dsn)) {
         aname <- paste0(dsn,".zip")
         if (isZip <- file.exists(aname)) {
            ziplist <- unzip(aname,exdir=tempdir());on.exit(file.remove(ziplist))
            dsn <- .grep("\\.shp$",ziplist,value=TRUE)
         }
         else {
            geocodeList <- eval(as.list(args(.geocode))$service)
            if ((length(geocode)==1)&&(!nchar(geocode)))
               geocode <- geocodeList
            da <- .geocode(dsn,service=geocode[1],place=place
                          ,area="bounding",select="top",verbose=verbose)
            if ((is.null(da))&&(length(geocode)==2)) {
               geocode <- geocode[2]
               da <- .geocode(dsn,service=geocode,area="bounding",select="top"
                             ,verbose=verbose)
            }
            else if (length(geocode)==2)
               geocode <- geocode[1]
            if (is.null(da)) {
               cat(paste("unable to geocode request",dQuote(dsn),"\n"))
               return(NULL)
            }
            if (is.null(dim(da)))
               da <- da[c("minx","miny","maxx","maxy")]
            else
               da <- da[,c("minx","miny","maxx","maxy")]
            da <- matrix(da[c(1,2,1,4,3,4,3,2,1,2)],ncol=2,byrow=TRUE)
            if (TRUE) {
               x <- da[,1]
               y <- da[,2]
               n <- 256
               x <- c(seq(x[1],x[2],len=n),seq(x[2],x[3],len=n)
                     ,seq(x[3],x[4],len=n),seq(x[4],x[5],len=n))
               y <- c(seq(y[1],y[2],len=n),seq(y[2],y[3],len=n)
                     ,seq(y[3],y[4],len=n),seq(y[4],y[5],len=n))
               da <- cbind(x,y)
            }
            if (isSF) {
               obj <- sf::st_sfc(sf::st_multilinestring(list(da)),crs=4326)
            }
            if (isSP) {
               obj <- sp::SpatialLines(list(sp::Lines(sp::Line(da),1L))
                                      ,proj4string=sp::CRS("+init=epsg:4326"))
            }
            geocodeStatus <- TRUE
            hasOpened <- TRUE
         }
      }
      else {
         if (isZip <- .lgrep("\\.zip$",dsn)>0) {
            ziplist <- unzip(dsn,exdir=tempdir());on.exit(file.remove(ziplist))
            dsn <- .grep("\\.(shp|sqlite|gpkg|geojson)$",ziplist,value=TRUE)
         }
         else if ((nchar(Sys.which("gzip")))&&(isZip <- .lgrep("\\.gz$",dsn)>0)) {
            dsn0 <- dsn
            dsn <- tempfile();on.exit(file.remove(dsn))
            system2("gzip",c("-f -d -c",dsn0),stdout=dsn)
         }
         if (isCDF <- .lgrep("\\.(nc|hdf)$",dsn)>0) {
            obj <- .read_ncdf(dsn,".+")
            if (!inherits(obj,"data.frame"))
               obj <- as.data.frame(as.ursa(obj[sapply(obj,is.ursa)]))
            else {
               indX <- .grep("^(lon|x$|west|east)",colnames(obj))
               indY <- .grep("^(lat|y$|south|north)",colnames(obj))
               if (!length(indX))
                  indX <- 1L
               if (!length(indY))
                  indY <- 2L
               colnames(obj)[c(indX,indY)] <- c("x","y")
            }
            p4s <- attr(obj,"proj4")
            if (isSF) {
               if (!is.null(p4s))
                  obj <- sf::st_as_sf(obj,coords=c("x","y"),crs=attr(obj,"proj4"))
               else
                  obj <- sf::st_as_sf(obj,coords=c("x","y"))
            }
            if (isSP) {
               sp::coordinates(obj) <- ~x+y
               if (!is.null(p4s))
                  sp::proj4string(obj) <- sp::CRS(p4s)
            }
            hasOpened <- TRUE
           # display(a)
         }
      }
      if ((!hasOpened)&&((!geocodeStatus)||(file.exists(dsn)))) {
         opW <- options(warn=0)
         if (isSF)
            lname <- try(sf::st_layers(dsn)$name)
         else {
            lname <- try(rgdal::ogrListLayers(dsn))
         }
         if (inherits(lname,"try-error")) {
            cat("Cannot get layers\n")
            return(NULL)
         }
         if (!is.character(layer))
            layer <- lname[layer[1]]
         else
            layer <- .grep(layer,lname,value=TRUE)
         if (length(layer)>1) {
            print(paste("Select only one layer:",paste(paste0(seq(layer),")")
                                       ,.sQuote(layer),collapse=", ")),quote=FALSE)
            return(NULL)
         }
         if (isSF) {
            obj <- sf::st_read(dsn,layer=layer,quiet=TRUE)
         }
         else {
            obj <- rgdal::readOGR(dsn,layer,verbose=FALSE)
         }
         options(opW)
      }
      if (.lgrep("\\.shp$",dsn)) {
         cpgname <- .gsub("\\.shp$",".cpg",dsn)
         if (file.exists(cpgname)) {
            cpg <- readLines(cpgname,warn=FALSE)
            if (cpg=="UTF-8")
               cpg <- NULL
         }
      }
   }
   if (verbose)
      print(c('engine.sf'=isSF,'engine.sp'=isSP))
   if (nchar(subset)) {
      obj <- do.call("subset",list(obj,parse(text=subset)))
   }
   if ((geocodeStatus)&&(style=="auto")) {
      style <- switch(geocode,nominatim="openstreetmap color"
                             ,google="google terrain color")
   }
   if ((toUnloadMethods)&&("package:methods" %in% search())) {
     # print(search())
      detach("package:methods",unload=FALSE) 
     # but namespace "methods" is not unloaded, because namespace "sp" is loaded
     # 'as' is not found now
   }
   if (isSF) {
      dname <- try(names(sf::st_agr(obj)),silent=TRUE)
      if (inherits(dname,"try-error"))
         dname <- character()
   }
   else if (isSP) {
      dname <- try(colnames(obj@data),silent=TRUE)
      if (inherits(dname,"try-error"))
         dname <- character()
   }
   hasTable <- length(dname)>0
   dname <- .grep(attr,dname,value=TRUE)
  # str(dname);q()
   if ((hasTable)&&(!length(dname))) {
      message("unable to get attributes by name")
     # str(asf)
     # return(invisible(20L))
   }
   if ((TRUE)&&(length(dname))) {
      if (isSF)
         obj <- obj[,dname]
      if (isSP) {
         obj <- obj[,dname]
      }
   }
   if (hasTable) {
     # lc <- Sys.getlocale("LC_CTYPE")
     # Sys.setlocale("LC_CTYPE","Russian")
      for (i in seq_along(dname)) {
         if (isSF) {
            da <- obj[,dname[i],drop=TRUE] ## sf>=0.5
           # da <- obj[,dname[i],drop=TRUE][,,drop=TRUE] ## sf>=0.4
           # str(da)
         }
         if (isSP) {
            da <- obj@data[,dname[i]]
         }
         if (is.character(da)) {
            Encoding(da) <- "UTF-8"
           ## if inherits(da,"POSIXlt") then 'da' is a list with 9 items
            if (isSF)
               obj[,dname[i]] <- da
            if (isSP)
               obj@data[,dname[i]] <- da
         }
      }
     # Sys.setlocale("LC_CTYPE",lc)
     # str(asf)
   }
   if (isSF) {
      if (TRUE) { ## not tested for multiple geometries POLYGON/MULTIPOLYGON
         if (inherits(obj,"sfc"))
            geoType <- .grep("^sfc_.+$",class(obj),value=TRUE)
         else
            geoType <- .grep("^sfc_.+$",class(obj[[attr(obj,"sf_column")]]),value=TRUE)
         geoType <- .gsub("^sfc_","",geoType)
         if (geoType=="GEOMETRY")
            geoType <- unique(as.character(sf::st_geometry_type(obj)))
      }
      else { ## low perfomance for long geometry
         geoType <- unique(as.character(sf::st_geometry_type(obj)))
      }
   }
   if (isSP)
      geoType <- switch(class(sp::geometry(obj))
                       ,SpatialPolygons="POLYGON"
                       ,SpatialPoints="POINT"
                       ,SpatialLines="LINE")
   if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType)) {
      if (isSF)
         obj <- sf::st_cast(obj,"MULTIPOLYGON")
      if (isSP) {
         stop("POLYGON to MULTIPOLYGON for 'Spatial' is not implemented")
      }
   }
   projClass <- c("longlat","stere","laea","merc")
   projPatt <- paste0("(",paste(projClass,collapse="|"),")")
   staticMap <- c("openstreetmap","google","sputnikmap")
   tilePatt <- paste0("(",paste0(unique(c(staticMap,.tileService())),collapse="|"),")")
   len <- 640L
   if (is.na(size[1]))
      size <- c(len,len)
   else if (is.character(size)) {
      size <- as.integer(unlist(strsplit(
                   .gsub("(\\d+)\\D+(\\d+)","\\1 \\2",size),split="\\s")))
   }
   else if (is.numeric(size))
      size <- rep(size,length=2)
   if (is.numeric(size))
      len <- as.integer(round(max(size)))
   g2 <- getOption("ursaSessionGrid")
   if (is.na(border)) {
      if ((!is.null(g2))||(!is.null(grid)))
         border <- 0L
      else
         border <- 27L
   }
   if (!.lgrep("auto",style))
      resetProj <- TRUE
   if (isEPSG)
      resetProj <- FALSE
  # if ((proj=="internal")&&(!is.na(keepProj))) {
  #    g2 <- NULL
  # }
   if (resetProj)
      g0 <- NULL
   else if ((is.null(grid))&&(!is.null(g2)))
      g0 <- g2
   else if (is.null(grid))
      g0 <- NULL
   else
      g0 <- grid
  # style <- "merc"
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
   isStatic <- .lgrep("static",style)>0
   mlen <- switch(art,google=640,openstreetmap=960,sputnikmap=640)
   if (isStatic) {
      len[len>mlen] <- mlen
   }
  # canTile <- .lgrep(art,eval(as.list(args(".tileService"))$server))>0
   canTile <- .lgrep(art,.tileService())>0
   isTile <- .lgrep("tile",style)>0 & canTile
   if ((!isStatic)&&(!isTile)) {
      if (art %in% staticMap)
         isStatic <- TRUE
      else if (canTile)
         isTile <- TRUE
      else
         art <- "none"
   }
   tpat <- unlist(gregexpr("\\{[xyz]\\}",style))
   tpat <- length(tpat[tpat>0])
   toZoom <- (isTile)||(isStatic)||(style=="web")||(tpat==3)
  # isColor <- .lgrep("colo(u)*r",style)>0
   isWeb <- .lgrep(tilePatt,art)
   if (verbose)
      print(data.frame(proj=proj,art=art,static=isStatic
                      ,canTile=canTile,tile=isTile,web=isWeb))
  # isOSM <- proj %in% "osm"
  # isGoogle <- proj %in% "google"
  # http://static-api.maps.sputnik.ru/v1/?width=400&height=400&z=6&clng=179&clat=70
  #                                &apikey=5032f91e8da6431d8605-f9c0c9a00357
  # isWeb <- isOSM | isGoogle | tryTile
   if ((is.null(g0))||(is.numeric(lon0))||(is.numeric(lat0))) {
  # if ((resetProj)||(is.ursa(g0,"grid"))||(is.numeric(lon0))||(is.numeric(lat0))) {
      if (isSF)
         proj4 <- sf::st_crs(obj)$proj4string
      if (isSP)
         proj4 <- sp::proj4string(obj)
      if ((proj4=="")&&(!(proj %in% c("auto","internal")))) {
         resetProj <- TRUE
         proj4 <- "auto"
      }
      isLonLat <- .lgrep("(\\+proj=longlat|epsg:4326)",proj4)>0
      if ((proj %in% c("auto"))&&(isLonLat)&&(!isEPSG)) { ## added 2016-08-09
         resetProj <- TRUE
         proj4 <- "auto"
      }
      isMerc <- .lgrep("\\+proj=merc",proj4)>0
      if (isMerc) {
         major <- .gsub(".+\\+a=(\\S+)\\s.+","\\1",proj4) ## 20037508
         if (identical(major,proj4)) {
            if (.lgrep("\\+(datum|ellps)=WGS84",proj4))
               B <- 20037508
            else ## yandex?
               B <- 20037508
           # print(B)
         }
         else
            B <- as.numeric(major)*pi
      }
      if (is.numeric(lon0) | is.numeric(lat0) | resetProj) {
         if (isSF) {
            asf2 <- sf::st_transform(obj,4326)
            asf_geom2 <- sf::st_geometry(asf2)
            xy <- lapply(asf_geom2,function(z) {
               if (!is.list(z)) {
                  if (is.null(dim(z))) {
                     d <- ifelse(inherits(z,"XYZ"),3,2)
                     dim(z) <- c(length(z)/d,d)
                  }
                  z <- list(z)
               }
               xy2 <- lapply(z,function(z2) {
                  if (!is.list(z2))
                     z2 <- list(z2)
                  unlist(t(z2[[1]])[1:2,])
               })
            })
            rm(asf2,asf_geom2)
         }
         if (isSP) {
            asp2 <- sp::spTransform(obj,"+init=epsg:4326")
            if (geoType=="POINT") {
               xy <- sp::coordinates(asp2)
            }
            else {
               asp2_geom <- switch(geoType,POLYGON=sp::geometry(asp2)@polygons
                                            ,LINE=sp::geometry(asp2)@lines
                                           ,POINT=sp::geometry(asp2))
               xy <- lapply(asp2_geom,function(z) {
                  gz <- switch(geoType
                              ,POLYGON=z@Polygons,LINE=z@Lines,POINT=z@Points)
                  lapply(gz,function(z3) t(sp::coordinates(z3)))
               })
               rm(asp2,asp2_geom)
            }
         }
         if (is.list(xy))
            xy <- matrix(c(unlist(xy)),ncol=2,byrow=TRUE)
         if (verbose)
            print(summary(xy))
         lon2 <- xy[,1]
         lat2 <- xy[,2]
         if (nrow(xy)>1) {
           # x <- cos(lon2*pi/180)
           # y <- sin(lon2*pi/180)
           # x <- mean(x)
           # y <- mean(y)
           # print(c(x=mean(x),y=mean(y)))
           # theta <- rep(0,length(x))
           # ind <- which(x!=0)
           # theta[-ind] <- pi/2*sign(y[-ind])
           # theta[ind] <- atan(y[ind]/x[ind])
           # ind <- which(x<0)
           # theta[ind] <- pi+theta[ind]
           # theta <- theta-pi
           # theta <- theta*180/pi
           # theta <- mean(theta)
           # lon2 <- range(lon2)
           # lon2 <- theta*pi/180
           # if (theta>180)
           #    theta <- theta-360
           # else if (theta<=(-180))
           #    theta <- theta+360
            lon3 <- lon2
            lon4 <- lon2
            ind3 <- which(lon3<0)
            ind4 <- which(lon4>180)
            lon3[ind3] <- lon3[ind3]+360
            lon4[ind4] <- lon4[ind4]-360
            lon5 <- lon2+360
            sd2 <- sd(lon2)
            sd3 <- sd(lon3)
            sd4 <- sd(lon4)
            sd5 <- sd(lon5)
            dr2 <- diff(range(lon2))
            dr3 <- diff(range(lon3))
            dr4 <- diff(range(lon4))
            dr5 <- diff(range(lon5))
            dr0 <- min(c(dr2,dr3,dr4,dr5))
            if (verbose)
               print(data.frame(r2=diff(range(lon2))
                               ,r3=diff(range(lon3))
                               ,r4=diff(range(lon4))
                               ,r5=diff(range(lon5))))
            if (verbose)
               print(data.frame(sd2=sd2,'sd3R'=sd3,'sd4L'=sd4,'sd5C'=sd5
                               ,n3=length(ind3),n4=length(ind4)))
            if ((sd3<=sd2)&&(sd3<=sd4)&&(dr3==dr0)) {
              # if (length(ind3))
              #    selection <- 3L
               lon2 <- lon3
            }
            else if ((sd4<=sd2)&&(sd4<=sd3)&&(dr4==dr0)) {
              # if (length(ind4))
              #    selection <- 4L
               lon2 <- lon4
            }
         }
        # if ((any(lon2<180))&&(any(lon2>180)))
        #    selection <- 3L
         if (verbose)
            print(summary(lon2))
        # selection <- 0L
         if ((FALSE)&&(mean(lon2)>0))
            lon2[lon2<0] <- lon2[lon2<0]+360
         bbox <- c(range(lon2),range(lat2))[c(1,3,2,4)]
        # options(ursaRasterizeSelection=selection)
        # options(ursaRasterizeBbox=bbox)
         theta2 <- mean(range(lon2))
        # print(c(old=theta2,new=theta))
        # lon_0 <- if (is.numeric(lon0)) lon0 else mean(range(lon2))
         lon_0 <- if (is.numeric(lon0)) lon0 else round(theta2,4)
         lat_ts <- if (is.numeric(lat0)) lat0 else round(mean(lat2),4)
         lat_0 <- if (lat_ts>=0) 90 else -90
         if (verbose)
            print(c(lon0=lon_0,lat0=lat_0,lat_ts=lat_ts))
         if (proj=="auto") {
            if (("web" %in% style)||(tpat==3))
           # if (style=="web")
               proj <- "merc"
            else if ((any(lat2<0))&&(any(lat2>0)))
               proj <- "merc"
            else if (isEPSG)
               proj <- "epsg"
            else
               proj <- "stere"
         }
         if (verbose)
            print(c(lon_0=lon_0,lat_ts=lat_ts))
         if (proj=="stere") {
            t_srs <- paste("","+proj=stere"
                          ,paste0("+lat_0=",lat_0)
                          ,paste0("+lat_ts=",lat_ts)
                          ,paste0("+lon_0=",lon_0)
                          ,"+k=1","+x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
         }
         else if (proj=="laea") {
            t_srs <- paste("","+proj=laea"
                          ,paste0("+lat_0=",lat_0)
                          ,paste0("+lon_0=",lon_0)
                          ,"+k=1","+x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
         }
         else if (proj=="merc")
            t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                          ,"+lat_ts=0.0",paste0("+lon_0=",lon_0)
                          ,"+x_0=0.0 +y_0=0 +k=1.0"
                          ,"+units=m +nadgrids=@null +wktext +no_defs")
         else if ((proj %in% c("longlat"))||(isLonLat)) {
            t_srs <- "+proj=longlat +datum=WGS84 +no_defs"
         }
         else if (proj %in% c("zzzgoogle")) {
            if (FALSE)#(selection %in% c(1000L,3L))
               t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                             ,"+lat_ts=0.0 +lon_0=180.0 +x_0=0.0 +y_0=0 +k=1.0"
                             ,"+units=m +nadgrids=@null +wktext +no_defs")
            else
               t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                             ,"+lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0"
                             ,"+units=m +nadgrids=@null +wktext +no_defs")
         }
         else
            t_srs <- NULL
         if (is.character(t_srs)) {
            if (isSF) {
               obj <- sf::st_transform(obj,t_srs)
            }
            if (isSP)
               obj <- sp::spTransform(obj,t_srs)
         }
        # xy <- .project(xy,t_srs)
        # print(summary(xy))
      }
      else if (isEPSG) {
         a <- .try({
            t_srs <- .epsg2proj4(style)
            if (isSF)
               obj <- sf::st_transform(obj,t_srs)
            if (isSP) {
               obj <- sp::spTransform(obj,t_srs)
            }
         })
         if (!a) {
            t_srs <- .epsg2proj4(style,force=TRUE)
            if (isSF)
               obj <- sf::st_transform(obj,t_srs)
            if (isSP) {
               obj <- sp::spTransform(obj,t_srs)
            }
         }
      }
   }
  # else if (((isSF)&&(is.ursa(a,"grid")))||((isSP)&&(is.ursa(asp,"grid")))) {
   else if (is.ursa(g0,"grid")) {
      t_srs <- g0$proj4
      if (isSF) {
        # opE <- options(show.error.messages=TRUE)
        # print(sf::st_bbox(obj))
        # print(sf::st_crs(obj)$proj4string)
         obj <- sf::st_transform(obj,t_srs)
        # print(sf::st_crs(obj)$proj4string)
        # print(sf::st_bbox(obj))
        # options(opE)
      }
      if (isSP) {
        # print(sp::proj4string(obj))
        # print(c(sp::bbox(obj)))
         obj <- sp::spTransform(obj,t_srs) ## not tested
        # print(sp::proj4string(obj))
        # print(c(sp::bbox(obj)))
      }
   }
   if (isSF) {
      obj_geom <- sf::st_geometry(obj)
     # bbox <- c(sf::st_bbox(obj_geom)) ## low performance sp<=0.5-2
      if (inherits(obj,"sfc"))
         bbox <- c(sf::st_bbox(obj_geom))
      else
         bbox <- attr(obj[[attr(obj,"sf_column")]],"bbox") ## ~ sf::st_bbox
      proj4 <- sf::st_crs(obj)$proj4string
   }
   if (isSP) {
      obj_geom <- switch(geoType,POLYGON=sp::geometry(obj)@polygons
                                   ,LINE=sp::geometry(obj)@lines
                                  ,POINT=sp::geometry(obj))
      bbox <- c(sp::bbox(obj))
      names(bbox) <- c("xmin","ymin","xmax","ymax")
      proj4 <- sp::proj4string(obj)
   }
   if ((bbox["xmin"]==bbox["xmax"])||(bbox["ymin"]==bbox["ymax"]))
      bbox <- bbox+100*c(-1,-1,1,1)
   if (FALSE) {
      .sc <- ifelse(.lgrep("\\+proj=(zzzlonglat|zzzmerc)",proj4)>0,0,expand-1)
      bbox[c(1,3)] <- mean(bbox[c(1,3)])+c(-1,1)*expand*diff(bbox[c(1,3)])/2
      bbox[c(2,4)] <- mean(bbox[c(2,4)])+c(-1,1)*expand*diff(bbox[c(2,4)])/2
   }
   else {
      .sc <- (expand-1)*sqrt(diff(bbox[c(1,3)])*diff(bbox[c(2,4)]))/2
      bbox <- bbox+c(-1,-1,1,1)*.sc
   }
   if (is.null(g0)) {
      if (!is.na(cell)) {
         res <- rep(cell,length=2)
         g0 <- regrid(bbox=unname(bbox[c("xmin","ymin","xmax","ymax")]),res=res
                     ,proj4=proj4,border=0)
      }
      else {
         res <- max(c(bbox["xmax"]-bbox["xmin"]),(bbox["ymax"]-bbox["ymin"]))/len
         p <- pretty(res)
         res <- p[which.min(abs(res-p))]
         g1 <- ursa_grid()
         g1$resx <- g1$resy <- as.numeric(res)
         g1$proj4 <- proj4
         g0 <- regrid(g1,bbox=unname(bbox[c("xmin","ymin","xmax","ymax")])
                     ,border=0) ## border=border
      }
   }
   if (toZoom) {
      res <- with(g0,sqrt(resx*resy))
      s <- 2*6378137*pi/(2^(1:21+8))
      zoom <- which.min(abs(s-res))
      g0 <- regrid(g0,res=s[zoom])
   }
   if (border>0) {
      g0 <- regrid(g0,border=border)
   }
   if ((FALSE)&&(isWeb)) {
      bbox <- with(g0,.project(cbind(c(minx,maxx),c(miny,maxy))
                              ,proj4,inv=TRUE))[c(1,3,2,4)]
      basemap <- .geomap(bbox,border=0,style=style,verbose=verbose)
      g0 <- ursa(basemap,"grid")
      attr(obj,"basemap") <- basemap
   }
   if (is.null(g2))
      session_grid(g0)
   attr(obj,"grid") <- g0
   attr(obj,"toUnloadMethods") <- toUnloadMethods
   attr(obj,"colnames") <- dname
   attr(obj,"style") <- style
   attr(obj,"geocodeStatus") <- geocodeStatus
  # attr(obj,"engine") <- ifelse(isSF,"sf","sp")
   if (exists("dsn"))
      attr(obj,"dsn") <- dsn
  # class(obj) <- c(class(obj),"ursaVectorExternal")
   obj
}
