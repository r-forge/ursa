'.glance' <- function(dsn,layer=".*",crs,attr=".+",len=640,expand=1.05
                        ,border=15,lat0=NA,lon0=NA,resetProj=FALSE
                        ,proj=c("auto","stere","laea","merc","internal"
                               ,"google","longlat")
                        ,feature=c("auto","attribute","geometry")
                        ,verbose=TRUE,...) {
  # feature <- "geometry"
   proj <- match.arg(proj)
   feature <- match.arg(feature)
   if (!requireNamespace("sf")) {
      res <- .rasterize(fname=dsn,crs=crs,attr=attr,len=len,res=NA,nodata=-9999
                        ,expand=expand,border=border,lat0=lat0,lon0=lon0
                        ,internalCall=FALSE,gdalopt="",ogropt="",where=""
                        ,strings=FALSE,resetProj=resetProj
                        ,proj=proj,feature="overlay",ID=FALSE,paint=5
                        ,verbose=FALSE)
      return(res)
   }
   cpg <- NULL
   if (!((is.character(dsn))&&(length(dsn)==1))) {
      if (inherits(dsn,"SpatialPointsDataFrame")) {
         a <- sf::st_as_sfc(dsn);rm(dsn)
      }
      else if (inherits(dsn,"sf")) {
         a <- dsn;rm(dsn)
      }
      else if (is.array(dsn)) {
         return(display(dsn,...))
      }
      else {
         a <- try(sf::st_as_sfc(dsn))
      }
      if (inherits(a,"try-error")) {
         print(class(dsn))
         return(31L)
      }
   }
   else {
      if (!file.exists(dsn)) {
         aname <- paste0(dsn,".zip")
         if (isZip <- file.exists(aname)) {
            ziplist <- unzip(aname);on.exit(file.remove(ziplist))
            dsn <- .grep("\\.shp$",ziplist,value=TRUE)
         }
      }
      else {
         if (isZip <- .lgrep("\\.zip$",dsn)>0) {
            ziplist <- unzip(dsn);on.exit(file.remove(ziplist))
            dsn <- .grep("\\.shp$",ziplist,value=TRUE)
         }
      }
      opW <- options(warn=0)
      lname <- sf::st_layers(dsn)$name
      if (length(lname)>1) {
         layer <- .grep(layer,lname,value=TRUE)
         if (length(layer)>1) {
            print(paste("Select only one layer:",paste(paste0(seq(layer),")"),sQuote(layer),collapse=", ")),quote=FALSE)
            return(30L)
         }
      }
      a <- sf::st_read(dsn,layer=layer,quiet=TRUE)
      options(opW)
      if (.lgrep("\\.shp$",dsn)) {
         cpgname <- .gsub("\\.shp$",".cpg",dsn)
         if (file.exists(cpgname)) {
            cpg <- readLines(cpgname,warn=FALSE)
            if (cpg=="UTF-8")
               cpg <- NULL
         }
      }
   }
   dname <- names(sf::st_agr(a))
   dname <- .grep(attr,dname,value=TRUE)
  # str(dname);q()
   if (!length(dname)) {
      message("unable to get attributes by name")
     # str(a)
     # return(invisible(20L))
   }
   if (!FALSE) {
     # lc <- Sys.getlocale("LC_CTYPE")
     # Sys.setlocale("LC_CTYPE","Russian")
      for (i in seq_along(dname)) {
         da <- a[,dname[i],drop=TRUE][,,drop=TRUE]
         if (is.character(da))
            Encoding(da) <- "UTF-8"
         a[,dname[i]] <- da
      }
     # Sys.setlocale("LC_CTYPE",lc)
     # str(a)
   }
   geoType <- unique(as.character(sf::st_geometry_type(a)))
   if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType))
      a <- sf::st_cast(a,"MULTIPOLYGON")
   sf_geom <- sf::st_geometry(a)
   g0 <- getOption("ursaSessionGrid")
   if (!(proj %in% c("auto","internal")))
      resetProj <- TRUE
  # if ((proj=="internal")&&(!is.na(keepProj))) {
  #    g0 <- NULL
  # }
   if (resetProj)
      crs <- NULL
   else if ((missing(crs))&&(!is.null(g0)))
      crs <- g0
   else if (missing(crs))
      crs <- NULL
   if ((is.null(crs))||(is.numeric(lon0))||(is.numeric(lat0))) {
  # if ((resetProj)||(is.ursa(crs,"grid"))||(is.numeric(lon0))||(is.numeric(lat0))) {
      proj4 <- sf::st_crs(a)$proj4string
      if ((proj4=="")&&(!(proj %in% c("auto","internal")))) {
         resetProj <- TRUE
         proj4 <- "auto"
      }
      isLonLat <- .lgrep("\\+proj=longlat",proj4)>0
      if ((proj %in% c("auto"))&&(isLonLat)) { ## added 2016-08-09
         resetProj <- TRUE
         proj4 <- "auto"
      }
      isMerc <- .lgrep("\\+proj=merc",proj4)>0
      if (isMerc) {
         major <- .gsub(".+\\+a=(\\S+)\\s.+","\\1",proj4) ## 20037508
         if (identical(major,proj4)) {
            if (.lgrep("\\+(datum|ellps)=WGS84",proj4))
               B <- 20037508
            else
               B <- 20037508
           # print(B)
         }
         else
            B <- as.numeric(major)*pi
      }
      if (is.numeric(lon0) | is.numeric(lat0) | resetProj) {
         a2 <- sf::st_transform(a,4326)
         sf_geom2 <- sf::st_geometry(a2)
         xy <- lapply(sf_geom2,function(z) {
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
         xy <- matrix(c(unlist(xy)),ncol=2,byrow=TRUE)
         if (verbose)
            print(summary(xy))
         lon2 <- xy[,1]
         lat2 <- xy[,2]
         if (nrow(xy)>1) {
            lon3 <- lon2
            lon4 <- lon2
            ind3 <- which(lon3<0)
            ind4 <- which(lon4>180)
            lon3[ind3] <- lon3[ind3]+360
            lon4[ind4] <- lon4[ind4]-360
            sd2 <- sd(lon2)
            sd3 <- sd(lon3)
            sd4 <- sd(lon4)
            if (verbose)
               print(data.frame(sd2=sd2,'sd3R'=sd3,'sd4L'=sd4
                               ,n3=length(ind3),n4=length(ind4)))
            if ((sd3<=sd2)&&(sd3<=sd4)) {
              # if (length(ind3))
              #    selection <- 3L
               lon2 <- lon3
            }
            else if ((sd4<=sd2)&&(sd4<=sd3)) {
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
         lon_0 <- if (is.numeric(lon0)) lon0 else mean(range(lon2))
         lat_ts <- if (is.numeric(lat0)) lat0 else mean(lat2)
         lat_0 <- if (lat_ts>=0) 90 else -90
         if (verbose)
            print(c(lon0=lon_0,lat0=lat_0,lat_ts=lat_ts))
         if (proj=="auto") {
            if ((any(lat2<0))&&(any(lat2>0)))
               proj <- "merc"
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
         else if (proj %in% c("merc","google"))
            t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                          ,"+lat_ts=0.0",paste0("+lon_0=",lon_0)
                          ,"+x_0=0.0 +y_0=0 +k=1.0"
                          ,"+units=m +nadgrids=@null +wktext  +no_defs")
         else if (proj %in% c("longlat"))
            t_srs <- "+proj=longlat +datum=WGS84 +no_defs"
         else if (proj %in% c("zzzgoogle")) {
            if (FALSE)#(selection %in% c(1000L,3L))
               t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                             ,"+lat_ts=0.0 +lon_0=180.0 +x_0=0.0 +y_0=0 +k=1.0"
                             ,"+units=m +nadgrids=@null +wktext  +no_defs")
            else
               t_srs <- paste("","+proj=merc +a=6378137 +b=6378137"
                             ,"+lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0"
                             ,"+units=m +nadgrids=@null +wktext  +no_defs")
         }
         else
            t_srs <- NULL
         if (is.character(t_srs)) {
            a <- sf::st_transform(a,t_srs)
         }
        # xy <- .project(xy,t_srs)
        # print(summary(xy))
      }
   }
   else if (is.ursa(a,"grid")) {
      if (is.character(t_srs))
         a <- sf::st_transform(a,ursa_proj(crs))
   }
   sf_geom <- sf::st_geometry(a)
   bbox <- sf::st_bbox(a)
   if ((bbox["xmin"]==bbox["xmax"])||(bbox["ymin"]==bbox["ymax"]))
      bbox <- bbox+100*c(-1,-1,1,1)
   if (TRUE) {
      .sc <- ifelse(.lgrep("\\+proj=(zzzlonglat|zzzmerc)",proj4)>0,0,expand-1)
      bbox[c(1,3)] <- mean(bbox[c(1,3)])+c(-1,1)*expand*diff(bbox[c(1,3)])/2
      bbox[c(2,4)] <- mean(bbox[c(2,4)])+c(-1,1)*expand*diff(bbox[c(2,4)])/2
   }
   proj4 <- sf::st_crs(a)$proj4string
   isGoogle <- proj %in% c("google") & requireNamespace("ggmap")
   if (isGoogle) {
      xy <-cbind(bbox[c(1,3)],bbox[c(2,4)])
      ll <- .project(xy,proj4,inv=TRUE)
      if ((xy[2,1]>0)&(ll[2,1])<0)
         ll[2,1] <- 360-ll[2,1]
     # r <- c(min(ll[,1]),min(ll[,2]),max(ll[,1]),max(ll[,2]))
      cxy <- colMeans(xy)
      center <- .project(cxy,proj4,inv=TRUE)
     # print(center)
      nc <- bbox[3]-bbox[1]
      nr <- bbox[4]-bbox[2]
      s <- 640
      if (nc>nr) {
         size <- c(s,round(s*nr/nc))
         sc <- 1
      }
      else {
         size <- c(round(s*nc/nr),s)
         sc <- nc/nr
      }
      dx <- ((bbox[3]-bbox[1])/6378137)*(180/pi)/sc
      for (zoom in 1:21) {
        # print(data.frame(zoom=zoom,google=360/2^(zoom-1),me=dx))
         if (360/2^(zoom-1)<dx*1.1)
            break
      }
      arglist <- list(...)
      myname <- names(arglist)
      mlang <- "en-EN"
      if (length(ind <- .grep("language",myname)))
         mlang <- arglist[[ind]]
      mtype <- "terrain"
      if (length(ind <- .grep("maptype",myname)))
         mtype <- arglist[[ind]]
      mcolor <- ifelse(mtype=="terrain","bw","color")
      if (length(ind <- .grep("color",myname)))
         mcolor <- arglist[[ind]]
      mscale <- 1
      if (length(ind <- .grep("scale",myname)))
         mscale <- arglist[[ind]]
      if (!(mscale %in% c(1,2)))
         mscale <- 1
      mzoom <- zoom
      if (length(ind <- .grep("zoom",myname)))
         mzoom <- arglist[[ind]]
      m <- ggmap::get_googlemap(center=center+c(0,0),zoom=mzoom,size=size
                        ,scale=mscale # ifelse(expand>1.5,1,1)
                        ,maptype=mtype
                        ,language=mlang
                        ,sensor=FALSE
                        ,urlonly=!TRUE,filename = "___ggmapTemp"
                        ,color=mcolor)
      basemap <- as.ursa(m)
      crs <- ursa_grid(basemap)
      lon_0 <- as.numeric(.gsub("^.*\\+lon_0=(\\S+)\\s.+$","\\1",proj4))
      radius <- as.numeric(.gsub("^.*\\+a=(\\S+)\\s.+$","\\1",proj4))
      B <- radius*pi
      shift <- B*lon_0/180
     # print(crs)
      crs$minx <- crs$minx-shift
      crs$maxx <- crs$maxx-shift
      crs$proj4 <- proj4
      if (crs$minx<(-2*B)) {
         crs$minx <- crs$minx+2*B
         crs$maxx <- crs$maxx+2*B
      }
      else if (crs$minx>2*B) {
         crs$minx <- crs$minx-2*B
         crs$maxx <- crs$maxx-2*B
      }
      if (crs$maxx<(-2*B)) {
         crs$minx <- crs$maxx+2*B
         crs$maxx <- crs$maxx+2*B
      }
      else if (crs$maxx>2*B) {
         crs$minx <- crs$minx-2*B
         crs$minx <- crs$minx-2*B
      }
     # print(crs)
      ursa_grid(basemap) <- crs
      if (crs$minx>crs$maxx) {
         if ((crs$minx<0)&&(bbox[1]>0))
            bbox[1] <- bbox[1]-2*B
         else if ((crs$minx>0)&&(bbox[1]<0))
            bbox[1] <- bbox[1]+2*B
         if ((crs$maxx<0)&&(bbox[3]>0))
            bbox[3] <- bbox[3]-2*B
         else if ((crs$maxx>0)&&(bbox[3]<0))
            bbox[3] <- bbox[3]+2*B
      }
   }
   else {
      res <- max(c(bbox["xmax"]-bbox["xmin"]),(bbox["ymax"]-bbox["ymin"]))/len
      p <- pretty(res)
      res <- p[which.min(abs(res-p))]
      g1 <- ursa_grid()
      g1$resx <- g1$resy <- res
      g1$proj4 <- proj4
      crs <- regrid(g1,bbox=unname(bbox[c("xmin","ymin","xmax","ymax")]),border=border)
      basemap <- NULL
   }
   session_grid(crs)
  # print(session_grid())
   geoType <- unique(as.character(sf::st_geometry_type(a)))
   if ((FALSE)&&(.lgrep("POLYGON",geoType))) {
      valid <- .try(ov <- sf::st_covers(sf_geom,sparse=!FALSE))
      if (!valid)
         isOverlap <- FALSE
      else 
         isOverlap <- length(which(sapply(ov,function(x) length(x)>1)))>0
   }
   else
      isOverlap <- FALSE
   if (feature=="auto")
      feature <- ifelse(isOverlap,"geometry","attribute")
  # dname <- names(attr(a,"relation_to_geometry"))
  # ct <- colorize(a[,dname[4],drop=TRUE][,1],alpha=1)
  # print(ct)
  # print(unname(ct$colortable[ct$ind]))
   if (feature=="attribute") {
      ct <- vector("list",length(dname))
      alpha <- ifelse(isGoogle,0.75,1)
      cpg <- "1251"
      for (i in seq_along(dname)) {
        # print(i)
        # print(dname[i])
        # str(a[,dname[i]][,1])
         val <- a[,dname[i],drop=TRUE][,1]
         if ((is.character(cpg))&&(is.character(val)))
            val <- iconv(val,"UTF-8","1251")
        # print(all(is.na(val)))
        # print(a[,dname[i],drop=TRUE][,1])
         ct[[i]] <- colorize(val,alpha=alpha,...)
        # print(names(ct[[i]]$colortable),quote=FALSE)
        # print(ct[[i]])
        # print("-----------------------------------------")
      }
      if (length(dname))
         res <- lapply(rep(NA,length(ct)),ursa_new)
      else
         res <- list(geometry=ursa_new())
      compose_open(res,scale=ifelse(isGoogle,1,NA),...)
      gline <- compose_gridline(...)
      cline <- compose_coastline(...)
      pb <- ursaProgressBar(min=0,max=length(res))
      for (i in seq_along(res)) {
         if (isGoogle)
            panel_new(fill="transparent",...)
         else
            panel_new(...) #fill=ifelse(isGoogle,"transparent","chessboard"))
         panel_plot(basemap,alpha=0.5)
        # if ((!length(ct))||(all(is.na(ct[[i]]$index)))) {
         if (!length(ct)) {
            panel_plot(sf_geom)
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
            if (.lgrep("polygon",geoType)) {
               panel_plot(sf_geom,col=col,border=bg.polygon,lwd=0.1,lty="blank")
               panel_plot(sf_geom,col="transparent",border=bg.polygon,lwd=0.1)
            }
            if (.lgrep("point",geoType)) {
               panel_plot(sf_geom
                         ,col=bg.point,bg=col,pch=21,lwd=0.25,cex=1)
            }
            if (.lgrep("line",geoType)) {
               panel_plot(sf_geom,lwd=3,col=bg.line)
               panel_plot(sf_geom,lwd=2,col=col)
            }
         }
         panel_coastline(cline)
         panel_gridline(gline)
         panel_scalebar(pos=ifelse(isGoogle,"topleft","bottomleft"),...)
         setUrsaProgressBar(pb)
      }
      close(pb)
      if (length(ct)) {
         ct <- lapply(ct,function(x) x$colortable)
         names(ct) <- dname
         compose_legend(ct,las=2,trim=2L)
      }
      compose_close(...)#res)
   }
   else if (feature=="geometry") {
      print(geoType)
      n <- length(sf_geom)
      da <- a[,dname,drop=TRUE]
      da <- rbind(format(da),paste0(names(da),":"))
     # e <- format(t(da),justify="right")
      e <- .gsub("^\\s+","",t(da))
     # e1 <- paste(apply(e[,c(n+1,1)],1,paste,collapse=" "),collapse="\n")
     # print(e)
     # message(e1)
     # q()
      res <- ursa_new(nband=n)
      ct <- lapply(seq(n),function(i) colorize(0L))
      compose_open(res,legend=NULL,...)
      gline <- compose_gridline(...)
      cline <- compose_coastline(...)
      pb <- ursaProgressBar(min=0,max=length(res))
      for (i in seq_along(res)) {
         panel_new()
        # panel_plot(sf_geom[[i]])
         if (.lgrep("polygon",geoType)) {
            panel_plot(sf_geom[[i]]
                      ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]),lwd=0.1)
         }
         if (.lgrep("point",geoType)) {
            panel_plot(sf_geom[[i]]
                      ,col="black",bg=unname(ct[[i]]$colortable[ct[[i]]$ind])
                      ,pch=21,lwd=0.25,cex=1)
         }
         if (.lgrep("line",geoType)) {
            panel_plot(sf_geom[[i]],lwd=3,col="#0000007F")
            panel_plot(sf_geom[[i]],lwd=2
                      ,col=unname(ct[[i]]$colortable[ct[[i]]$ind]))
         }
         panel_coastline(cline)
         panel_gridline(gline)
         panel_scalebar(pos=ifelse(isGoogle,"bottom","bottomleft"))
         e1 <- paste(apply(e[,c(n+1,i)],1,paste,collapse=" "),collapse="\n")
         panel_annotation(text=e1,adj=0,...) # pos="topleft")
         setUrsaProgressBar(pb)
      }
      close(pb)
      compose_close(...)
      str(n)
   }
   invisible(0L)
}
'.cmd.glance' <- function() {
   a <- .args2list()
   do.call(".glance",a)
   NULL
}
