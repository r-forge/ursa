'.rasterize' <- function(fname,crs,attr=".+",len=640,res=NA,nodata=-9999
                        ,expand=1.05,border=0,lat0=NA,lon0=NA,internalCall=FALSE
                        ,gdalopt="",ogropt="",where=""
                        ,strings=FALSE,resetProj=FALSE
                        ,proj=c("auto","stere","laea","merc","internal"
                               ,"google","longlat")
                       # ,keepProj=NA
                        ,feature=c("overlay","group","separate"),ID=FALSE
                        ,paint=5,verbose=FALSE,...) {
   requireNamespace("foreign") ## 'foreign' is in R-core
   if ((FALSE)&&(verbose))
      str(list(fname=fname,internalCall=internalCall,resetProj=resetProj
              ,proj=match.arg(proj),feature=match.arg(feature)
              ,crs=!missing(crs)))
   if (!internalCall) {
      fpath <- dirname(fname)
      list1 <- .dir(path=fpath
                       ,pattern=paste0("^",.gsub("\\.shp(\\.zip)*$","",basename(fname))
                                     ,"\\.(cpg|dbf|prj|shp|shx)(\\.zip)*$")
                       ,full.names=TRUE)
      sname <- .maketmp()
      if (length(list1)) {
         isZip <- .lgrep("\\.zip$",list1)>0
         if (isZip)
            list1 <- unzip(list1)
         listTmp <- paste0(sname,.gsub("(.+)(\\.(cpg|dbf|prj|shp|shx)$)","\\2",list1))
         if (isZip) {
            file.rename(list1,listTmp)
            file.remove(list1[which(file.exists(list1))])
         }
         else
           file.copy(list1,listTmp,overwrite=TRUE)
      }
      else {
         cmd <- paste("ogr2ogr","-f",dQuote("ESRI Shapefile")
                     ,paste0(sname,".shp"),fname)
         if (verbose)
            message(cmd)
         system(cmd)
         listTmp <- .dir(pattern=paste0("^",sname,"\\.(cpg|dbf|prj|shp|shx)$"))
      }
      fname <- paste0(sname,".shp")
      on.exit(file.remove(listTmp[file.exists(listTmp)]))
   }
   else
      sname <- .gsub("\\.shp$","",fname)
   dbfname <- paste0(sname,".dbf")
   da <- foreign::read.dbf(dbfname)
   if (!internalCall) {
      da$plfeatid <- seq(nrow(da))
      op <- options(warn=-1)
      foreign::write.dbf(da,dbfname)
      options(op)
   }
   ##~ else {
      ##~ print("HERE")
      ##~ da$plfeatid <- seq(nrow(da))
   ##~ }
   fname0 <- fname
   proj <- match.arg(proj)
   feature <- match.arg(feature)
   g0 <- getOption("ursaSessionGrid")
   if (!(proj %in% c("auto","internal")))
      resetProj <- TRUE
   keepProj <- getOption("ursaRasterizeProj")
   if (is.null(keepProj))
      keepProj <- NA
   if ((proj=="internal")&&(!is.na(keepProj))) {
     # proj <- "auto"
     # resetProj <- FALSE
      g0 <- NULL
   }
   if (resetProj)
      crs <- NULL
   else if ((missing(crs))&&(!is.null(g0)))
      crs <- g0
   else if (missing(crs))
      crs <- NULL
  # str(list(crs=crs))
   if (!.lgrep("\\.shp$",fname))
      fname <- paste0(fname,".shp")
   ##~ if (.lgrep("\\s",fname))
   fname <- dQuote(fname)
   lname <- .gsub("\\.shp","",basename(fname0))
   cmd <- paste("ogrinfo",fname,"-so -geom=NO"#,"-where",dQuote(where)
               ,"-sql",dQuote(paste("select * FROM",dQuote(dQuote(lname)))))
   ##~ cmd <- paste("ogrinfo",fname,"-geom=NO")
   if (verbose)
      message(cmd)
   info <- system(cmd,intern=TRUE)
   tname <- .maketmp(ext="shp")
   rname <- .maketmp()
   if ((is.null(crs))||(is.numeric(lon0))||(is.numeric(lat0))) {
      proj4 <- system(paste("gdalsrsinfo","-o proj4",fname),intern=TRUE)
      proj4 <- .gsub("'","",proj4)
      if (!length(proj4))
         proj4 <- ""
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
            print(B)
         }
         else
            B <- as.numeric(major)*pi
      }
      if (verbose)
         print(data.frame(isLonLat=isLonLat
                         ,internalCall=internalCall
                         ,first=isLonLat & !internalCall
                         ,lon0=is.numeric(lon0)
                         ,lat0=is.numeric(lat0)
                         ,resetProj=resetProj))
     # if (isLonLat & !internalCall | is.numeric(lon0) | is.numeric(lat0) | resetProj) {
      if (FALSE & !internalCall | is.numeric(lon0) | is.numeric(lat0) | resetProj) {
         cond1 <- proj=="laea" & is.numeric(lon0)
         cond2 <- proj=="stere" & is.numeric(lon0) & is.numeric(lat0)
         if (!(cond1 | cond2)) {
            # do not need searching lon0
         }
         Fout <- paste0(gsub("\\.shp$","",fname0),".out")
        # cmd <- c("ogrinfo",ogropt,"-al",fname)
         cmd <- c("ogrinfo","-al","-where",dQuote(where),fname)
         if (verbose)
            message(paste(cmd,collapse=" "))
         system2(cmd[1],cmd[-1],stdout=Fout)
         xy <- readLines(Fout,warn=FALSE)
         file.remove(Fout)
         patt <- "^Geometry:\\s(.+)$"
         geometry <- .gsub(patt,"\\1",.grep(patt,xy,value=TRUE))
         if (!(geometry %in% c("Point","3D Point")) ){
            xy <- .grep("^\\s{2}(multi)*(polygon|point|line(string*))\\s"
                        ,xy,value=TRUE)
            xy <- gsub("(\\(|\\)|[A-Za-z])"," ",xy)
            xy <- unlist(strsplit(xy,split=","))
         }
         else {
            xy <- .grep("^\\s{2}.*point\\s.+",xy,value=TRUE)
            xy <- gsub("(\\(|\\)|[A-Za-z_])"," ",xy)
            xy <- .grep("^\\s+$",xy,invert=TRUE,value=TRUE)
         }
         nr <- length(xy)
         xy <- unlist(strsplit(xy,split="\\s+"))
        # xy <- as.numeric(xy[nchar(xy)>0])
         xy <- as.numeric(xy[.grep("(\\-)*\\d+(\\.\\d+)*",xy,invert=FALSE)])
         xy <- matrix(xy,nrow=nr,byrow=TRUE)[,c(1,2)]
        # xy <- matrix(xy,ncol=2,byrow=TRUE)
         if ((FALSE)&&(!isLonLat)) {
            x_0 <- as.numeric(.gsub(".*\\+x_0=(\\S+)\\s.*","\\1",proj4))
            if ((x_0!=0)&&(verbose)) {
               opW <- options(warn=1)
               warning("Is 'x_0' parameter in PROJ.4 string extracted correctly?")
               options(opW)
              # xy[,1] <- xy[,1]+x_0
            }
            if (verbose)
               print(summary(xy))
         }
         if (!isLonLat)
            xy <- .project(xy,proj4,inv=TRUE)
         if (verbose) {
            print(summary(xy))
            print(proj4)
         }
         lon2 <- xy[,1]
         lat2 <- xy[,2]
         lon3 <- lon2
         lon4 <- lon2
         ind3 <- which(lon3<0)
         ind4 <- which(lon4>180)
         lon3[ind3] <- lon3[ind3]+360
         lon4[ind4] <- lon4[ind4]-360
         sd2 <- sd(lon2)
         sd3 <- sd(lon3)
         sd4 <- sd(lon4)
        # selection <- 0L
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
        # if ((any(lon2<180))&&(any(lon2>180)))
        #    selection <- 3L
         if (verbose)
            print(summary(lon2))
        # selection <- 0L
         if ((FALSE)&&(mean(lon2)>0))
            lon2[lon2<0] <- lon2[lon2<0]+360
         bbox <- c(range(lon2),range(lat2))[c(1,3,2,4)]
        # options(ursaRasterizeSelection=selection)
         options(ursaRasterizeBbox=bbox)
         lon_0 <- if (is.numeric(lon0)) lon0 else mean(range(lon2))
         lat_ts <- if (is.numeric(lat0)) lat0 else mean(lat2)
         lat_0 <- if (lat_ts>=0) 90 else -90
        # keepProj <- getOption("ursaRasterizeProj")
         if (is.na(keepProj)) {
            keepProj <- proj
            options(ursaRasterizeProj=proj)
         }
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
                          ,"+k=1","+x_0=0 +y_0=0 +ellsp=WGS84 +units=m +no_defs")
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
         cmd <- paste("ogr2ogr",ifelse(verbose,"","-q")
                     ,"--config OGR_ENABLE_PARTIAL_REPROJECTION YES"
                     ,ogropt
                     ,"-skipfailures"
                     ,"-where",dQuote(where)
                     ,"-t_srs",dQuote(t_srs)
                     ,"-overwrite",tname,fname)
         if (verbose)
            message(cmd)
         system(cmd)
         rel <- as.list(match.call())
         rel[["fname"]] <- tname
         rel$internalCall <- TRUE
         rel$lon0 <- NULL
         rel$lat0 <- NULL
         rel$resetProj <- FALSE
         rel$proj <- "internal"
        # rel$keepProj <- keepProj
         ret <- do.call(as.character(rel[1]),rel[-1]) ## recursive!!!
         lname <- .gsub("\\.shp","",basename(tname))
         file.remove(.dir(pattern=paste0("^",lname,"\\.(cpg|dbf|prj|shp|shx)$")))
         return(ret)
      }
      extent <- getOption("ursaRasterizeBbox")
      if ((isMerc)&&(!is.null(extent))) {
        # selection <- getOption("ursaRasterizeSelection")
         if (!is.null(extent)) {
            print(extent)
            if (extent[2]==-90)
               extent[2] <- -90+1e-4
            if (extent[4]==90)
               extent[4] <- 90-1e-4
            extent <- matrix(extent,ncol=2,byrow=TRUE)
           # print(extent)
           # print(proj4)
            bbox <- .project(extent,proj4)
            if ((extent[2,1]>0)&&(bbox[2,1]<0))
               bbox[2,1] <- 2*B+bbox[2,1]
            if ((extent[1,1]<0)&&(bbox[1,1]>0))
               bbox[1,1] <- -2*B+bbox[1,1]
           # print(bbox)
           # q()
            bbox <- c(t(bbox))
         }
      }
      else {
         bbox <- .grep("Extent:",info,value=TRUE)
         bbox <- unlist(strsplit(.gsub("(^\\S+|\\(|\\)|,|\\-\\s)","",bbox)
                                ,split="\\s+"))
         bbox <- as.numeric(bbox[nchar(bbox)>0])
      }
      .sc <- ifelse(.lgrep("\\+proj=(zzzlonglat|zzzmerc)",proj4)>0,0,expand-1)
      .sc2 <- sqrt((bbox[3]-bbox[1])*(bbox[4]-bbox[2]))
      .sc <- ifelse(.sc2==0,0.05,.sc2)*c(-.sc,.sc)
      bbox[c(1,3)] <- bbox[c(1,3)]+.sc
      bbox[c(2,4)] <- bbox[c(2,4)]+.sc
      if (.lgrep("\\+proj=zzzmerc",proj4)) {
         if (bbox[1]<(-20037508))
            bbox[1] <- -20037508
         if (bbox[3]>20037508)
            bbox[3] <- 20037508
      }
      if (.lgrep("\\+proj=longlat",proj4)) {
         if (bbox[2]<(-90))
            bbox[2] <- -90
         if (bbox[4]>(+90))
            bbox[4] <- 90
         if ((bbox[1]<(-180))&&(bbox[3]>(+180))) {
            bbox[1] <- -180
            bbox[3] <- +180
         }
         else if ((bbox[1]<0)&&(bbox[3]>360)) {
            bbox[1] <- 0
            bbox[3] <- 360
         }
      }
      rm(.sc,.sc2)
      if ((FALSE)&&(isMerc)) {
         if (bbox[1]<(-B))
            bbox[1] <- -B
         if (bbox[2]<(-B))
            bbox[2] <- -B
         if (bbox[3]>(+B))
            bbox[3] <- B
         if (bbox[4]>(+B))
            bbox[4] <- B
      }
     # keepProj <- getOption("ursaRasterizeProj")
      if ((!is.na(keepProj))&&(keepProj=="google")) {
         xy <-cbind(bbox[c(1,3)],bbox[c(2,4)])
         ll <- .project(xy,proj4,inv=TRUE)
        # print(bbox)
        # print(xy)
        # print(ll)
         if ((xy[2,1]>0)&(ll[2,1])<0)
            ll[2,1] <- 360-ll[2,1]
        # r <- c(min(ll[,1]),min(ll[,2]),max(ll[,1]),max(ll[,2]))
         requireNamespace("ggmap")
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
         m <- ggmap::get_googlemap(center=center+c(0,0),zoom=zoom,size=size
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
         session_grid(crs)
        # print(session_grid(basemap))
        # q()
        # display(basemap,scale=1,coast=FALSE,pointsize=12)
        # q()
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
        # print(bbox)
        # str(session_grid())
         if (FALSE) {
            compose_open(scale=1,pointsize=12,legend=NULL)
            panel_new()
            panel_raster(basemap)
            segments(bbox[1],bbox[2],bbox[3],bbox[4],col="orange",lwd=10)
            segments(bbox[3],bbox[2],bbox[1],bbox[4],col="orange",lwd=10)
            segments(bbox[1],bbox[2],bbox[1],bbox[4],col="orange",lwd=10)
            segments(bbox[1],bbox[4],bbox[3],bbox[4],col="orange",lwd=10)
            segments(bbox[3],bbox[4],bbox[3],bbox[2],col="orange",lwd=10)
            segments(bbox[3],bbox[2],bbox[1],bbox[2],col="orange",lwd=10)
            panel_points(cxy[1],cxy[2])
            panel_gridline(decor=TRUE)
            panel_scalebar(position="top")
            compose_close()
            q()
           # display(basemap,scale=1,gridline=FALSE,coastline=FALSE)
        }
      }
      else {
         if (!is.numeric(res)) {
            res <- max(c(bbox[3]-bbox[1]),(bbox[4]-bbox[2]))/len
           # res <- sqrt((bbox[3]-bbox[1])*(bbox[4]-bbox[2]))/len
            p <- pretty(res)
            res <- p[which.min(abs(res-p))]
         }
         g1 <- .grid.skeleton()
         g1$resx <- g1$resy <- res
         g1$proj4 <- proj4
         crs <- regrid(g1,bbox=bbox,border=border)
      }
      if ((.lgrep("\\+proj=zzzmerc",proj4))&&(keepProj!="google")) {
         B <- 20037508
         if ((crs$maxx>B)||(crs$minx<(-B))) {
            x <- seq(crs,"x")
            ind <- which(x>=(-B+res/2) & x<=(B-res/2))
            crs$columns <- length(ind)
            crs$minx <- head(x[ind],1)-res/2
            crs$maxx <- tail(x[ind],1)+res/2
         }
         B <- 20037508 #-15*111*1e3
         if ((crs$maxy>B)||(crs$miny<(-B))) {
            y <- seq(crs,"y")
            ind <- which(y>=(-B+res/2) & y<=(B-res/2))
            crs$rows <- length(ind)
            crs$miny <- head(y[ind],1)-res/2
            crs$maxy <- tail(y[ind],1)+res/2
         }
      }
      tname <- fname
   }
   else {
      if (is.null(crs))
         crs <- session_grid()
      cmd <- with(crs,paste("ogr2ogr",ifelse(verbose,"","-q"),ogropt
                           ,"-t_srs",dQuote(proj4)
                           ,"-where",dQuote(where)
                           ,"-overwrite",tname,fname))
      if (verbose)
         message(cmd)
      system(cmd)
   }
   geom <- .grep("Geometry:",info,value=TRUE)
   geom <- .gsub("Geometry:\\s","",geom)
   for (a in c("line","point","polygon"))
      geom[.lgrep(a,geom)] <- a
  # tname <- fname
   lname <- .gsub("\\.shp","",basename(tname))
   session_grid(crs)
   nodata2 <- nrow(da)+1
   if (feature=="overlay") {
      cmd <- with(crs,paste("gdal_rasterize",ifelse(verbose,"","-q")
                           ,gdalopt
                           ,"-a plfeatid"
                           ,"-tr",resx,resy
                           ,"-te",minx,miny,maxx,maxy
                           ,"-a_nodata",nodata2,"-init",nodata2
                           ,"-of ENVI -ot Int32"
                           ,tname,rname))
      if (verbose)
         message(cmd)
      system(cmd)
      ref <- read_envi(rname,verbose=verbose)
      if (FALSE) {
         print(ursa_grid(ref))
         print(ref)
         display(ref)
         q()
      }
   }
   else {
      nodata2 <- nrow(da)+1
      ref <- ursa_new(nband=nrow(da),ignorevalue=nodata2)
      pb <- ursaProgressBar(min=0,max=nrow(da))
      for (i in seq(nrow(da))) {
         cmd <- with(crs,paste("gdal_rasterize",ifelse(verbose,"","-q")
                              ,gdalopt
                              ,"-burn",i
                              ,"-where",dQuote(paste0("plfeatid=",sQuote(i)))
                              ,"-tr",resx,resy
                              ,"-te",minx,miny,maxx,maxy
                              ,"-a_nodata",nodata2,"-init",nodata2
                              ,"-of ENVI -ot Int16"
                              ,tname,rname))
         if (verbose)
            message(cmd)
         system(cmd)
         ref[i] <- read_envi(rname)
         setUrsaProgressBar(pb,i)
      }
      close(pb)
   }
   ind <- .grep("plfeatid",colnames(da))
   if (nchar(where))
      plfeatid <- da$plfeatid
   if (!ID)
      da <- da[,-ind,drop=FALSE]
   else {
      colnames(da)[ind] <- "FeatureID"
      attr <- paste0("(",attr,"|FeatureID)")
   }
   da <- da[,.grep(attr,colnames(da)),drop=FALSE]
   if (length(ind <- which(apply(da,2,function(x) all(is.na(x))))))
      da <- da[,-ind,drop=FALSE]
   if (feature!="group") {
      res <- vector("list",ncol(da))
      names(res) <- colnames(da)
      for (j in seq(ncol(da))) {
         d <- da[,j]
         isChar <- is.character(d) | is.factor(d)
        # print(d)
        # print(unclass(d))
         if (is.factor(d))
            d <- as.character(d)
         if (feature=="separate")
         {
            r <- ref
            if (!is.numeric(d)) {
               dname <- unique(d)
               d2 <- as.integer(factor(d,levels=dname))-1L
               print(d)
            }
            else
               d2 <- d
            for (i in seq_along(d2))
               r[ref==i] <- d2[i]
            bandname(r) <- as.character(format(d,digits=3))
            if (!is.numeric(d)) {
               ct <- rep(NA,length=length(dname))
               names(ct) <- dname
               ursa_colortable(r) <- ct
            }
         }
         else if (feature=="overlay") {
            if (nchar(where))
               r <- reclass(ref,src=plfeatid,dst=d)
            else
               r <- reclass(ref,src=seq(nrow(da)),dst=d)
            bandname(r) <- names(res)[j]
            ignorevalue(r) <- .optimal.nodata(r$value)
         }
         if ((FALSE)&&(isChar)) {
            r <- colorize(r,name=levels(d),...) ## ,stretch="category"
            q()
         }
         if (!(geom %in% c("polygon"))) {
           # print(as.table(r))
            if (paint>1) {
               isCategory <- .is.category(r$value)
               r <- focal_special(r,"gauss",cover=1e-6,size=paint,sigma=1e11
                                 ,fillNA=TRUE,verbose=!TRUE)
               if (isCategory) {
                 # r$value[] <- as.integer(.round(r$value))
                  class(r$value) <- "ursaCategory"
               }
            }
           # print(as.table(r))
         }
         res[[j]] <- r
      }
   }
   else {
      res <- ref
      bn <- apply(da,1,function(x) paste(colnames(da),x,sep="=",collapse=" "))
      bn <- .gsub("\\s*=\\s*","=",bn)
      bandname(res) <- bn
      if (!(geom %in% c("polygon"))) {
         res <- focal_special(res,"gauss",size=paint,sigma=1e11,fillNA=TRUE)
      }
   }
   file.remove(.dir(paste0("^",lname,"\\.(cpg|dbf|prj|shp|shx)$")))
   envi_remove(rname)
   if ((!is.na(keepProj))&&(keepProj=="google")) {
      ##~ res <- lapply(res,function(x) {ursa_grid(x) <- ursa_grid(basemap);x})
      res$basemap <- basemap
      ##~ session_grid(basemap)
   }
   options(ursaRasterizeProj=NULL)
   res
}
'.cmd.rasterize' <- function() {
   a <- .args2list()
   ind <- .grep("proj",names(a))
   isGoogle <- length(ind)>0 && a[[ind]]=="google"
   b <- do.call(".rasterize",a)
   .elapsedTime("rasterization done")
  # saveRDS(b,"res1.rds")
  # b2 <- if (.is.ursa_stack(b)) b[[1]] else b
  # write_envi(b2,"res1")
   if (!TRUE)
      b <- b[c(length(b)-1,length(b))]
   session_grid(b)
   a$verbose <- FALSE
   if (!isGoogle) {
      a[[1]] <- quote(b)
      do.call("display",a)
      return(NULL)
   }
   lat <- with(session_grid(),.project(cbind(c(0,0),c(miny,maxy)),proj4,inv=TRUE)[,2])
   sc <- abs(1/cos(lat*pi/180))
   coast <- !FALSE #with(session_grid(),(minx>=(-B))&&(maxx<=(+B)))
   n <- length(b)-1
   cl <- compose_design(b[-n])
   s1 <- colSums(cl$layout)
   las <- if (all(s1[c(1,length(s1))]==0)) 2L else 1L
  # options(ursaPngAuto=TRUE)
   compose_open(cl,scale=1,pointsize=12)
   bl <- compose_coastline(coast=coast)
   ll <- compose_gridline(decor=TRUE)
   ct <- vector("list",n)
   for (i in seq(n)) {
      panel_new()#col="white")
      panel_raster(b[[n+1]],alpha=0.75)
      ct[[i]] <- panel_raster(b[[i]],alpha=0.75)
      panel_coastline(bl)
      panel_gridline(ll)
      if (max(sc)/min(sc)>1.2) {
         if (length(unique(sign(lat)))>1) {
            panel_scalebar(pos="center")
            panel_scalebar(pos=ifelse(sc[1]>sc[2],"bottom","top"))
         }
         panel_scalebar(pos="bottom")
         panel_scalebar(pos="top")
      }
      else
         panel_scalebar(pos="top")
   }
   unit <- names(b)
   for (i in seq(n))
     # .compose_legend(ct[[i]],las=las)
      do.call("legend_colorbar",c(list(ct[[i]]),las=las,units=unit[i],a[-1]))
   compose_close()
   NULL
}
