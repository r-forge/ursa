'.geocode' <- function(loc=NULL,area=c("point","bounding"),type=NULL
                      ,select=c("top","expand","all")
                      ,service=c("nominatim","google"),verbose=TRUE) {
   if (is.null(loc)) {
      if (!TRUE) {
         dst <- tempfile()
         download.file("http://ip-api.com/csv",dst,mode="wt")
         a <- readLines(dst,warn=FALSE)
         file.remove(dst)
         a <- unlist(strsplit(a,split=","))
         pt <- c(lon=as.numeric(a[9]),lat=as.numeric(a[10]))
      }
      else
         a <- readLines("http://ip-api.com/line")
      pt <- c(lon=as.numeric(a[9]),lat=as.numeric(a[8]))
      attr(pt,"type") <- "IP"
      return(pt)
   }
   area <- match.arg(area)
   service <- match.arg(service)
   select <- match.arg(select)
   if (service=="nominatim") {
       ## curl -H Accept-Language:de 'http://nominatim.openstreetmap.org......."
      src <- paste0("http://nominatim.openstreetmap.org/search.php?q=",loc
                  # ,"&polygon_text=1"
                   ,"&format=xml","&bounded=0","&accept-language=en-US,ru")
      dst <- tempfile() # "nominatim.xml" # tempfile()
      download.file(URLencode(URLencode(iconv(src,to="UTF-8")))
                   ,dst,quiet=!verbose)
      xmlstring <- scan(dst,character(),quiet=!verbose)
      if (dirname(dst)==tempdir())
         file.remove(dst)
      ind <- grep("geotext",xmlstring)
      if (length(ind)) {
         geotext <- xmlstring[ind]
         print(geotext)
      }
      ptype <- .grep("^type=",xmlstring,value=TRUE)
      ptype <- .gsub(".*\'(.+)\'.*","\\1",ptype)
      lon <- .grep("lon=",xmlstring,value=TRUE)
      lon <- as.numeric(.gsub(".*\'(.+)\'.*","\\1",lon))
      lat <- .grep("lat=",xmlstring,value=TRUE)
      lat <- as.numeric(.gsub(".*\'(.+)\'.*","\\1",lat))
      pt <- cbind(lon=lon,lat=lat)
      rownames(pt) <- ptype
      ind <- grep("boundingbox",xmlstring)
      bounding <- xmlstring[ind]#[1]
      bounding <- .gsub(".*\"(.+)\".*","\\1",bounding)
      bounding <- lapply(bounding,function(p){
         as.numeric(unlist(strsplit(p,split=",")))
      })
      bounding <- do.call(rbind,bounding)
      rownames(bounding) <- ptype
      colnames(bounding) <- c("miny","maxy","minx","maxx")
      ann <- .grep("display_name=",xmlstring,value=TRUE)
      ann <- .gsub(".*\"(.+)\".*","\\1",ann)
      importance <- .grep("importance",xmlstring,value=TRUE)
      importance <- as.numeric(.gsub(".*\'(.+)\'.*","\\1",importance))
     # type <- NULL ## debug
      if (is.character(type)) {
         typeInd <- which(!is.na(match(ptype,type)))
         if (length(typeInd)) {
            bounding <- bounding[typeInd,,drop=FALSE]
            pt <- pt[typeInd,,drop=FALSE]
            importance <- importance[typeInd]
         }
      }
      important <- which(importance==max(importance))
      if (select=="top") {
         lat <- lat[important]
         lon <- lon[important]
         pt <- pt[important,,drop=FALSE]
         bounding <- bounding[important,,drop=FALSE]
         ptype <- ptype[important]
      }
      if (area=="bounding") {
         if (FALSE) {
            print(bounding)
            da <- data.frame(lon=range(bounding[,c(3,4)])
                            ,lat=range(bounding[,c(1,2)]))#,z=1:2)
         }
         bbox <- c(minx=min(bounding[,3]),miny=min(bounding[,1])
                  ,maxx=max(bounding[,4]),maxy=max(bounding[,2]))
         if (select=="top")
            attr(box,"type") <- type
         return(bbox)
      }
      if (area=="point") {
         if (nrow(pt)==1) {
            ptype <- rownames(pt)
            ptname <- colnames(pt)
            pt <- c(pt)
            names(pt) <- ptname
            attr(pt,"type") <- ptype
         }
         return(pt)
      }
   }
   else if (service=="google") {
      src <- paste0("https://maps.googleapis.com/maps/api/geocode/xml?"
                   ,"address=",loc)
      dst <- tempfile() # "google.xml" #tempfile()
      download.file(URLencode(URLencode(iconv(src,to="UTF-8")))
                   ,dst,quiet=!verbose)
      xmlstring <- scan(dst,character(),quiet=!verbose)
      if (dirname(dst)==tempdir())
         file.remove(dst) 
      ilat <- .grep("<lat>",xmlstring)
      ilon <- .grep("<lng>",xmlstring)
      glat <- as.numeric(.gsub("<lat>(.+)</lat>","\\1",xmlstring[ilat]))
      glon <- as.numeric(.gsub("<lng>(.+)</lng>","\\1",xmlstring[ilon]))
      aname <- .grep("(location|southwest|northeast)",xmlstring[c(ilat,ilon)-1]
                    ,value=TRUE)
      aname <- .gsub("(<|>)","",aname)
      bname <- .grep("<(viewport|bounds)>",xmlstring[c(ilat,ilon)-2]
                    ,value=TRUE)
      bname <- .gsub("(<|>)","",bname)
      iname <- .grep("<(viewport|bounds)>",xmlstring)
      names(glon) <- names(glat) <- aname
      if ((!length(glon))||(!length(glat))) {
        # loc <- RJSONIO::fromJSON("http://ip-api.com/json")
         return(NULL)
      }
      ptype <- .grep("<location_type>",xmlstring,value=TRUE)
      ptype <- .gsub("<.+>(.+)</.+>","\\1",ptype)
      pt <- c(lon=unname(glon["location"]),lat=unname(glat["location"]))
      attr(pt,"type") <- ptype
      glon <- glon[.grep("location",aname,invert=TRUE)]
      glat <- glat[.grep("location",aname,invert=TRUE)]
      lname <- paste0(rep(bname,each=2),".",names(glon))
      names(glon) <- names(glat) <- lname
      n <- length(bname)
      bbox <- cbind(minx=rep(NA,n),miny=rep(NA,n),maxx=rep(NA,n),maxy=rep(NA,n))
      rownames(bbox) <- bname
      for (i in seq_along(bname)) {
         bbox[i,"minx"] <- glon[.grep(paste0(bname[i],"\\.southwest"),lname)]
         bbox[i,"miny"] <- glat[.grep(paste0(bname[i],"\\.southwest"),lname)]
         bbox[i,"maxx"] <- glon[.grep(paste0(bname[i],"\\.northeast"),lname)]
         bbox[i,"maxy"] <- glat[.grep(paste0(bname[i],"\\.northeast"),lname)]
      }
      if (verbose) {
         print(pt)
         print(bbox)
      }
      if (area=="point") {
         return(pt)
      }
      else if (area=="bounding") {
         if (select %in% "top")
            return(bbox["viewport",])
         if (select %in% "expand")
            return(bbox["bounds",])
         return(bbox)
         if (FALSE) {
            glon <- range(glon)
            glat <- range(glat)
            dlon <- abs(diff(glon))
            dlat <- abs(diff(glat))
            if ((dlon<0.01)&&(dlat<0.01)) {
               sc <- abs(1/cos(mean(glat)))
               mul <- 1 # 3
               glon <- mean(glon)+mul*abs(diff(glon))*sc*c(-1,1)/2
               glat <- mean(glat)+mul*abs(diff(glat))*c(-1,1)/2
            }
            da <- data.frame(lon=range(glon),lat=range(glat))
         }
         return(bbox)
      }
   }
   return(NULL)
}
'.geomap' <- function(place=NULL,style="",geocode="",size=NA,zoom="0",verbose=FALSE) {
  # a <- .glance(place)
   if (!nchar(style))
      style <- "google static"
   staticMap <- c("openstreetmap","google","sputnik")
   tilePatt <- paste0("(",paste0(unique(c(staticMap,.untile())),collapse="|"),")")
   if (!.lgrep(tilePatt,style))
      art <- "none"
   else {
      art <- .gsub2(tilePatt,"\\1",style)
      proj <- "merc"
   }
   isStatic <- .lgrep("static",style)>0
   if ((!isStatic)&&(":::" %in% as.character(as.list(match.call())[[1]]))) {
      stop("Operation is prohibited: unable to display credits.")
   }
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
   mlen <- switch(art,google=640,openstreetmap=960,sputnik=511)
   if (isStatic) {
      len[len>mlen] <- mlen
   }
  # canTile <- .lgrep(art,eval(as.list(args(".untile"))$server))>0
   canTile <- .lgrep(art,.untile())>0
   isTile <- .lgrep("tile",style)>0 & canTile
   if ((!isStatic)&&(!isTile)) {
      if (art %in% staticMap)
         isStatic <- TRUE
      else if (canTile)
         isTile <- TRUE
      else
         art <- "none"
   }
   isColor <- .lgrep("colo(u)*r",style)>0
   isWeb <- .lgrep(tilePatt,art)
   if (verbose)
      print(data.frame(proj=proj,art=art,color=isColor,static=isStatic
                      ,canTile=canTile,tile=isTile,web=isWeb))
   if (!nchar(geocode))
      geocode <- if (.lgrep("google",style)) "google" else "nominatim"
   geocodeList <- c("google","nominatim")
   geocode <- match.arg(geocode,geocodeList)
   geocodeStatus <- FALSE
   if (!((is.numeric(place))&&(length(place)==4))) {
      place <- try(.geocode(place,service=geocode,area="bounding"
                           ,select="expand",verbose=verbose))
      if (inherits(place,"try-error")) {
         geocode <- switch(geocode,google="nominatim",nominatim="google")
         place <- try(.geocode(place,service=geocode,area="bounding"
                              ,select="expand",verbose=verbose))
      }
      if (!inherits(place,"try-error"))
         geocodeStatus <- TRUE
   }
   credits <- attr(.untile(),"credits")[art]
  # str(unname(place),digits=8)
   bbox <- place
  # size <- c(640,640)
   B0 <- 6378137
   B <- B0*pi
   x <- B*bbox[c(1,3)]/180
   cross180 <- x[1]>x[2]
   if (cross180)
      x[1] <- x[1]-2*B
   lon_0 <- round(180*mean(x)/B,6)
   proj4 <- paste("","+proj=merc +a=6378137 +b=6378137"
                 ,"+lat_ts=0.0",paste0("+lon_0=",lon_0)
                 ,"+x_0=0.0 +y_0=0 +k=1.0"
                 ,"+units=m +nadgrids=@null +wktext  +no_defs")
   bbox <- matrix(bbox,ncol=2,byrow=TRUE)
   bbox <- .project(bbox,proj4)
   bbox <- c(xmin=bbox[1,1],ymin=bbox[1,2],xmax=bbox[2,1],ymax=bbox[2,2])
   res <- max(c((bbox["xmax"]-bbox["xmin"])/size[1]
        ,(bbox["ymax"]-bbox["ymin"])/size[2]))
   s <- 2*6378137*pi/(2^(1:21+8))
   zman <- zoom
   zoom <- which.min(abs(s-res))
   for (i in c(zoom,zoom-1)) {
      if (i<1)
         break
      res <- s[i]
      g0 <- regrid(ursa_grid(),res=res,proj4=proj4#,border=border
                  ,setbound=unname(bbox[c("xmin","ymin","xmax","ymax")]))
     # if (isTile)
     #    break
      if ((g0$columns<=size[1])&&(g0$rows<=size[2]))
         break
   }
   zoom <- i
   if (is.numeric(zman))
      zman <- round(zman)
   else if (is.character(zman)) { ## "+1" "---"
      if (.lgrep("^(\\+|\\-)\\d$",zman)) {
         zman <- eval(parse(text=paste0(zoom,zman)))
      }
      else {
         zman <- round(as.numeric(zman))
         if (zman==0)
            zman <- zoom
      }
   }
   if (FALSE) {
      pattZoom <- "(zoom=(\\d+))"
      if (.lgrep(pattZoom,style))
         zman <- as.integer(.gsub2(pattZoom,"\\2",style))
      else
         zman <- zoom
   }
   if (zman!=zoom) {
      if (verbose)
         print(c(zoomAuto=zoom,zoomManual=zman))
      m <- 2^(zoom-zman)
      g0 <- regrid(g0,mul=1/m,expand=m)
      zoom <- zman
   }
   if ((TRUE)&&(geocodeStatus)) {
      x0 <- (g0$minx+g0$maxx)/2
      y0 <- (g0$miny+g0$maxy)/2
      minx <- x0-g0$resx*size[1]/2
      maxx <- x0+g0$resx*size[1]/2
      miny <- y0-g0$resy*size[2]/2
      maxy <- y0+g0$resy*size[2]/2
      g0 <- regrid(g0,minx=minx,maxx=maxx,miny=miny,maxy=maxy)
   }
   B <- 6378137*pi*(0.95+1*0.05)
   if (g0$maxy>(+B))
      g0 <- regrid(g0,maxy=+B)
   if (g0$miny<(-B))
      g0 <- regrid(g0,miny=-B)
   cxy <- with(g0,c(minx+maxx,miny+maxy)/2)
   center <- c(.project(cxy,proj4,inv=TRUE))
   bound <- .project(with(g0,rbind(c(minx,miny),c(maxx,maxy))),g0$proj4
                     ,inv=TRUE)
   xr <- with(g0,seq(minx,maxx,len=32))
   yr <- rep(with(g0,(miny+maxy)/2),length(xr))
   lr <- .project(cbind(xr,yr),g0$proj4,inv=TRUE)[,1]
   cross180 <- length(which(diff(lr)<0))
  # print(g0)
   if (isTile) {
     # proj <- c("cycle","mapsurfer","sputnik")[2]
      B0 <- 6378137
      B <- B0*pi
      dz <- 2^zoom
      res <- 2*pi*B0/dz
      dx0 <- lon_0*pi/180*B0
      minx <- g0$minx+dx0
      maxx <- g0$maxx+dx0
      epsg3857 <- paste("","+proj=merc +a=6378137 +b=6378137"
                       ,"+lat_ts=0.0 +lon_0=0.0"
                       ,"+x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null"
                       ,"+wktext  +no_defs")
      g1 <- regrid(g0,setbound=c(minx,g0$miny,maxx,g0$maxy),proj=epsg3857)
      g1 <- regrid(g1,res=2*pi*B0/dz)
     # g1a <- regrid(g1,res=c(g0$resx,g0$resy))
      g1 <- regrid(g1,minx=g1$minx-dx0,maxx=g1$maxx-dx0,res=c(g0$resx,g0$resy))
     # print(g1)
      sx <- sort(c(c(minx,maxx)
                  ,seq(-B*3,+B*3,by=2*B)))
      sx <- sx[sx>=minx & sx<=maxx]
      dx <- diff(sx)
      dr <- 3+2*zoom
      yr <- with(g0,seq(maxy,miny,len=dr))
      t0 <- NULL
      h <- NULL
      for (j in seq_along(dx)) {
         tX <- NULL
         xr <- seq(sx[j]+1e-6,sx[j+1]-1e-6,len=dr)
         gr <- .project(expand.grid(x=xr,y=yr),g0$proj4,inv=TRUE)
         gr[,1] <- gr[,1]-lon_0
        # print(unique(gr[,1]))
        # print(unique(gr[,2]))
         for (i in seq(nrow(gr))) {
            tX <- rbind(tX,.deg2num(lon=gr[i,1],lat=gr[i,2],zoom=zoom))
         }
         ind <- which(tX[,1]<0)
         if (length(ind))
            tX[ind,1] <- dz+tX[ind,1]
         ind <- which(tX[,1]>=dz)
         if (length(ind))
            tX[ind,1] <- tX[ind,1]-dz
         tX <- unique(tX)
        # print(tX)
         hX <- unique(tX[,1])
         lon <- (c(head(hX,1),tail(hX,1))+c(0,1))/dz*360-180
        # print(lon)
         t0 <- rbind(t0,tX)
         h <- c(h,hX)
         if (j==1)
            v <- unique(tX[,2])
      }
      if (verbose) {
         colnames(t0) <- c("x","y")
         print(cbind(t0,z=zoom))
      }
      tgr <- expand.grid(z=zoom,y=v,x=h)
      igr <- expand.grid(y=seq_along(v)-1,x=seq_along(h)-1)
      img <- array(0,dim=c(256*length(v),256*length(h),3))
     # print(tgr)
     # print(igr)
      for (i in seq(nrow(tgr))) {
         img2 <- .untile(z=zoom,x=tgr[i,"x"],y=tgr[i,"y"],server=art
                        ,verbose=verbose)
         img[igr[i,"y"]*256+seq(256),igr[i,"x"]*256+seq(256),] <- img2[,,1:3]
      }
      basemap <- 255*as.ursa(img,aperm=TRUE,flip=TRUE)
      ursa(basemap,"grid") <- g1
      basemap <- as.integer(regrid(basemap,g0,resample=FALSE))
   }
   else { ## staticmap
      php <- switch(art
         ,sputnik=paste0("http://static-api.maps.sputnik.ru/v1/"
                        ,"?width={w}&height={h}&z={z}&clng={lon}&clat={lat}")
         ,google=paste0("http://maps.googleapis.com/maps/api/staticmap"
                       ,"?center={lat},{lon}&zoom={z}&size={w}x{h}")
         ,openstreetmap=paste0("http://staticmap.openstreetmap.de/staticmap.php"
                              ,"?center={lat},{lon}&zoom={z}&size={w}x{h}")
         )
     # php <- switch(art,google="http://maps.googleapis.com/maps/api/staticmap"
     #        ,openstreetmap="http://staticmap.openstreetmap.de/staticmap.php")
      isOSM <- .lgrep("openstreetmap",art)
      isGoogle <- .lgrep("google",art)
      adv <- paste(.grep("=",unlist(strsplit(style,split="\\s+")),value=TRUE)
                  ,collapse="&")
      if ((isOSM)&&(cross180)) {
         B0 <- 6378137
         B <- B0*pi
         minx <- g0$minx+lon_0*pi/180*B0
         maxx <- g0$maxx+lon_0*pi/180*B0
         sx <- sort(c(c(minx,maxx)
                     ,seq(-B*3,+B*3,by=2*B)))
         sx <- sx[sx>=minx & sx<=maxx]
         dx <- diff(sx)
         mx <- sx[-1]-dx/2
        # print(sx)
        # print(round(mx))
         lon2 <- 180*mx/B
         lon2[lon2<(-180)] <- lon2[lon2<(-180)]+360
         lon2[lon2>(+180)] <- lon2[lon2>(+180)]-360
        # print(g0$columns)
        # print(g0$columns*dx/sum(dx))
         col2 <- ceiling(g0$columns*dx/sum(dx))
         if (sum(col2)!=g0$columns)
            col2[cross180+1] <- g0$columns-sum(col2[seq(cross180)])
        # print(col2)
         img <- array(0,dim=c(g0$rows,g0$columns,3))
         for (i in seq(cross180+1)) {
            src <- php
            src <- .gsub("{w}",col2[i],src)
            src <- .gsub("{h}",g0$rows,src)
            src <- .gsub("{lon}",round(lon2[i],11),src)
            src <- .gsub("{lat}",round(center[2],11),src)
            src <- .gsub("{z}",zoom,src)
            if (nchar(adv)) {
              #    src <- paste0(src,"&",adv)
               s1 <- .args2list(.gsub("&"," ",src))
               s2 <- .args2list(.gsub("&"," ",adv))
               ind <- match(names(s2),names(s1))
               ind1 <- which(!is.na(ind))
               if (length(ind1))
                  s1[na.omit(ind)] <- s2[ind1]
               ind2 <- which(is.na(ind))
               if (length(ind2))
                  s1 <- c(s1,s2[ind2])
               src <- unlist(s1)
               src <- .gsub("^=","",paste(names(src),src,sep="=",collapse="&"))
            }
            fname <- tempfile()
            download.file(src,fname,mode="wb",quiet=!verbose)
            j <- if (i==1) 0 else sum(col2[seq(i-1)])
            img[,j+seq(col2[i]),] <- png::readPNG(fname)
            file.remove(fname)
         }
         basemap <- 255*as.ursa(img,aperm=TRUE,flip=TRUE)
      }
      else {
         center <- round(center,11)
         src <- php
         src <- .gsub("{w}",g0$columns,src)
         src <- .gsub("{h}",g0$rows,src)
         src <- .gsub("{lon}",format(center[1],scientific=FALSE),src)
         src <- .gsub("{lat}",format(center[2],scientific=FALSE),src)
         src <- .gsub("{z}",zoom,src)
         if (nchar(adv)) {
           #    src <- paste0(src,"&",adv)
            s1 <- .args2list(.gsub("&"," ",src))
            s2 <- .args2list(.gsub("&"," ",adv))
            ind <- match(names(s2),names(s1))
            ind1 <- which(!is.na(ind))
            if (length(ind1))
               s1[na.omit(ind)] <- s2[ind1]
            ind2 <- which(is.na(ind))
            if (length(ind2))
               s1 <- c(s1,s2[ind2])
            src <- unlist(s1)
            src <- .gsub("^=","",paste(names(src),src,sep="=",collapse="&"))
         }
         fname <- tempfile()
         download.file(src,fname,mode="wb",quiet=!verbose)
         basemap <- 255*as.ursa(png::readPNG(fname),aperm=TRUE,flip=TRUE)
         file.remove(fname)
      }
      mul <- unique(c(ursa_ncol(basemap)/ursa_ncol(g0)
                     ,ursa_nrow(basemap)/ursa_nrow(g0)))
      if (length(mul)==1)
         g0 <- regrid(g0,mul=mul)
      ursa(basemap,"grid") <- g0
   }
   if (!isColor) {
      basemap <- as.integer(round(sum(basemap*c(0.30,0.59,0.11))))
      basemap <- colorize(basemap,minvalue=0,maxvalue=255,pal=c("black","white"))
   }
   attr(basemap,"credits") <- unname(attr(.untile(),"credits")[art])
   session_grid(g0)
   basemap
}
