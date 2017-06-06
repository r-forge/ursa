'.geomap' <- function(loc=NULL,style="",geocode="",size=NA,zoom="0"
                     ,border=0,verbose=FALSE) {
  # a <- .glance(loc)
   if (!nchar(style))
      style <- "google static"
   if (is.na(zoom))
      zoom <- "0"
   staticMap <- c("openstreetmap","google","sputnik")
   tilePatt <- paste0("(",paste0(unique(c(staticMap,.untile())),collapse="|"),")")
   if (!.lgrep(tilePatt,style))
      art <- "none"
   else {
      art <- .gsub2(tilePatt,"\\1",style)
      proj <- "merc"
   }
   isStatic <- .lgrep("static",style)>0
  # if ((!isStatic)&&("ursa" %in% loadedNamespaces())) {
  #    stop("Operation is prohibited: unable to display attribution.")
  # }
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
   geocodeList <- eval(as.list(args(.geocode))$service)
   if (!nchar(geocode))
      geocode <- if (.lgrep("google",style)) "google" else "nominatim"
   geocode <- match.arg(geocode,geocodeList)
   geocodeStatus <- FALSE
   if (!((is.numeric(loc))&&(length(loc)==4))) {
      loc <- try(.geocode(loc,service=geocode,area="bounding"
                           ,select="top",verbose=verbose))
      if (inherits(loc,"try-error")) {
         geocode <- switch(geocode,google="nominatim",nominatim="google")
         loc <- try(.geocode(loc,service=geocode,area="bounding"
                              ,select="top",verbose=verbose))
      }
      if (!inherits(loc,"try-error"))
         geocodeStatus <- TRUE
   }
  # print(loc)
  # q()
  # copyright <- attr(.untile(),"copyright")[art]
  # str(unname(loc),digits=8)
   if (length(loc)==2)
      bbox <- c(loc,loc)
   else
      bbox <- loc
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
   if ((is.numeric(zman))&&(zman<=0))
      zman <- as.character(zman)
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
      if (zman>18)
         zman <- 18
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
      if (FALSE)
         g0 <- regrid(g0,mul=1/m,expand=m)
      else {
         bbox <- with(g0,c(minx,miny,maxx,maxy))
         m2 <- if (m<1) 1 else with(g0,sqrt((maxx-minx)*(maxy-miny)))/2
        # print(c(m=m,m2=m2,expand=m*m2))
         g0 <- regrid(g0,mul=1/m,bbox=bbox+c(-1,-1,1,1)*m*m2)
      }
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
   if (border>0) {
      g0 <- regrid(g0,border=border)
   }
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
      if (FALSE) {
         g2 <- regrid(g0,res=2*pi*B0/dz)
         xr <- with(g2,seq(minx,maxx,by=resx)[-1]-resx/2)+dx0
         yr <- rev(with(g2,seq(miny,maxy,by=resy)[-1]-resy/2))
         g2 <- regrid(g2,res=c(g0$resx,g0$resy),proj4=g0$proj4)
        # g2 <- regrid(g2,minx=g2$minx-dx0,maxx=g2$maxx-dx0)
      }
      else {
         g1 <- regrid(g0,setbound=c(minx,g0$miny,maxx,g0$maxy),proj=epsg3857)
         g1 <- regrid(g1,res=2*pi*B0/dz)
         g1 <- regrid(g1,res=c(g0$resx,g0$resy),proj4=g0$proj4)
         g1$minx <- g1$minx-dx0
         g1$maxx <- g1$maxx-dx0
      }
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
      img <- array(0L,dim=c(256*length(v),256*length(h),3))
     # print(tgr)
     # print(igr)
      for (i in seq(nrow(tgr))) {
         img2 <- .untile(z=zoom,x=tgr[i,"x"],y=tgr[i,"y"],server=art
                        ,verbose=verbose)
         img[igr[i,"y"]*256L+seq(256),igr[i,"x"]*256+seq(256),] <- img2[,,1:3]
      }
      basemap <- as.ursa(img,aperm=TRUE,flip=TRUE)
      ursa(basemap,"grid") <- g1
     # basemap <- as.integer(regrid(basemap,g0,resample=FALSE))
      basemap <- regrid(basemap,g0,resample=FALSE)
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
         basemap <- as.integer(255*as.ursa(img,aperm=TRUE,flip=TRUE))
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
         basemap <- as.integer(255L*as.ursa(png::readPNG(fname)
                                           ,aperm=TRUE,flip=TRUE))
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
   if (isTile)
      attr(basemap,"copyright") <- unname(attr(.untile(),"copyright")[art])
   session_grid(g0)
   basemap
}
