##~ '.panel_coastline' <- function(
                               ##~ coastline=TRUE,col="grey60",fill="transparent"
                              ##~ ,detail=NA,density=NA,angle=NA,land=FALSE
                              ##~ ,lwd=1,lty=1L,...) {
   ##~ NULL
##~ }
'compose_coastline' <- function(...) {
   arglist <- list(...)
   kwd <- "coast(line)*"
   coastline <- .getPrm(arglist,name=paste0("(",kwd,"|decor$)")
                       ,class=list("integer","logical"),default=TRUE)
   if (any(!coastline)) {
      res <- list(coast_xy=NULL)
      class(res) <- "ursaCoastLine"
      return(res)
   }
   res <- .getPrm(arglist,name=kwd,class="ursaCoastLine",default=NULL)
   if (!is.null(res)) {
      return(res)
   }
   res <- getOption("ursaPngCoastLine")
   if (!is.null(res))
      return(res)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   defcol <- ifelse(bg<128,"#FFFFFF3F","#0000003F") # grey60
   col <- .getPrm(arglist,name="(col|line)",kwd=kwd,default=defcol) 
   fill <- .getPrm(arglist,name="fill",kwd=kwd,default="transparent")
   detail <- .getPrm(arglist,name="detail",kwd=kwd,default=NA_character_)
   density <- .getPrm(arglist,name="density",kwd=kwd,default=NA_real_)
   angle <- .getPrm(arglist,name="angle",kwd=kwd,default=NA_real_)
   land <- .getPrm(arglist,name="land",kwd=kwd,default=FALSE)
   lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=0.5)
   lty <- .getPrm(arglist,name="lty",kwd=kwd,default=1L)
   fail180 <- .getPrm(arglist,name="fail180",kwd=kwd,default=NA)
   obj <- .getPrm(arglist,name=paste0("(^$|",kwd,")") ## name="^$"
                 ,class=list("character","matrix","SpatialPolygonsDataFrame")#[-3]
                 ,default=NULL)
   verbose <- .getPrm(arglist,name="verbose",kwd=kwd,default=FALSE)
   if (is.integer(coastline)) {
      panel <- coastline
     # coastline <- TRUE
   }
   else
      panel <- 0L
   .compose_coastline(obj=obj,panel=panel,col=col,fill=fill,detail=detail
                     ,density=density,angle=angle
                     ,land=land,lwd=lwd,lty=lty,fail180=fail180,verbose=verbose)
}
'.compose_coastline' <- function(obj=NULL,panel=0,col=NA,fill="transparent"
                                ,detail=NA,density=NA,angle=NA,land=FALSE
                                ,lwd=0.5,lty=1,fail180=NA,verbose=FALSE) {
   if (verbose)
      str(list(obj=obj,panel=panel,col=col,fill=fill,detail=detail
              ,density=density,angle=angle
              ,land=land,lwd=lwd,lty=lty,fail180=fail180))
   if (!is.null(obj)) {
      isPoly <- inherits(obj,"SpatialPolygonsDataFrame")
      if ((is.matrix(obj))&&(ncol(obj)==2))
         coast_xy <- obj
      else if ((is.character(obj))||(isPoly)) {
         if ((isPoly)||(.lgrep("\\.shp",obj))) {
            if (isPoly)
               a <- obj
            else
               a <- .shp.read(obj)
            a <- lapply(methods::slot(a,grep("(polygons|lines)"
                              ,methods::slotNames(a),value=TRUE)),function(x) {
               y <- lapply(methods::slot(x,grep("(Polygons|Lines)"
                              ,methods::slotNames(x),value=TRUE)),function(y) {
                  do.call("rbind",lapply(list(sp::coordinates(y),cbind(NA,NA))
                                        ,matrix,ncol=2))
               })
               do.call("rbind",lapply(y,matrix,ncol=2))
            })
            coast_xy <- head(do.call("rbind",lapply(a,matrix,ncol=2)),-1)
            rm(a)
         }
         else if (.lgrep("\\.rds$",obj)) {
            g1 <- session_grid()
            coast_xy <- readRDS(obj)
            if (nchar(g1$proj)) {
              # b <- attributes(coast_xy)
               coast_xy <- .project(coast_xy,g1$proj4)
              # attributes(coast_xy) <- b
            }
         }
         else
            coast_xy <- NULL
      }
      if (!is.null(coast_xy))
      {
         shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
         options(ursaPngShadow=ifelse(shadow %in% c(0,255),"",fill))
         res <- list(coast_xy=coast_xy,panel=panel
                    ,col=col,fill=fill,shadow=shadow,land=land
                    ,density=density,angle=angle,lwd=lwd,lty=lty)
         class(res) <- "ursaCoastLine"
         options(ursaPngCoastLine=res)
         return(res)
      }
   }
   g1 <- session_grid()
   isLongLat <- .lgrep("\\+proj=longlat",g1$proj4)>0
   isMerc <- .lgrep("\\+proj=merc",g1$proj4)>0
   isUTM <- .lgrep("\\+proj=utm",g1$proj4)>0
   proj <- g1$proj4
   proj <- proj[nchar(proj)==max(nchar(proj))]
   if ((any(is.na(proj)))||(nchar(proj)==0))
      return(NULL)
   isDetail <- !is.na(detail)
   if (is.na(detail))
      detail <- "l"
   if (!(detail %in% c("l","i","h","f",NA)))
      message(paste("coastline detail:",detail))
   if ((FALSE)&&(.lgrep("proj=(merc|longlat)",proj))&&((g1$maxx<=20037508))) {
      if (detail %in% c("f"))
         detail <- "h180"
      else
         detail <- paste0(detail,"180")
   }
   if (FALSE)
   {
      g2 <- with(g1,my.expand.grid(x=seq(minx,maxx,length=16)
                                  ,y=seq(miny,maxy,length=16)))
      ll <- .project(with(g2,cbind(x,y)),g1$proj4,inv=TRUE)
      if (all(ll[,2]<65))
         return(NULL)
   }
  # fpath <- Sys.getenv("R_RMAP_TEMPLATE")
   fpath <- getOption("ursaTemplate")
  # if (!nchar(fpath))
  #    fpath <- system.file("template",package="ursa")
   if (!is.na(detail)) {
      fname <- file.path(fpath,paste0("coast-",detail,".rds"))
      if (!file.exists(fname)) {
         detail <- "l"
         fname <- file.path(fpath,paste0("coast-",detail,".rds"))
      }
      xy <- readRDS(fname)
   }
   else
      xy <- readRDS("C:/platt/shapefile/auxiliary/thematicmapping.org/countries.rds")
   coast_xy <- cbind(lon=xy[,1],lat=xy[,2])
   ind <- .grep("^\\d+$",proj)
  # isLoaded <- .lgrep("package:rgdal",search())>0
   isLoaded <- "rgdal" %in% loadedNamespaces()
   if (length(ind))
   {
      proj4 <- "" ## was NA
      try(proj4 <- get(paste0("epsg",proj[ind])))
      if (!nchar(proj4))
      {
         if (!isLoaded)
            requireNamespace("rgdal",quietly=.isPackageInUse())
         proj4 <- rgdal::CRSargs(sp::CRS(sprintf("+init=epsg:%s",proj[ind])))
      }
      else if (!isLoaded) {
         if (!requireNamespace("proj4",quietly=.isPackageInUse()))
            requireNamespace("rgdal")
      }
   }
   else
   {
      if (!isLoaded) {
         if (!requireNamespace("proj4",quietly=.isPackageInUse()))
            requireNamespace("rgdal",quietly=.isPackageInUse())
      }
      proj4 <- paste(proj,collapse=" ")
   }
   if ((!isLongLat)&&(!isMerc)) {
      lat0 <- .gsub("^.*\\+lat_[012]=(\\S+)\\s.*$","\\1",proj4)
      if (lat0==proj4)
         lat0 <- NA
      else
         lat0 <- as.numeric(lat0)
   }
   else
      lat0 <- NA
   ant_xy <- NULL
   ind <- attr(xy,"antarctic")
   if (!is.null(ind))
      indS <- which(abs(coast_xy[ind,2])<(90-1e-3))
   if (!isLongLat) {
      coast_xy <- .project(coast_xy,proj4)
      isInf <- any(is.infinite(coast_xy))
      if (isInf) {
         res <- list()
         class(res) <- "ursaCoastLine"
         options(ursaPngCoastLine=res)
         return(res)
      }
   }
   if (!is.null(ind)) {
      if ((is.na(lat0))||(lat0<=0)) {
         ant_xy <- coast_xy[ind,]
         if (length(indS))
            ant_xy <- ant_xy[indS,]
      }
      ind <- c(head(ind,1)-1,ind)
      coast_xy <- coast_xy[-ind,]
   }
   if (is.na(fail180))
      fail180 <- (isMerc || isLongLat)
   if ((fail180)&&(isLongLat || isMerc)) {
      if (isLongLat)
         B <- 180
      else if (isMerc) {
         B <- .getMajorSemiAxis(proj4)*pi
        # B <- 7720000
         '.shift' <- function(seg) {
            center <- mean(seg[,1])
            j <- which(abs(diff(seg[,1]))>B)
            if (!length(j))
               return(NULL)
            j1 <- c(1,j+1)
            j2 <- c(j,nrow(seg))
            if (center<0)
               k <- which(seg[j1,1]>0.9*B)
            else
               k <- which(seg[j1,1]<=(-0.9*B))
            j1 <- j1[k]
            j2 <- j2[k]
           # print(data.frame(j1=j1,j2=j2,center=center,seg=seg[j1,1]))
            found <- FALSE
            if (center<0) {
               for (m in seq_along(j1))
                  seg[j1[m]:j2[m],1] <- seg[j1[m]:j2[m],1]-2*B
            }
            else {
               for (m in seq_along(j1))
                  seg[j1[m]:j2[m],1] <- seg[j1[m]:j2[m],1]+2*B
            }
           # plot(seg[,1],seg[,2],type="l")
            seg
         }
         if ((TRUE)||(g1$minx<(-B))||(g1$maxx>(+B))) {
            ind <- which(is.na(coast_xy[,1]))
            ind1 <- c(1,ind+1)
            ind2 <- c(ind-1,nrow(coast_xy))
            for (i in seq(length(ind1))) {
               if (ind1[i]>ind2[i])
                  next
               seg <- .shift(coast_xy[ind1[i]:ind2[i],])
               if (is.null(seg))
                  next
              # print(c(i=i,ind1=ind1[i],ind2=ind2[i]))
               coast_xy[ind1[i]:ind2[i],] <- seg
            }
            if (!is.null(ant_xy)) {
               seg <- .shift(ant_xy)
               if (!is.null(seg))
                  ant_xy <- seg
            }
         }
      }
      cond1 <- g1$minx<(-B*(169.2/180)) ## east boarder of Eurasia
      cond2 <- g1$maxx>(+B)
      if (cond1) {
         if (verbose)
            print("expand to the West")
         opp1 <- coast_xy
         opp1[,1] <- opp1[,1]-2*B
         if (!is.null(ant_xy)) {
            if (FALSE) {
               opp1a1 <- opp1a2 <- ant_xy[-nrow(ant_xy),]
               opp1a1[,1] <- opp1a1[,1]-4*B
               opp1a2[,1] <- opp1a2[,1]-2*B
               opp1a <- rbind(opp1a1,c(NA,NA),opp1a2)
               rm(opp1a1,opp1a2)
            }
            else {
               opp1a <- ant_xy[-1,]
               opp1a[,1] <- opp1a[,1]-2*B
            }
         }
      }
      if (cond2) {
         if (verbose)
            print("expand the East")
         opp2 <- coast_xy
         opp2[,1] <- opp2[,1]+2*B
         if (!is.null(ant_xy)) {
            opp2a <- ant_xy[-1,]
            opp2a[,1] <- opp2a[,1]+2*B
         }
      }
      if (cond1) {
         coast_xy <- rbind(coast_xy,c(NA,NA),opp1)
         if (!is.null(ant_xy))
            ant_xy <- rbind(opp1a,ant_xy)
      }
      if (cond2) {
         coast_xy <- rbind(coast_xy,c(NA,NA),opp2)
         if (!is.null(ant_xy))
            ant_xy <- rbind(ant_xy,opp2a)
      }
   }
   if (any(is.na(coast_xy[1,])))
      coast_xy <- coast_xy[-1,]
   n <- nrow(coast_xy)
   if (any(is.na(coast_xy[n,])))
      coast_xy <- coast_xy[-n,]
   if (!is.null(ant_xy)) {
      if ((isLongLat)||(isMerc)) {
         ant1 <- ant_xy[1,]
         ant2 <- ant_xy[nrow(ant_xy),]
         ant1[2] <- g1$miny-g1$resy
         ant2[2] <- ant1[2]
         ant_xy <- rbind(ant1,ant_xy,ant2,ant1)
         rownames(ant_xy) <- NULL
         if (FALSE) { ## non-reproducible code for non-author 
            plot(0,0,xlim=range(c(coast_xy[,1],ant_xy[,1]),na.rm=TRUE),
                    ,ylim=range(c(coast_xy[,2],ant_xy[,2]),na.rm=TRUE),type="n")
            polypath(coast_xy[,1],coast_xy[,2],col="red")
            polypath(ant_xy[,1],ant_xy[,2],col="green")
            stop("")
         }
      }
      coast_xy <- rbind(coast_xy,c(NA,NA),ant_xy)
   }
   if (!isDetail) {
      inside <- with(g1,coast_xy[,1]>=minx & coast_xy[,1]<=maxx &
                            coast_xy[,2]>=miny & coast_xy[,2]<=maxy)
      inside <- any(na.omit(unique(inside)))
      if (inside) {
         area <- with(g1,max(maxx-minx,maxy-miny))
         if (isLongLat)
            area <- area*111
         else
            area <- area/1000
         if (area<=(-10))
            return(NULL)
         else if (area<=100)
            detail <- "f"
         else if (area<=500)
            detail <- "h"
         else if (area<=3000)
            detail <- "i"
         else
            detail <- "l"
         if (detail!="l") {
            fname <- file.path(fpath,paste0("coast-",detail,".rds"))
            if (file.exists(fname)) {
               if (FALSE) {
                  arglist <- list(...)
                  ind <- .grep("detail",names(arglist))
                  if (length(ind))
                     arglist[[ind]] <- detail
                  else
                     arglist$detail <- detail
                  return(do.call("compose_coastline",arglist))
               }
               else {
                  arglist <- as.list(match.call())
                  arglist$detail <- detail
                  return(do.call(as.character(arglist[[1]]),arglist[-1]))
               }
            }
         }
      }
   }
  # ind <- which(coast_xy[,2]<(-68))
  # print(summary(coast_xy[ind,1]))
   shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
   options(ursaPngShadow=ifelse(shadow %in% c(0,255),"",fill))
   res <- list(coast_xy=coast_xy,panel=panel,col=col,fill=fill,shadow=shadow
              ,land=land,density=density,angle=angle,lwd=lwd,lty=lty)
   class(res) <- "ursaCoastLine"
   options(ursaPngCoastLine=res)
   res
}
'panel_coastline' <- function(...) {
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- list(...)
   kwd <- "coast(line)*"
   coastline <- .getPrm(arglist,name=paste0("(",kwd,"|decor)")
                       ,class=list("integer","logical","ursaCoastLine")[1:2]
                       ,default=TRUE)
   ##~ if (inherits(coastline,"ursaCoastLine")) {
      ##~ obj <- coastline
      ##~ coastline <- TRUE
      ##~ isFound <- TRUE
   ##~ }
   ##~ else
      ##~ isFound <- FALSE
   if (!coastline)
      return(NULL)
  # if (!isFound)
   obj <- .getPrm(arglist,class="ursaCoastLine",default=NULL)
   figure <- getOption("ursaPngFigure")
   if ((!is.logical(coastline))&&(figure!=coastline))
      return(NULL)
   if (is.null(obj)) {
      obj <- getOption("ursaPngCoastLine")
      if (is.null(obj))
         obj <- compose_coastline(...)
   }
   if ((any(obj$panel))&&(!(figure %in% obj$panel)))
      return(NULL)
   if (is.null(obj$coast_xy))
      return(NULL)
   if (!FALSE) {
      obj$col <- .getPrm(arglist,name="col",kwd=kwd,default=obj$col)
      obj$fill <- .getPrm(arglist,name="fill",kwd=kwd,default=obj$fill)
      obj$density <- .getPrm(arglist,name="density",kwd=kwd,default=obj$density)
      obj$angle <- .getPrm(arglist,name="angle",kwd=kwd,default=obj$angle)
      obj$land <- .getPrm(arglist,name="land",kwd=kwd,default=obj$land)
      obj$lwd <- .getPrm(arglist,name="lwd",kwd=kwd,default=obj$lwd)
      obj$lty <- .getPrm(arglist,name="lty",kwd=kwd,default=obj$lty)
   }
   verbose <- .getPrm(arglist,name="verbose",kwd=kwd,default=FALSE)
   .panel_coastline(obj,verbose=verbose)
}
'.panel_coastline' <- function(obj,verbose=FALSE) {
   with(obj,{
      shadow <- unname(col2rgb(fill,alpha=TRUE)[4,1])
      if (verbose)
         str(list(col=col,fill=fill,shadow=shadow#,detail=detail
                 ,density=density,angle=angle,land=land,lwd=lwd,lty=lty))
      if ((TRUE)&&(shadow!=255)||
          ((!is.na(angle[1]))&&(!is.na(density[1]))))
      {
        # op <- par(usr=par()$usr-c(0,125000,0,125000))
        # print(par()$usr)
         if ((is.na(angle[1]))||(is.na(density[1]))) {
            if (inherits(coast_xy,"SpatialPolygonsDataFrame"))
               plot(coast_xy,border=col,col=fill,lwd=lwd,add=TRUE)
            else
               polygon(coast_xy[,1],coast_xy[,2],border=col,col=fill,lwd=lwd)
         }
         else {
            for (an in angle) {
              # print(str(list(angle=an,border=col,col=fill,density=density,lwd=lwd)))
               polygon(coast_xy[,1],coast_xy[,2],border=col,col=fill
                      ,density=density,angle=an,lwd=lwd)
            }
         }
        # par(op)
      }
      else
      {
         if (inherits(coast_xy,"SpatialPolygonsDataFrame"))
            plot(coast_xy,border=col,col=fill,lwd=lwd
                ,usePolypath=TRUE,rule=c("winding","evenodd")[2],add=TRUE)
         else {
            if (land)
            {
               g1 <- session_grid()
               x <- with(g1,c(minx,minx,maxx,maxx,minx)+c(-1,-1,1,1,-1)*resx)
               y <- with(g1,c(miny,maxy,maxy,miny,miny)+c(-1,1,1,-1,-1)*resy)
               coast_xy <- rbind(cbind(x,y),c(NA,NA),coast_xy)
            }
           ## ?polypath: Hatched shading (as implemented for polygon()) is not (currently) supported.
           ## if semi-opacity|trasparency them 'polygon' else fill is transparent
            polypath(coast_xy[,1],coast_xy[,2],border=col,col=fill
                    ,rule=c("winding","evenodd")[2],lwd=lwd) ##,density=15??
         }
      }
   })
   invisible(NULL)
}
