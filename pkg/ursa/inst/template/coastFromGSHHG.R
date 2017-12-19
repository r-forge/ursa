require(rgdal)
require(ursa)
# require(plutil)
'.is180' <- function(x) abs(abs(x)-180)<1e-9 | abs(abs(x)-0)<1e-9
'.merge' <- function(b,ind,pole=FALSE)
{
  # writeOGR(b[ind[1],],".","ant1",overwrite=TRUE,driver="ESRI Shapefile")
  # writeOGR(b[ind[2],],".","ant2",overwrite=TRUE,driver="ESRI Shapefile")
   xy1 <- b@polygons[[ind[1]]]@Polygons[[1]]@coords[-1,]
   xy2 <- b@polygons[[ind[2]]]@Polygons[[1]]@coords[-1,]
   if (FALSE) {
      xy1 <- rbind(head(xy1),tail(xy1))
      xy2 <- rbind(head(xy2),tail(xy2))
      dimnames(xy1) <- NULL
      dimnames(xy2) <- NULL
   }
   if (!pole) {
      ind2 <- which(xy2[,1]<=0)
      if (length(ind2))
         xy2[ind2,1] <- xy2[ind2,1]+360
   }
  # print(xy1)
  # print(xy2)
  # str(xy1)
  # str(xy2)
   f1 <- which(.is180(xy1[,1]))
   f2 <- which(.is180(xy2[,1]))
   f3 <- NULL
   for (i1 in f1) {
      d1 <- abs(xy1[i1,2]-xy2[f2,2])
      d2 <- abs(xy1[i1,1]-xy2[f2,1])
      d <- which(d1==min(d1) & d2==0)
      if (length(d))
         f3 <- rbind(f3,c(i1,f2[d]))
   }
  # print(xy1[f1,])
  # print(xy2[f2,])
  # print(f3)
   f1 <- f3[,1]
   f2 <- f3[,2]
   ef1 <- c(1,f1,nrow(xy1))
   ef2 <- c(1,f2,nrow(xy2))
   if ((f2[1]==-1)&&(f2[2]==nrow(xy)))
      xy <- rbind(xy1[1:f1[1],]
                 ,xy2[f2[1]:f2[2],]
                 ,xy1[f1[2]:nrow(xy1),]
                 )
   else {
      xy <- NULL
      xy <- rbind(xy,xy1[1:f1[1],])
      ##~ if ((f2[1]<f2[2])||(f2[2]<nrow(xy2))) {
      ##~ if ((f2[1]<nrow(xy2))&&(f2[2]<nrow(xy2))) { ## inverse
      if (f2[2]-f2[1]==1) { ## inverse
         xy <- rbind(xy,xy2[f2[1]:1,])
         xy <- rbind(xy,xy2[nrow(xy2):f2[2],])
      }
      else if (f2[1]-f2[2]==1){
         xy <- rbind(xy,xy2[f2[1]:nrow(xy2),])
         xy <- rbind(xy,xy2[1:f2[2],])
      }
      else
         xy <- rbind(xy,xy2[f2[1]:f2[2],])
      if (f1[2]==nrow(xy1))
         xy <- rbind(xy,xy1[nrow(xy1):f1[1],])
      else
         xy <- rbind(xy,xy1[f1[2]:nrow(xy1),])
   }
  # str(xy)
  # print(xy1)
  # print(xy2)
  # print(xy)
   xy <- unique(xy)
   xy <- rbind(xy,xy[1,])
   if (pole) {
      p <- 90-1e-6
      ind3 <- which(abs(xy[,2])>p)
      if (length(ind3))
         xy[ind3,2] <- p*sign(xy[ind3,2])
   }
  # print(xy)
  # q()
   df <- b@data[ind[1],]
   df$id <- ursa:::.gsub("\\D","",df$id)
   sa <- Polygons(list(Polygon(xy)),paste0("-",df$id))
   sa <- SpatialPolygons(list(sa),proj4string=CRS(proj4string(b)))
   sa <- SpatialPolygonsDataFrame(sa,data=df,match.ID=FALSE)
  # writeOGR(sa,".","ant",overwrite=TRUE,driver="ESRI Shapefile")
  # q()
   sa
}
'.append' <- function(b1,b2)
{
   b2@polygons <- lapply(b2@polygons,function(b){
      b@ID <- paste0("-",b@ID)
      b
   })
   df3 <- rbind(b1@data,b2@data)
   sa <- c(b1@polygons,b2@polygons)
   sa <- SpatialPolygons(sa,proj4string=CRS(proj4string(b1)))
   sa <- SpatialPolygonsDataFrame(sa,data=df3,match.ID=FALSE)
   sa
}
'.rebuild' <- function(b,id,pole=FALSE) {
   ind <- na.omit(which(b@data$id %in% id))
   if (!length(ind))
      return(b)
   b1 <- b[-ind,]
   b2 <- .merge(b,ind,pole=pole)
   b <- .append(b1,b2)
  # writeOGR(b2,".","ant",overwrite=TRUE,driver="ESRI Shapefile")
   b
}
invisible({
   epsg3411 <- paste("","+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1"
                    ,"+x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
   proj <- epsg3411
   res <- c("l","i","h","f")[1]
   fname1 <- paste0("GSHHS_",res,"_L1")
   fname5 <- paste0("GSHHS_",res,"_L5")
   isMerge <- TRUE
   op <- options(warn=-1)
   b <- readOGR(file.path("C:/tmp/GSHHS_shp",res),fname1,useC=!TRUE)
   b5 <- readOGR(file.path("C:/tmp/GSHHS_shp",res),fname5,useC=!TRUE)
   b <- .append(b,b5)
   rm(b5)
   options(op)
   ursa:::.elapsedTime("shapefiles have been read")
   if (!FALSE)
   {
      north <- switch(res,l=-90,i=50,h=50,f=60,-90)
      ind <- which(sapply(b@polygons,function(x)
         any(x@Polygons[[1]]@coords[,2]>=north)
      ))
      b <- b[ind,]
      n <- switch(res,l=1,i=2,h=8,f=32)*3*180+1
      lon <- seq(-180,360,length=n)
      lon[n] <- lon[1]
      rm(n)
      b@polygons <- sapply(b@polygons,function(x){
         xy <- x@Polygons[[1]]@coords
         ind <- as.integer(xy[,2]<north)
         if (all(ind==0))
         {
            stopifnot(!any(xy[,2]<north))
            return(x)
         }
        # if (nrow(xy)>1000)
        #    return(x)
         xy <- xy[-nrow(xy),]
         ind3 <- which(diff(ind)<0)
         ind3 <- ind3[length(ind3)]
         if (ind3<nrow(xy))
            xy <- xy[c((ind3+1):nrow(xy),1:(ind3+0)),]
        # xy <- rbind(xy,xy[1,])
         ind <- as.integer(xy[,2]>=north)
         ind1 <- which(diff(ind)>0)+1L
         ind2 <- which(diff(ind)<0)
         ind1 <- c(1L,ind1)
         stopifnot(length(ind1)==length(ind2))
         sa <- vector("list",length(ind1))
         for (i in seq_along(ind1))
         {
            i1 <- ind1[i]
            i2 <- ind2[i]
            ind5 <- c(i1-1,i1:i2,i2+1)
            if (ind5[1]<1)
               ind5[1] <- nrow(xy)
           # print(ind5[c(1,2,length(ind5)-1,length(ind5))])
            xy2 <- xy5 <- xy[ind5,]
            xy2[c(1,length(ind5)),2] <- north
            if (length(which(xy2[,2]<north)))
            {
               print(ind)
               print(ind1)
               print(ind2)
               print(i)
               print(xy[i1:i2])
               print(xy5)
               print(xy2)
               q()
            }
            xy3 <- xy2[c(1,length(ind5)),]
            lon2 <- xy3[,1]
            d <- abs(sin(-diff(lon2*pi/180)))
            if (d>0.05)
            {
               lon2[lon2>180] <- lon2[lon2>180]-360
               lon3 <- lon[which(lon>=min(lon2) & lon<=max(lon2))]
               if (lon2[1]>lon2[2])
                  lon3 <- rev(lon3)
               lon4 <- c(lon2[1],lon3,lon2[2])
               xy4 <- cbind(x=rev(lon4),y=rep(north,length(lon4)))
               xy2 <- rbind(xy2[1:nrow(xy2),],xy4)
            }
           # stopifnot(!any(xy2[,2]<north))
            sa[[i]] <- Polygon(xy2)
         }
         x@Polygons <- sa
         x
      })
   }
   if (isMerge) {
      b <- .rebuild(b,c("97-E","97-W"))
      ursa:::.gc(!TRUE)
      b <- .rebuild(b,c("112-E","112-W"))
      ursa:::.gc(!TRUE)
      b <- .rebuild(b,c("525-E","525-W"))
      ursa:::.gc(!TRUE)
      b <- .rebuild(b,c("1716-E","1716-W"))
      ursa:::.gc(!TRUE)
      b <- .rebuild(b,c("0-E","0-W"))
      ursa:::.gc(!TRUE)
      b <- .rebuild(b,c("4-E","4-W"),pole=TRUE)
      ursa:::.gc(!TRUE)
   }
   Fout <- fname1
   if (!isMerge)
      Fout <- paste0(Fout,"_180")
   xy <- NULL
   if ((!FALSE)&&(isMerge)) {
      id <- ursa:::.grep("\\D",b@data$id,value=TRUE)
      if (length(id)) {
         print(id)
         stop("Non-merged polygons")
      }
      b@data <- data.frame(ID=as.integer(b@data$id))
   }
   else
      b@data <- data.frame(ID=b@data$id)
   b1 <- b@polygons
   isATA <- FALSE
   for (i1 in seq_along(b1))
   {
      found <- b@data$ID[i1]==4
      b2 <- b1[[i1]]@Polygons
      if (found) {
         m1 <- nrow(xy)+1L
         isATA <- TRUE
      }
      for (i2 in seq_along(b2)) {
         crd <- b2[[i2]]@coords
         if (found)
            str(crd)
         xy <- rbind(xy,crd,c(NA,NA))
      }
      if (found) {
         m2 <- nrow(xy)-1L
         print(c(m1=m1,m2=m2))
      }
   }
   xy <- xy[-nrow(xy),]
   colnames(xy) <- c("lon","lat")
   if (isATA) {
     # xy0 <- list(world=xy[-c((m1-1L):m2),],ATA=xy[m1:(m2-1L),])
   }
   print(summary(xy))
  # print(head(xy))
  # print(tail(xy))
  # print(xy[c(62370:62380,63520:nrow(xy)),])
   ind <- which(xy[,2]<(-90+1e-3))
   attr(xy,"south_pole") <- ind
   if (isATA)
      attr(xy,"antarctic") <- m1:m2
   str(xy)
   saveRDS(xy,paste0("coast-",res,ifelse(isMerge,"","180"),"-updated.rds"))
  # if (isMerge)
  #    b <- spTransform(b,CRS(proj))
  # writeOGR(b,".",Fout,overwrite=TRUE,driver="ESRI Shapefile")
   ursa:::.elapsedTime("completed!")
  # msgShow()
  # file.remove(filelist("^tmp.+"))
})
