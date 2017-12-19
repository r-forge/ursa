# wrappers to spatial (not raster) objects
'spatial_crs' <- 'spatial_proj4'<- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_crs(obj)$proj4string)
   }
   if (isSP) {
      return(sp::proj4string(obj))
   }
   return(NULL)
}
'spatial_geometry' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_geometry(obj))
   }
   if (isSP) {
      return(sp::geometry(obj))
   }
   return(NULL)
}
'spatial_bbox' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- sf::st_bbox(obj)
      rname <- names(res)
      res <- as.numeric(res)
      names(res) <- rname
      return(res)
   }
   if (isSP) {
      res <- c(sp::bbox(obj))
      names(res) <- c("xmin","ymin","xmax","ymax")
      return(res)
   }
   return(NULL)
}
'spatial_engine' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return("sf")
   }
   if (isSP) {
      return("sp")
   }
   return(NULL)
}
'spatial_data' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      res <- obj
      sf::st_geometry(res) <- NULL
      return(res)
   }
   if (isSP) {
      return(res@data)
   }
   return(NULL)
}
'spatial_transform' <- function(obj,crs,verbose=FALSE,...) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      return(sf::st_transform(obj,crs,...))
   }
   if (isSP) {
      if (is.numeric(crs))
         crs <- .epsg2proj4(crs,force=FALSE)
      return(sp::spTransform(obj,crs,...)) ## sp::CRS(crs) ?
   }
   return(NULL)
}
