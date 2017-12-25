# wrappers to spatial (not raster) objects
'spatial_crs' <- 'spatial_proj4' <- function(obj,verbose=FALSE) {
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
'spatial_crs<-' <- 'spatial_proj4<-' <- function(obj,verbose=FALSE,value) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      sf::st_crs(obj) <- sf::NA_crs_
      sf::st_crs(obj) <- value
   }
   if (isSP) {
      if (is.numeric(value))
         value <- .epsg2proj4(value,force=FALSE)
      sp::proj4string(obj) <- NA_character_
      sp::proj4string(obj) <- value
   }
   obj
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
'spatial_fields' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      dname <- try(names(sf::st_agr(obj)),silent=TRUE)
      if (inherits(dname,"try-error"))
         dname <- character()
      return(dname)
   }
   if (isSP) {
      dname <- try(colnames(obj@data),silent=TRUE)
      if (inherits(dname,"try-error"))
         dname <- character()
      return(dname)
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
      attributes(res) <- attributes(res)[c("names","row.names","class")]
      return(res)
   }
   if (isSP) {
      return(obj@data)
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
'spatial_geotype' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF) {
      if (inherits(obj,"sfc"))
         geoType <- .grep("^sfc_.+$",class(obj),value=TRUE)
      else
         geoType <- .grep("^sfc_.+$",class(obj[[attr(obj,"sf_column")]]),value=TRUE)
      geoType <- .gsub("^sfc_","",geoType)
      if (geoType=="GEOMETRY")
         geoType <- unique(as.character(sf::st_geometry_type(obj)))
      return(geoType)
   }
   if (isSP) {
      geoType <- switch(class(sp::geometry(obj))
                       ,SpatialPolygons="POLYGON"
                       ,SpatialPoints="POINT"
                       ,SpatialLines="LINESTRING")
      return(geoType)
   }
   return(NULL)
}
'spatial_coordinates' <- function(obj,verbose=FALSE) {
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- inherits(obj,"Spatial")
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   geoType <- spatial_geotype(obj)
   if (isSF) {
      if (length(geoType)>1) {
         if (("POLYGON" %in% geoType)&&("MULTIPOLYGON" %in% geoType)) {
            obj <- sf::st_cast(obj,"MULTIPOLYGON")
            geoType <- "MULTIPOLYGON"
         }
         else
            stop(paste("Unimplemented for multiple geometries (sf): "
                      ,paste(geoType,collapse=", ")))
      }
      if (geoType=="POINT") {
        # ret <- do.call("rbind",lapply(sf::st_geometry(obj),unclass))
         ret <- sapply(sf::st_geometry(obj),unclass)
         rownames(ret) <- seq(nrow(ret))
         return(ret)
      }
      if (geoType=="LINESTRING") {
         multi <- any(sapply(sf::st_geometry(obj),is.list))
         if (!multi) {
            ret <- lapply(sf::st_geometry(obj),unclass)
            names(ret) <- seq_along(ret)
            return(ret)
         }
         else
            stop(paste("Unimplemented MULTILINESTRING (sf)"))
      }
      if (geoType=="MULTIPOLYGON") {
         ret <- lapply(sf::st_geometry(obj),unclass)
         names(ret) <- seq_along(ret)
         return(ret)
      }
      if (geoType=="POLYGON") {
         ret <- lapply(sf::st_geometry(obj),unclass)
         names(ret) <- seq_along(ret)
         return(ret)
      }
     # ret <- lapply(sf::st_geometry(obj),unclass) ## dummy
      stop(paste("Unimplemented for geometry (sf): "
                ,paste(geoType,collapse=", ")))
   }
   if (isSP) {
      if (geoType=="POINT") {
         ret <- unname(sp::coordinates(obj))
         rownames(ret) <- seq(nrow(ret))
         return(ret)
      }
      if (geoType=="LINESTRING") {
         ulen <- sort(unique(sapply(obj@lines,function(x) length(x@Lines))))
         if ((length(ulen)==1)&&(ulen==1)) {
            ret <- lapply(obj@lines,function(x) {
               x2 <- lapply(x@Lines,sp::coordinates)[[1]]
            })
            names(ret) <- seq_along(ret)
            return(ret)
         }
         else 
            stop(paste("Unimplemented MULTILINESTRING (sp)"))
      }
      if (geoType=="POLYGON") {
         ulen <- sort(unique(sapply(obj@polygons,function(x) length(x@Polygons))))
        # ret <- vector("list",length(ulen))
         ret <- lapply(obj@polygons,function(x1) {
            x2 <- x1@Polygons
            hole <- sapply(x2,function(x3) x3@hole)
            if (TRUE) {# if (any(hole)) {
               indF <- which(!hole)
               ret2 <- vector("list",length(indF))
               jF <- 1L
               prevHole <- TRUE
               for (i in seq_along(hole)) {
                  if (!hole[i]) {
                     if (i==length(hole))
                        h2 <- 0
                     else {
                        h1 <- i+1
                        hole2 <- hole[h1:length(hole)]
                        if (hole2[1]) {
                           indE <- which(diff(hole2)!=0)
                           if (!length(indE))
                              h2 <- length(hole2)
                           else
                              h2 <- indE[1]
                           print(h2)
                        }
                        else
                           h2 <- 0
                     }
                     if (!h2) {
                        ret2[[jF]] <- list(sp::coordinates(x2[[i]]))
                        jF <- jF+1L
                        next
                     }
                     h2 <- h2+1
                     ret3 <- vector("list",h2)
                     ret3[[1]] <- sp::coordinates(x2[[i]])
                     jH <- 2L
                  }
                  else {
                     ret3[[jH]] <- sp::coordinates(x2[[i]])
                     jH <- jH+1L
                     if (jH>h2) {
                        ret2[[jF]] <-ret3
                        jF <- jF+1L
                     }
                  }
               }
               ret2
            }
            else 
               stop(paste("Unimplemented POLYGON (sp) without holes"))
         })
         names(ret) <- seq_along(ret)
         return(ret)
      }
      stop(paste("Unimplemented for geometry (sp): "
                ,paste(geoType,collapse=", ")))
   }
   return(NULL)
}
