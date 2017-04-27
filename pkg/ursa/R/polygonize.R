'polygonize' <- function(obj,fname,verbose=TRUE,...) {
   isList <- .is.ursa_stack(obj)
   if ((!isList)&&(!is.ursa(obj)))
      return(NULL)
  # requireNamespace("sp",quietly=.isPackageInUse())
  # requireNamespace("methods",quietly=.isPackageInUse())
   g1 <- if (isList) ursa_grid(obj[[1]]) else ursa_grid(obj)
   prj <- ursa_proj(g1)
   if (isList) {
      for (i in seq_along(obj)) {
         b0 <- as.data.frame(obj[[i]])
         b <- if (i==1) b0 else cbind(b,b0[,3:ncol(b0),drop=FALSE])
      }
   }
   else
      b <- as.data.frame(obj)
   n <- nrow(b)
   sa <- vector("list",n)
   dx <- g1$resx/2
   dy <- g1$resy/2
   if ((FALSE)&&(requireNamespace("sf",quietly=.isPackageInUse()))) { ## dev with 'sf' functions
     # sa <- vector("list",nrow(b))
      .elapsedTime("A")
     # x <- matrix(rep(b$x,5),ncol=5)+c(-dx,-dx,+dx,+dx,-dx)
     # y <- matrix(rep(b$y,5),ncol=5)+c(-dy,+dy,+dy,-dy,-dy)
     # print(head(x))
     # str(x)
     # str(y)
      sa <- lapply(seq(n),function(i){
         x <- unname(b$x[i]+c(-dx,-dx,+dx,+dx,-dx))
         y <- unname(b$y[i]+c(-dy,+dy,+dy,-dy,-dy))
         cbind(x,y)
      })
      .elapsedTime("B")
      sa <- sf::st_polygon(sa)
      .elapsedTime("C")
      sa <- sf::st_as_sf(b[,3:ncol(b),drop=FALSE],coords=sa)
      .elapsedTime("D")
      str(head(sa))
      q()
      x1 <- b$x-dx
      x2 <- b$x+dx
      y1 <- b$y-dy
      y2 <- b$y+dy
   }
   if (verbose) {
      pb <- ursaProgressBar(min=0,max=n)
      i <- -1
      repeat({
         n1 <- n*(10^i)
         if (n1<100)
            break
         i <- i-1
      })
      n1 <- 10^(-i-1)
   }
   for (i in seq(n))
   {
      x <- b$x[i]+c(-dx,-dx,+dx,+dx,-dx)
      y <- b$y[i]+c(-dy,+dy,+dy,-dy,-dy)
      sa[[i]] <- sp::Polygons(list(sp::Polygon(cbind(x,y))),i)
      if ((verbose)&&(i%%n1==0))
         setUrsaProgressBar(pb,i)
   }
   if (verbose)
      close(pb)
   sa <- sp::SpatialPolygons(sa,proj4string=sp::CRS(prj))
   sa <- sp::SpatialPolygonsDataFrame(sa,data=b[,3:ncol(b),drop=FALSE]
                                     ,match.ID=FALSE)
   if (!missing(fname))
      return(.shp.write(sa,fname,...))
   sa
}
'.vectorize' <- function(obj,fname,opt="") {
   Fout <- .maketmp()
   write_envi(obj,paste0(Fout,"."))
   cmd <- paste("python",Sys.which("gdal_polygonize.py")
               ,opt," -f \"ESRI Shapefile\"",Fout,".",fname)
   system(cmd)
   envi_remove(Fout)
   0L
}
