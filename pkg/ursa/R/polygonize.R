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
   da <- b[,3:ncol(b),drop=FALSE]
   n <- nrow(b)
   sa <- vector("list",n)
   dx <- g1$resx/2
   dy <- g1$resy/2
   if (verbose)
      pb <- ursaProgressBar(min=0,max=n)
   for (i in seq(n))
   {
      x <- b$x[i]+c(-dx,-dx,+dx,+dx,-dx)
      y <- b$y[i]+c(-dy,+dy,+dy,-dy,-dy)
      sa[[i]] <- sp::Polygons(list(sp::Polygon(cbind(x,y))),i)
      if (verbose)
         setUrsaProgressBar(pb,i)
   }
   if (verbose)
      close(pb)
   sa <- sp::SpatialPolygons(sa,proj4string=sp::CRS(prj))
   sa <- sp::SpatialPolygonsDataFrame(sa,data=da,match.ID=FALSE)
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
