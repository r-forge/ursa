'.project' <- function(xy,proj,inv=FALSE) {
   ## because of quick load ot 'proj4' package
   opW <- options(warn=-11);on.exit(options(opW))
  # if (("package:rgdal" %in% search())||
  #     (!requireNamespace("proj4",quietly=TRUE)))
  #    res <- rgdal::project(xy=xy,proj=proj,inv=inv)
  # else
  #    res <- proj4::project(xy=t(xy),proj=proj,inverse=inv)
   a <- FALSE
  # print(summary(xy))
  # proj4 <- requireNamespace("proj4",quietly=TRUE)
  # print(proj4)
   if ((!FALSE)&&(!("package:rgdal" %in% search()))&&
       (requireNamespace("proj4",quietly=FALSE))) {
      a <- .try({
        ## suppressMessages(require(proj4)) ## uncomment?
         res <- proj4::project(xy=t(xy),proj=proj,inverse=inv)
      },silent=TRUE)
      if ((!FALSE)&&(!a)&&(nrow(xy)==2)) {
         a <- .try({
           ## suppressMessages(require(proj4)) ## uncomment?
            res <- proj4::project(xy=xy,proj=proj,inverse=inv)
         },silent=TRUE)
      }
   }
   if (!a) {
      requireNamespace("rgdal",quietly=FALSE)
      if (is.list(xy))
         xy <- cbind(xy[[1]],xy[[2]])
      else if (!is.matrix(xy))
         xy <- matrix(xy,ncol=2)
     # ind <- which((is.na(xy[,1]))|(is.na(xy[,2])))
      ind <- which(is.na(xy[,1])) ## less conditions
      if (length(ind)) {
         res <- matrix(NA,ncol=2,nrow=nrow(xy))
         a <- .try(res[-ind,] <- rgdal::project(xy=xy[-ind,],proj=proj,inv=inv))
      }
      else
         a <- .try(res <- rgdal::project(xy=xy,proj=proj,inv=inv))
      if (!a) {
         .epsg3411 <- paste("","+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1"
                           ,"+x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
         if (requireNamespace("proj4",quietly=TRUE)) {
            res <- proj4::project(xy=xy,proj=.epsg3411)
            res <- proj4::ptransform(res,.epsg3411,proj)
            return(res)
         }
      }
   }
   if ((FALSE)&&(!inv)&&(.lgrep("\\+proj=merc",g1$proj))) {
      g1 <- session_grid()
      ext <- 20037508
      if (g1$maxx>ext) {
         ind <- which(res[,1]<0)
        # print("before:")
        # print(res)
         if (length(ind))
            res[ind,1] <- 2*ext+res[ind,1]
        # print("after:")
        # print(res)
      }
   }
   return(res)
}
