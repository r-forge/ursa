'.rasterize' <- function(obj,...) {
   if (!nchar(Sys.which("gdal_rasterize")))
      return(NULL)
   arglist <- list(...)
   verbose <- .getPrm(arglist,name="verb(ose)*",default=FALSE)
  # obj <- .getPrm(arglist,name=".*",class=list("character","ursaVectorExternal"))
   external <- is.character(obj)
   if (external) {
      dsnE <- obj
      if (verbose)
         .elapsedTime("read vector -- start")
      obj <- .spatialize(obj,...)
      if (verbose)
         .elapsedTime("read vector -- finish")
   }
   if (external)
      dsn <- dsnE
   else
      dsn <- attr(obj,"dsn")
  # print(c(dsn=dsn,dsnE=dsnE))
   g0 <- attr(obj,"grid")
   dname <- attr(obj,"colnames")
  # attr <- .getPrm(arglist,name="attr",default=".+")
   feature <- .getPrm(arglist,name="feature",valid=c("attribute","geometry"))
   where <- .getPrm(arglist,name="subset",default="")
   ogropt <- .getPrm(arglist,name="ogropt",default="")
   isZip <- .lgrep("\\.zip$",dsn)>0
   fname1 <- .gsub("\\.zip$","",dsn)
   fname2 <- paste0(fname1,".zip")
   fname3 <- gsub("(\\..+$)",".zip",fname1)
   cond1 <- file.exists(fname1)
   cond2 <- file.exists(fname2)
   cond3 <- file.exists(fname3)
  # print(dsn)
  # print(c(fname1=fname1,fname2=fname2,fname3=fname3))
  # print(c(isZip=isZip,cond1=cond1,cond2=cond2,cond3=cond3))
   on.exit(NULL)
   if (cond1)
      dsn <- fname1
   else if (cond2 | cond3) {
      if (cond2)
         ziplist <- unzip(fname2,exdir=tempdir())
      if (cond3)
         ziplist <- unzip(fname3,exdir=tempdir())
      on.exit(file.remove(ziplist),add=TRUE)
      dsn <- .grep("\\.shp$",ziplist,value=TRUE)
   }
   else
      dsn <- NA
   isSF <- inherits(obj,"sf")
   isSP <- !isSF
  # print(dsn)
  # str(obj)
   if ((nchar(ogropt)>1)||(nchar(where)>0)) {
      shpname <- .maketmp()
      cmd <- paste("ogr2ogr"
              ,ogropt 
              ,"-where",dQuote(where)
              ,"-f",.dQuote(c("ESRI Shapefile","SQLite","GeoJSON")[1])
             # ,ifelse(verbose,"-progress","")
              ,.dQuote(paste0(shpname,".shp")),.dQuote(dsn)
              )
      if (verbose)
         message(cmd)
      system(cmd)
      list3 <- .dir(path=dirname(shpname)
                   ,pattern=paste0(basename(shpname),"\\.(cpg|dbf|prj|shp|shx)")
                   ,recursive=FALSE,full.names=TRUE)
      on.exit(file.remove(list3),add=TRUE)
      lname <- basename(shpname)
      dsn <- .grep("\\.shp$",list3,value=TRUE)
   }
   cmd <- paste("gdalsrsinfo -o proj4",.dQuote(dsn))
   if (verbose)
      message(cmd)
   proj4 <- system(cmd,intern=TRUE)
   proj4 <- .gsub("'","",proj4)
   proj4 <- .gsub("(^\\s|\\s$)","",proj4)
   ftemp <- .maketmp() # .maketmp() #tempfile(pattern="") # ".\\tmp1"
   cmd <- paste("ogrinfo","-q",.dQuote(dsn))
   if (verbose)
      message(cmd)
   lname <- system(cmd,intern=TRUE)
   lname <- .gsub("(\\s\\(.+\\))*$","",lname)
   lname <- .gsub("^\\d+:\\s(.+)$","\\1",lname)
   if (proj4!=g0$proj4) {
     # if (verbose)
     #   message("REPROJECT")
      shpname <- .maketmp()
      cmd <- paste("ogr2ogr","-t_srs",.dQuote(g0$proj4)
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
              ,"-select FID"
              ,"-f",.dQuote(c("ESRI Shapefile","SQLite")[1])
              ,ifelse(verbose,"-progress","")
              ,.dQuote(paste0(shpname,".shp")),.dQuote(dsn)
              )
      if (verbose)
         message(cmd)
     # .elapsedTime("a")
      system(cmd)
     # .elapsedTime("b")
      list2 <- .dir(path=dirname(shpname)
                   ,pattern=paste0(basename(shpname),"\\.(cpg|dbf|prj|shp|shx)")
                   ,recursive=FALSE,full.names=TRUE)
      on.exit(file.remove(list2),add=TRUE)
      lname <- basename(shpname)
      shpname <- .grep("\\.shp$",list2,value=TRUE)
   }
   else
      shpname <- dsn
   if (is.null(dname)) {
      md <- system(paste("ogrinfo -al -so",dsn),intern=TRUE)
      patt <- "^(.+): \\S+ \\(.+\\)$"
      md <- .grep(patt,md,value=TRUE)
      dname <- .grep(attr,.gsub(patt,"\\1",md),value=TRUE)
   }
   if (feature=="attribute") {
     # opt <- paste()
      cmd <- with(g0,paste("gdal_rasterize"
              ,"-a FID"
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
              ,"-init -1 -a_nodata -1"
              ,"-a_srs",.dQuote(proj4)
              ,"-tr",resx,resy
             # ,"-where",dQuote(subset)
              ,"-te",minx,miny,maxx,maxy
              ,"-of ENVI -ot Int32",ifelse(verbose,"","-q")
              ,.dQuote(shpname),ftemp))
      
      if (verbose) {
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- start"))
         message(cmd)
      }
      system(cmd)
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- finish"))
      va <- read_envi(ftemp,resetGrid=TRUE)
      envi_remove(ftemp)
      if (global_min(va)==0)
         va <- va+1L
      va[va==0] <- NA
      res <- lapply(dname,function(x){
         a <- reclass(va,src=seq(nrow(obj)),dst=obj[[x]])
         if (.is.category(a))
            ursa(a,"nodata") <- length(ursa(a,"colortable"))
         names(a) <- x
         a
      })
     # names(res) <- dname
   }
   else if (feature=="geometry") {
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- start"))
      nr <- nrow(obj)
      res <- lapply(seq(nr),function(i){
         cmd <- with(g0,paste("gdal_rasterize"
              ,"-a FID"
              ,"-sql",.dQuote(paste("select FID,* from",.dQuote(.dQuote(lname))
                             ,"where",paste0(.dQuote("FID"),"=",i)))
              ,"-dialect",c("SQLITE","OGRSQL")[2]
             # ,"-where",paste0(.dQuote("FID"),"=",i)
              ,"-tr",resx,resy
              ,"-te",minx,miny,maxx,maxy
              ,"-of ENVI -ot Int16",ifelse(verbose,"","-q")
              ,shpname,ftemp))
         if (verbose)
            message(cmd)
         system(cmd)
         va <- read_envi(ftemp)
         envi_remove(ftemp)
         va[va==0] <- NA
         va
      })
      if (verbose)
         .elapsedTime(paste("rasterize",.sQuote(feature),"-- finish"))
   }
   res
}
'.gdalwarp' <- function(src,dst=NULL,grid=NULL,resample="near",nodata=NA
                       ,resetGrid=FALSE,verbose=1L) {
   if (is.null(grid)) {
      grid <- getOption("ursaSessionGrid")
   }
   else
      grid <- ursa_grid(grid)
  if (!nchar(Sys.which("gdalwarp"))) {
      withRaster <- requireNamespace("raster",quietly=.isPackageInUse())
      if (withRaster) {
         r1 <- as.Raster(src)
         session_grid(grid)
         r2 <- as.Raster(ursa_new(0L))
         r3 <- try(raster::resample(r1,r2,method=c("bilinear","ngb")[1]))
         if (inherits(r3,"try-error")) {
            if (verbose)
               message('reprojection is failed')
            return(src)
         }
      }
      else if (verbose)
         message(paste("'gdalwarp' is not found; package 'raster' is not found."
                      ,"Reprojection is failed."))
      return(src)
  }
  # a <- open_envi(src)
  # ct <- ursa_colortable(a)
  # close(a)
   if (is.ursa(src)) {
      removeSrc <- TRUE
      .src <- src
      nodata <- ignorevalue(src)
      src <- .maketmp(ext=".")
      write_envi(.src,src)
   }
   else {
      removeSrc <- FALSE
     # nodata <- NA
   }
   inMemory <- is.null(dst)
   if (inMemory)
      dst <- .maketmp(ext=".")
   if (verbose)
      print(c(inMemory=inMemory,removeSrc=removeSrc,isNullGrid=is.null(grid)))
   if (is.null(grid))
      cmd <- paste("gdalwarp -overwrite -of ENVI"
                  ,ifelse(is.na(nodata),"",paste("-srcnodata",nodata,"-dstnodata",nodata))
                  ,"-r",resample,src,dst)
   else
      cmd <- with(grid,paste("gdalwarp -overwrite -of ENVI"
                        ,"-t_srs",.dQuote(proj4),"-tr",resx,resy,"-te",minx,miny,maxx,maxy
                        ,ifelse(is.na(nodata),"",paste("-srcnodata",nodata,"-dstnodata",nodata))
                        ,"-r",resample,src,dst))
   if (verbose)
      message(cmd)
   if (verbose>1)
      return(NULL)
   system(cmd)
   session_grid(NULL)
   if (inMemory)
      ret <- read_envi(dst)
   else
      ret <- open_envi(dst)
   if (!is.na(nodata)) {
      ignorevalue(ret) <- nodata
      if (inMemory)
         ret[ret==nodata] <- NA
   }
   if (inMemory)
      envi_remove(dst)
   if (removeSrc)
      envi_remove(src)
   if (resetGrid)
      session_grid(ret)
   ret
}
