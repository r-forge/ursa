'.write_spatial' <- 
'.write_ogr' <- function(obj,fname,driver=NA,compress=""
                        ,zip=NULL,ogr2ogr=nchar(Sys.which("ogr2ogr"))>0
                        ,transform=NA,verbose=FALSE) {
  # obj <- head(obj,100)
   bname <- basename(fname)
   dname <- dirname(fname)
   fext <- .gsub("^.+\\.(.+)$","\\1",bname)
   wait <- 60
   interimExt <- c("gpkg","geojson","shp")[3]
   driverList <- c(shp="ESRI Shapefile"
                  ,sqlite="SQLite",json="GeoJSON",geojson="GeoJSON",gpkg="GPKG"
                  ,tab="Mapinfo File",kml="KML")
   if (!nchar(compress)) {
      packPatt <- "^(zip|bz(ip)*2|gz(ip)*|xz)$"
      if (.lgrep(packPatt,fext)>0) {
         compress <- .gsub(packPatt,"\\1",fext)
         bname <- gsub(paste0("\\.",compress),"",bname)
         fext <- .gsub("^(.+)\\.(.+)$","\\2",bname)
         fname <- file.path(dname,bname)
      }
   }
   lname <- .gsub("^(.+)\\.(.+)$","\\1",bname)
   ext <- switch(driver,'ESRI Shapefile'="(cpg|dbf|prj|qpj|shp|shx)"
                       ,'MapInfo File'="(mif|mid)",fext)
   isList <- is.list(obj)
   if (isList) {
      cl <- sapply(obj,inherits,c("sf","sfc","SpatialLinesDataFrame"
                          ,"SpatialPointsDataFrame","SpatialPolygonsDataFrame"
                          ,"SpatialLines","SpatialPoints","SpatialPolygons"))
      if (any(!cl))
         isList <- FALSE
   }
   if (!is.character(driver)) {
      driver <- driverList[fext]
      if (is.na(driver)) {
         if (isList)
            driver <- driverList["sqlite"]
         else
            driver <- driverList["sqlite"]
         fext <- names(driver)
         lname <- bname
         bname <- paste0(lname,".",fext)
         fname <- file.path(dname,bname)
      }
   }
   if (isList) {
      if (!nchar(Sys.which("ogr2ogr")))
         stop("'ogr2ogr' is requires to merge layers")
     # fname1 <- paste0("res",seq_along(obj),".gpkg")
      fname1 <- .maketmp(length(obj),ext=interimExt)
      pb <- ursaProgressBar(min=0,max=2*length(obj))
      p4s <- sapply(obj,function(x){
         res <- if (inherits(x,c("sf","sfc"))) sf::st_crs(x)$proj4string
                else sp::proj4string(x)
         res
      })
      p4s <- unname(p4s)
      keepCRS <- ifelse(length(p4s)==1,TRUE,do.call("all.equal",as.list(p4s)))
      if (verbose)
         .elapsedTime("list:0")
      iname <- vector("list",length(fname1))
      for (i in seq_along(obj)) {
         o <- obj[[i]]
         isSF <- inherits(o,c("sf","sfc"))
         isSP <- !isSF
         if (isSF) {
            ind <- match(attr(o,"sf_column"),colnames(o))
            jname <- colnames(o)[-ind]
            colnames(o)[-ind] <- sprintf("fld%03d",seq_along(jname))
         }
         else if (isSP) {
            jname <- colnames(o@data)
            colnames(o@data) <- sprintf("fld%03d",seq_along(jname))
         }
         iname[[i]] <- jname
         .write_ogr(o,fname1[i],verbose=verbose) ## RECURSIVE
         setUrsaProgressBar(pb)
         if (verbose)
            .elapsedTime(paste0("list:",i))
      }
      lname <- names(obj)
      if (is.null(lname))
         lname <- rep("",length(obj))
      if (any(ind <- !nchar(lname))) {
         lname[ind] <- as.character(as.list(match.call())$obj)[-1][ind]
        # lname[ind] <- aname[ind]
      }
      dopt <- character()
      lopt <- character()
      if (driver=="MapInfo File")
         dopt <- c(dopt,"FORMAT=MIF")
      if (driver=="SQLite") {
        # dopt <- c(dopt,"SPATIALITE=yes")
         lopt <- c(lopt,"LAUNDER=NO")#,"SPATIAL_INDEX=YES")
      }
      suppressWarnings({
         first <- TRUE
         op <- options(warn=2)
         for (i in seq(wait)) {
            if (!file.exists(fname))
               break
            if (file.remove(fname))
               break
            if (first) {
               cat(paste("Waiting",wait,"seconds for permitted writting"
                        ,.sQuote(bname)))
               first <- FALSE
            }
            cat(".")
            Sys.sleep(1)
         }
         options(op)
         if (!first) {
            if (i==wait)
               cat(" FAILURE!\n")
            else
               cat(" ok!\n")
         }
      })
      for (i in seq_along(fname1)) {
         b <- paste(fname,fname1[i],"-nln",lname[i])
         if (length(dopt))
            b <- paste(paste("-dco",dopt),b)
         if (length(lopt))
            b <- paste(paste("-lco",lopt),b)
         if ((i==1)&&(keepCRS))
            b <- paste(b,"-t_srs",.dQuote(p4s[1]))
         if ((interimExt=="shp")&&(!is.null(iname[[i]]))) {
            aname <- sprintf("fld%03d",seq_along(iname[[i]]))
            s <- paste("select",paste(aname,"as",paste0("\\\"",iname[[i]],"\\\"")
                                     ,collapse=",")
                      ,"from",paste0("\\\""
                                    ,.gsub("\\.shp$","",basename(fname1[i]))
                                    ,"\\\""))
            s <- paste("-sql",.dQuote(s))
            ##~ cat(s)
            ##~ cat("\n")
            ##~ s <- ""
         }
         else
            s <- ""
         if (i==1)
            cmd <- paste("ogr2ogr",s,"-f",.dQuote(driver),b)
         else
            cmd <- paste("ogr2ogr",s,"-update -append",b)
         if (verbose)
            message(cmd)
         if (!system(cmd)) {
            if (.grep("\\.shp$",basename(fname1[i])))
               .shp.remove(fname1[i])
            else if (file.exists(fname1[i]))
               file.remove(fname1[i])
         }
         setUrsaProgressBar(pb)
         if (verbose)
            .elapsedTime(paste0("append:",i))
      }
      close(pb)
      return(invisible(0L))
   }
   if ((nchar(Sys.which("ogr2ogr"))>0)&&(driver %in% "SQLite")) {
      interim <- TRUE
      driver0 <- driver
      fext0 <- fext
      fname0 <- fname
      fext <- interimExt
      driver <- driverList[fext]
     # fname <- .gsub(paste0("\\.",fext0,"$"),paste0(".",fext),fname)
      fname <- .maketmp(ext=interimExt)
      bname <- basename(fname)
      p4s <- if (inherits(obj,c("sf","sfc"))) sf::st_crs(obj)$proj4string
             else sp::proj4string(obj)
   }
   else
      interim <- FALSE
   dopt <- character()
   lopt <- character()
   if (driver=="MapInfo File")
      dopt <- c(dopt,"FORMAT=MIF")
   if (driver=="SQLite") {
     # dopt <- c(dopt,"SPATIALITE=yes")
      lopt <- c(lopt,"LAUNDER=NO")#,"SPATIAL_INDEX=YES")
   }
   if ((is.logical(compress))&&(compress)) {
      compress <- if (driver %in% c("GeoJSON","SQLite","GPKG","KML")) "gz" 
                  else "zip"
   }
   if (verbose)
      print(data.frame(fname=fname,pack=compress,bname=bname,layer=lname
                      ,ext=fext,driver=driver))
   suppressWarnings({
      first <- TRUE
      op <- options(warn=2)
      for (i in seq(wait)) {
         if (!file.exists(fname))
            break
         if (file.remove(fname))
            break
         if (first) {
            cat(paste("Waiting",wait,"seconds for permitted writting"
                     ,.sQuote(bname)))
            first <- FALSE
         }
         cat(".")
         Sys.sleep(1)
      }
      options(op)
      if (!first) {
         if (i==wait)
            cat(" FAILURE!\n")
         else
            cat(" ok!\n")
      }
   })
   isSF <- inherits(obj,c("sf","sfc"))
   isSP <- !isSF
   if (verbose)
      print(data.frame(sf=isSF,sp=isSP,row.names="engine"))
   if (isSF)
      aname <- try(names(sf::st_agr(obj)),silent=TRUE)
   else if (isSP)
      aname <- try(colnames(obj@data),silent=TRUE)
   if (inherits(aname,"try-error"))
      aname <- character()
   if (driver %in% c("ESRI Shapefile")) {
      for (a in aname) {
         if (!inherits(obj[[a]],"POSIXt"))
            next
         v <- as.character(as.POSIXct(as.numeric(obj[[a]])
                          ,origin="1970-01-01",tz="UTC"))
         obj[[a]] <- paste0(.gsub("(^.+\\d)(\\s)(\\d.+$)","\\1\\T\\3",v),"Z")
      }
   }
   if (isSP) {
      if (driver %in% c("GeoJSON","KML","GPX")) {
         obj <- sp::spTransform(obj,sp::CRS("+init=epsg:4326"))
      }
      opW <- options(warn=1)
     # dsn <- if (driver %in% c("zzzESRI Shapefile")) dname else fname
      if (interim) {
         colnames(obj@data) <- iname <- sprintf("fld%03d",seq_along(colnames(obj@data)))
      }
      rgdal::writeOGR(obj,dsn=fname,layer=lname,driver=driver
                     ,dataset_options=dopt,layer_options=lopt
                   # ,encoding=encoding
                     ,overwrite_layer=TRUE
                     ,verbose=verbose)
      options(opW)
   }
   else if (isSF) {
      if (FALSE) {
         f <- .dir(path=dname
                  ,pattern=paste0("^",lname,"\\.",ext,"$")
                  ,full.names=TRUE)
         file.remove(f)
      }
      if (driver %in% c("GeoJSON","KML","GPX")) {
         obj <- sf::st_transform(obj,4326)
      }
      opW <- options(warn=1)
      if (interim) {
         ind <- head(seq_along(obj),-1)
         names(obj)[ind] <- iname <- sprintf("fld%03d",ind)
      }
      sf::st_write(obj,dsn=fname,layer=lname,driver=driver
                  ,dataset_options=dopt,layer_options=lopt
                  ,delete_layer=file.exists(fname)
                  ,delete_dsn=file.exists(fname)
                  ,quiet=!verbose)
      options(opW)
   }
   if (interim) {
      dopt <- character()
      lopt <- character()
      if (driver0=="MapInfo File")
         dopt <- c(dopt,"FORMAT=MIF")
      if (driver0=="SQLite") {
        # dopt <- c(dopt,"SPATIALITE=yes")
         lopt <- c(lopt,"LAUNDER=NO")#,"SPATIAL_INDEX=YES")
      }
      b <- character()
      if (length(dopt))
         b <- c(b,paste("-dco",.dQuote(dopt)))
      if (length(lopt))
         b <- c(b,paste("-lco",.dQuote(lopt)))
      lnameF <- ifelse(interimExt=="shp",.gsub("\\.shp$","",basename(fname)),lname)
      if (length(aname)) {
         s <- paste("select"
                   ,paste(iname,"as",paste0("\\\"",aname,"\\\""),collapse=",")
                   ,"from",paste0("\\\"",lnameF,"\\\""))
         s <- paste("-sql",.dQuote(s))
      }
      else
         s <- ""
      cmd <- paste("ogr2ogr"
                  ,ifelse(verbose,"-progress",""),s
                  ,"-f",.dQuote(driver0)
                  ,ifelse(interimExt=="shp","",paste("-t_srs",.dQuote(p4s)))
                  ,b
                  ,.dQuote(fname0),.dQuote(fname),"-nln",lname)
      if (verbose)
         message(cmd)
      system(cmd)
      if (file.exists(fname0)) {
         if (interimExt=="shp")
            .shp.remove(fname)
         else
            file.remove(fname)
      }
   }
   else if (driver=="ESRI Shapefile")
      writeLines("1251",file.path(dname,paste0(lname,".cpg")))
   if (!nchar(compress))
      return(invisible(NULL))
   if ((.lgrep("gz",compress))&&(nchar(Sys.which("gzip"))))
      system2("gzip",list(fname))
   else if (.lgrep("bz(ip)*2",compress)&&(nchar(Sys.which("bzip2"))))
      system2("bzip2",list(fname))
   else if (.lgrep("xz",compress)&&(nchar(Sys.which("xz"))))
      system2("xz",list(fname))
   else if (compress=="zip") {
      f <- .dir(path=dname
               ,pattern=paste0("^",lname,"\\.",ext,"$")
               ,full.names=TRUE)
      z <- paste0(fname,".zip")
      opW <- options(warn=-1)
      first <- TRUE
      for (i in seq(wait)) {
         if (!file.exists(z))
            break
         if (file.remove(z))
            break
         if (first) {
            cat(paste("Waiting for deleting",.sQuote(z)))
            first <- FALSE
         }
         cat(".")
         Sys.sleep(1)
      }
      if (!first) {
         if (i==wait)
            cat(" FAILURE!\n")
         else
            cat(" ok!\n")
      }
      options(opW)
      utils::zip(z,f,flags="-qm9j") ## verbose output ## 'myzip(z,f,keys="-m -9 -j")'
   }
   invisible(NULL)
}
'.shp.layer' <- function(fname)
{
   f <- basename(fname)
   if (!.lgrep("\\.shp$",f))
      return(f)
   .gsub("\\.shp$","",f)
}
'.shp.file' <- function(fname)
{
   if (.lgrep("\\.shp$",fname))
      return(fname)
   paste0(fname,".shp")
}
'.shp.read' <- function(fname,reproject=TRUE,encoding="1251",resetGrid=FALSE
                       ,verbose=0L,...)
{
  ## b <- sf::st_read("gde-1-1-15.shp",quiet=TRUE)
  ## b <- sf::st_transform(b,ursa_proj(a))
  # print(fname)
   requireNamespace("rgdal",quietly=.isPackageInUse())
   if (resetGrid)
      reproject <- FALSE
  # require(methods)
   if (.lgrep("\\.zip$",basename(fname)))
      fname <- .gsub("\\.zip$","",fname)
   fpath <- dirname(fname)
   z <- file.path(fpath,paste0(.shp.layer(fname),".zip")) # ,.
   if (!file.exists(z))
      z <- file.path(fpath,paste0(.shp.layer(fname),".shp.zip"))
   if (file.exists(z))
   {
      a <- utils::unzip(z,exdir=fpath,junkpaths=TRUE)
      on.exit(file.remove(a))
   }
   if (verbose>1)
      .elapsedTime("readOGR:start")
   cpgname <- file.path(fpath,paste0(.shp.layer(fname),".cpg"))
   e_opt <- if (file.exists(cpgname)) readLines(cpgname,warn=FALSE) else ""
   i_opt <- if (grepl("UTF(-)*8",e_opt)) TRUE else FALSE
  # print(data.frame(e_opt=e_opt,i_opt=i_opt))
   opW <- options(warn=0)
   res <- rgdal::readOGR(.shp.file(fname),.shp.layer(fname),pointDropZ=TRUE
                        ,encoding=e_opt,use_iconv=i_opt
                        ,verbose=as.logical(verbose),...)
   options(opW)
   if (verbose>1)
      .elapsedTime("readOGR:finish")
   proj4 <- session_grid()$proj4
   if ((reproject)&&(nchar(proj4))&&(!is.na(sp::proj4string(res)))) {
      if (verbose>1)
         .elapsedTime("spTransform:start")
      res <- sp::spTransform(res,proj4)
      if (verbose>1)
         .elapsedTime("spTransform:finish")
   }
   if (resetGrid)
      session_grid(NULL)
   res
}
'.shp.write' <- function(obj,fname,compress=FALSE,zip=NULL)
{
   requireNamespace("methods",quietly=.isPackageInUse()) 
  ## Error: inherits(obj, "Spatial") is not TRUE
  # require("methods") 
   requireNamespace("rgdal",quietly=.isPackageInUse())
  # suppressMessages(require(rgdal)) ## should be already loaded
   if (!is.null(zip))
      compress <- zip
   fpath <- dirname(fname)
   layer <- .shp.layer(fname)
   suppressWarnings({
      first <- TRUE
      op <- options(warn=2)
      repeat({
         if (!file.exists(.shp.file(fname)))
            break
         if (file.remove(.shp.file(fname)))
            break
         if (first) {
            cat(paste("Waiting for permitted writting",.sQuote(basename(fname))))
            first <- FALSE
         }
         cat(".")
         Sys.sleep(1)
      })
      if (!first)
         cat(" ok!\n")
      options(op)
   })
   opW <- options(warn=0)
   rgdal::writeOGR(obj,fpath,layer,driver="ESRI Shapefile"
                 # ,encoding=encoding
                  ,overwrite=TRUE)
   options(opW)
   writeLines("1251",file.path(fpath,paste0(layer,".cpg")))
   if (!compress)
      return(NULL)
   f <- .dir(path=dirname(fname)
            ,pattern=paste0("^",.shp.layer(fname),"\\.(cpg|dbf|prj|qpj|shp|shx)$")
            ,full.names=TRUE)
   z <- paste0(.shp.file(fname),".zip")
   opW <- options(warn=-1)
   first <- TRUE
   while(file.exists(z)) {
      if (file.remove(z))
         break
      if (first) {
         cat(paste("Waiting for deleting",.sQuote(z)))
         first <- FALSE
      }
      cat(".")
      Sys.sleep(1)
   }
   if (!first)
      cat(" ok!\n")
   options(opW)
   utils::zip(z,f,flags="-qm9j") ## verbose output ## 'myzip(z,f,keys="-m -9 -j")'
}
'.shp.geometry' <- function(fname,verbose=FALSE) {
   if (.lgrep("\\.zip$",basename(fname)))
      fname <- .gsub("\\.zip$","",fname)
   fpath <- dirname(fname)
   lname <- .shp.layer(fname)
   z <- file.path(fpath,paste0(lname,".zip"))
   if (!file.exists(z))
      z <- file.path(fpath,paste0(lname,".shp.zip"))
   if (file.exists(z))
   {
      a <- utils::unzip(z,exdir=fpath,junkpaths=TRUE)
      on.exit(file.remove(a))
   }
   info <- system(paste("ogrinfo",.dQuote(fname)),intern=TRUE)
   patt <- paste0(".+",lname," \\((.+)\\)")
   res <- .gsub(patt,"\\1",.grep(patt,info,value=TRUE))
   if (verbose)
      names(res) <- fname
   res
}
'.shp.remove' <- function(fname) {
   if (.lgrep("\\.shp$",fname))
      fname <- .gsub("\\.shp$","",fname)
   file.remove(.dir(paste0("^",fname,"\\.(cpg|dbf|prj|shp|shx)$")))
}
'.sf.read' <- function(fname,reproject=TRUE,encoding="1251",verbose=0L,...)
{
  ## b <- sf::st_read("gde-1-1-15.shp",quiet=TRUE)
  ## b <- sf::st_transform(b,ursa_proj(a))
  # print(fname)
   if (!requireNamespace("sf",quietly=.isPackageInUse()))
      return(NULL)
   if (!file.exists(fname)) {
      aname <- paste0(fname,".zip")
      if (isZip <- file.exists(aname)) {
         ziplist <- unzip(aname);on.exit(file.remove(ziplist))
         fname <- .grep("\\.shp$",ziplist,value=TRUE)
      }
   }
   else {
      if (isZip <- .lgrep("\\.zip$",fname)>0) {
         ziplist <- unzip(fname);on.exit(file.remove(ziplist))
         fname <- .grep("\\.shp$",ziplist,value=TRUE)
      }
   }
   if (verbose>1)
      .elapsedTime("st_read:start")
   res <- sf::st_read(fname,quiet=TRUE)
   if (isZip)
      cpgname <- .grep("\\.cpg$",ziplist,value=TRUE)
   else
      cpgname <- .gsub("\\.shp$",".cpg")
   if ((length(cpgname))&&(file.exists(cpgname))) {
      cpg <- readLines(cpgname,warn=FALSE)
      if (cpg=="UTF-8")
         cpg <- NULL
   }
   if (verbose>1)
      .elapsedTime("st_read:finish")
   proj4 <- session_grid()$proj4
   if ((reproject)&&(nchar(proj4))&&(!is.na(sf::st_crs(res)))) {
      if (verbose>1)
         .elapsedTime("st_transform:start")
      res <- sf::st_transform(res,proj4)
      if (verbose>1)
         .elapsedTime("st_transform:finish")
   }
   res
}
