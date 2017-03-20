'[.ursaRaster' <- function(x,i,j,...,drop=FALSE)
{
   verbose <- FALSE
   if (verbose)
      print(match.call())
   dimx <- dim(x$value)
   clValue <- class(x$value)
   res <- x
   missingJ <- missing(j) ## read all lines and (selected) bands
   missingI <- missing(i) ## read all bands and (selected) lines
   if ((missingJ)&&(missingI))
   {
      if (length(dimx)==2L)
         return(x) ## return(x$value) identiical(x$value,as.matrix(x))==TRUE
   }
   if (!is.null(dimx))
   {
      if ((!missingI)&&(is.ursa(i)))
      {
        # return(res*i) ## removed 20160805
         if (nband(i)==1)
            res$value[which(is.na(i$value)),] <- NA
         else
            res$value[which(is.na(i$value))] <- NA
         return(res) ## added 20160805
      }
      if (missingJ)
         j <- seq(dimx[1])
      if (missingI)
         i <- seq(dimx[2])
      if ((is.numeric(i))&&(any(abs(i)>dimx[2])))
         i <- as.character(i)
      if (is.character(i))
      {
         args <- list(...)
         regexp <- if ("regexp" %in% names(args)) args[["regexp"]] else FALSE
         i <- .getBand(res,i,regexp=regexp)
         j <- seq(dimx[1])
         missingJ <- TRUE
         missingI <- FALSE
      }
     # if (is.con(res$con))
     #    res$con$indexZ <- res$con$indexZ[i]
     # print(c(j=range(j),i=i))
     # res$name <- make.unique(res$name[i],"_")
      if (all(i<0))
      {
         if (!is.na(res$con$posZ[1]))
            i <- seq(res$con$posZ)[i]
         else
            i <- seq(res$dim[2])[i]
      }
     # res$dim[2] <- length(i)
      if (!missingJ)
      {
         if (verbose)
            .elapsedTime("expandIndex:start")
         if (is.na(x$con$indexC)[1])
            s <- x$grid$columns
         else
            s <- length(x$con$indexC)
         j2 <- j
         j <- as.integer(c(t(col(array(NA,dim=c(length(j2),s)))+(j2-1)*s)))
         if (max(j)>x$dim[1])
         {
            opW <- options(warn=0)
            warning(paste("Expected",length(j),"bytes, but found",max(j),"bytes"))
            options(opW)
            j <- j[j<=x$dim[1]]
         }
         if (verbose)
            .elapsedTime("expandIndex:finish")
      }
      if (.is.con(res$con))
      {
         if (!missingJ) {
            if (is.na(res$con$posR[1]))
               res$con$posR <- j2
            else
               res$con$posR <- res$con$posR[j2]
         }
         if (!missingI)
         {
            if (is.na(res$con$posZ[1]))
            {
               if (is.logical(i))
                  res$con$posZ <- which(i)
               else
                  res$con$posZ <- i
            }
            else
               res$con$posZ <- res$con$posZ[i]
         }
      }
      else
         res$name <- res$name[i]
      cl <- class(res$value)
      res$value <- res$value[j,i,drop=drop]
      class(res$value) <- cl
      res$dim <- dim(res$value)
      return(res)
   }
   res$con$compress <- 0L
   grid <- res$grid
   con <- res$con
   internalReading <- con$connection %in% c("gzfile")
   externalReading <- !internalReading
  # print(c(externalReading=externalReading))
   if ((1)&&(!missingJ)&&(is.character(j)))
      stop("TODO: is.character(j)")
   if ((1)&&(!missingI)&&(is.character(i)))
   {
      args <- list(...)
      regexp <- if ("regexp" %in% names(args)) args[["regexp"]] else FALSE
      i <- .getBand(res,i,regexp=regexp)
      if (is.null(i))
      {
         op <- options(warn=1)
         warning(paste("Speciefied name",paste0("'",i,"'"),"is not in bandname"))
         options(op)
         return(NULL)
      }
      missingJ <- TRUE
      missingI <- FALSE
   }
   if ((!missingI)&&(all(i<0)))
   {
      if (!is.na(res$con$posZ[1]))
         i <- seq(res$con$posZ)[i]
      else
         i <- seq(res$dim[2])[i]
   }
   if ((missingJ)&&(missingI)) ## read full
   {
      if (con$driver=="ENVI") {
         n <- prod(with(con,samples*lines*bands))
         xdim <- with(con,c(lines,samples,bands))
         if ((con$seek)&&(con$interleave %in% c("bsq","bil"))&&
             (externalReading)&&(TRUE)) {
            seek(con,where=0L,origin="start",rw="r")
            if (con$interleave=="bsq") {
               if (con$mode=="integer")
                  res$value <- with(con,.Cursa("readBsqBandInteger"
                                   ,fname=fname,dim=xdim,index=seq(bands),n=bands
                                   ,datatype=datatype,swap=swap
                                   ,res=integer(bands*samples*lines)))$res
               else
                  res$value <- with(con,.Cursa("readBsqBandDouble"
                                   ,fname=fname,dim=xdim,index=seq(bands),n=bands
                                   ,datatype=datatype,swap=swap
                                   ,res=double(bands*samples*lines)))$res
            }
            else if (con$interleave=="bil") {
               if (con$mode=="integer") {
                  res$value <- with(con,.Cursa("readBilLineInteger2"
                                   ,fname=fname,dim=xdim,index=seq(lines),n=lines
                                   ,datatype=datatype,swap=swap
                                   ,res=integer(bands*samples*lines)))$res
               }
               else {
                  res$value <- with(con,.Cursa("readBilLineDouble2",fname,dim=xdim
                           ,lines=seq(lines)
                           ,nline=lines,datatype=datatype,swap=swap
                           ,res=double(with(con,lines*samples*bands))))$res
               }
            }
            dim(res$value) <- with(con,c(samples,lines,bands))
         }
         else {
            res$value <- with(con,.readline(handle,datatype,n,endian))
            if (con$interleave=="bil") ##bil[col,band,row] -> R[col,row,band]
            {
               dim(res$value) <- with(con,c(samples,bands,lines))
               res$value <- aperm(res$value,c(1,3,2)) ##(3,1,2)
            }
            else if (con$interleave=="bip") ##bip[band,col,row] -> R[col,row,band]
            {
               dim(res$value) <- with(con,c(bands,samples,lines))
               res$value <- aperm(res$value,c(2,3,1)) ##c(3,2,1)
            }
            else if (con$interleave=="bsq") ##bsq[col,row,band] -> R[col,row,band]
            {
               dim(res$value) <- with(con,c(samples,lines,bands))
              # res$value <- aperm(res$value,c(1,2,3)) ##c(2,1,3)
            }
            else
               stop("read ENVI: Error in input header file ",con$interleave
                   ," incorrect interleave type")
         }
      }
      else if (con$driver=="GDAL") { ## read full
         res$value <- rgdal::getRasterData(con$handle)
         dim(res$value) <- with(con,c(samples,lines,bands))
      }
      else
         stop(paste("unknown driver:",con$driver))
      if ((con$samples!=res$grid$columns)||(con$lines!=res$grid$rows))
         res$value <- res$value[con$indexC,con$indexR,,drop=FALSE]
      dimy <- dim(res$value)
      dim(res$value) <- c(dimy[1]*dimy[2],dimy[3]) ## t()
      res$dim <- dim(res$value)
      if (!is.na(con$nodata)) {
         if (abs(con$nodata)<1)
            res$value[abs(res$value-con$nodata)<1e-27] <- NA
         else
            res$value[abs(res$value/con$nodata-1)<1e-6] <- NA
      }
      class(res$value) <- clValue
      return(res)
   }
   else if ((missingJ)&&(!missingI)) ## read band
   {
      if (verbose)
         cat("read bands ",min(i),":",max(i),"\n")
      if (is.list(i))
         i <- unlist(i)
      nb <- length(i)
      i <- as.integer(i)
      nline <- if (!is.na(con$indexR[1L])) length(con$indexR) else con$lines
      minJ <- min(con$indexR)-1
      minI <- min(i)
      toWarp <- with(con,(!is.na(indexR)[1])&&(length(indexR)!=lines)||
                         (!is.na(indexC)[1])&&(length(indexC)!=samples))
      if (con$driver=="ENVI") {
         if (con$interleave=="bil")
         {
            if ((externalReading)&&(TRUE))
            {
               if (con$seek)
                  seek(con,where=0L,origin="start",rw="r")
               xdim <- with(con,c(lines,samples,bands))
               if (con$mode=="integer")
                  val <- .Cursa("readBilBandInteger",con$fname,dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nb*samples*lines)))$res
               else
                  val <- .Cursa("readBilBandDouble",con$fname,dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nb*samples*lines)))$res
            }
            else
            {
               n <- nb*con$samples
               val <- array(NA,dim=c(n,nline))
               for (r in seq(nline))
               {
                  pos <- with(con,(minJ+bands*(r-1)+minI-1)*samples*sizeof)
                  if (con$seek)
                     seek(con,where=pos,origin="start",rw="r")
                  val[,r] <- with(con,.readline(handle,datatype,n,endian))
               }
            }
            if (toWarp) ## added 2013-06-14
            {
               dim(val) <- with(con,c(samples,nb,lines))
               val <- val[,,con$indexR,drop=FALSE]
            }
            dim(val) <- with(con,c(samples,nb,nline))
            val <- aperm(val,c(1,3,2))
         }
         else if (con$interleave=="bip")
         {
            n <- with(con,samples*bands)
            val <- array(NA,dim=c(nb*con$samples,nline))
            ind <- which(with(con,(seq(bands*samples)-1)%%bands+1) %in% i)
            for (r in seq(nline))
            {
               pos <- with(con,(minJ+(r-1)*samples*bands)*sizeof)
               if (con$seek)
                  seek(con,where=pos,origin="start",rw="r")
               val[,r] <- with(con,.readline(handle,datatype,n,endian))[ind]
            }
            dim(val) <- with(con,c(nb,samples,nline))
            val <- aperm(val,c(2,3,1))
         }
         else if (con$interleave=="bsq")
         {
            isSeq <- identical(i,min(i):max(i))
           # val <- array(NA,dim=c(con$samples*nline,nb))
            if ((externalReading)&&(TRUE))
            {
              # cat("isDll\n")
               if (con$seek)
                  seek(con,where=0L,origin="start",rw="r")
               xdim <- with(con,c(lines,samples,bands))
              # str(list(i=i,dim=xdim,nb=nb,fname=con$fname))
               if (con$mode=="integer")
               {
                  val <- .Cursa("readBsqBandInteger",fname=con$fname,dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nb*samples*lines)))$res
               }
               else
               {
                  val <- .Cursa("readBsqBandDouble",fname=con$fname,dim=xdim,index=i
                           ,n=nb,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nb*samples*lines)))$res
               }
            }
            else
            {
               val <- array(NA,dim=c(con$samples*nline,nb))
               if ((1)&&(isSeq))
               {
                  n <- with(con,nline*samples*nb)
                  pos <- with(con,(minJ+(minI-1)*lines*samples)*sizeof)
                  if (con$seek)
                     seek(con,where=pos,origin="start",rw="r")
                  val[] <- with(con,.readline(handle,datatype,n,endian))
               }
               else
               {
                  n <- with(con,nline*samples)
                  for (r in seq(along=i))
                  {
                     pos <- with(con,(minJ+(i[r]-1)*lines*samples)*sizeof)
                     if (con$seek)
                        seek(con,where=pos,origin="start",rw="r")
                     val[,r] <- with(con,.readline(handle,datatype,n,endian))
                  }
               }
            }
            if (toWarp)
            {
               dim(val) <- with(con,c(samples,lines,nb))
               val <- val[,con$indexR,,drop=FALSE] ## before 2012-12-23
            }
            dim(val) <- with(con,c(samples,nline,nb))
           # val <- aperm(val,c(1,2,3))
         }
         else
            stop("Error in input header file ",con$interleave
                ," incorrect interleave type")
      }
      else if (con$driver=="GDAL") { ## read band
         val <- rgdal::getRasterData(con$handle,band=i)
         dim(val) <- with(con,c(samples,nline,nb))
      }
      if (!is.na(con$indexC[1]))
         res$value <- val[con$indexC,,,drop=FALSE]
      else
         res$value <- val
      dimy <- dim(res$value)
      dim(res$value) <- c(dimy[1]*dimy[2],dimy[3])
      if (!is.na(con$nodata)) {
         if (abs(con$nodata)<1)
            res$value[abs(res$value-con$nodata)<1e-27] <- NA
         else {
           # print(abs(res$value/con$nodata-1))
            res$value[abs(res$value/con$nodata-1)<1e-6] <- NA
         }
      }
      res$con$posZ <- i
      class(res$value) <- clValue
      return(res)
   }
   else if ((!missingJ)&&(missingI)) ## read line
   {
      if (is.list(j))
         j <- unlist(j)
      j <- as.integer(seq(min(j),max(j)))
      nline <- length(j)
      minJ <- (min(j)-1L)+min(con$indexR-1L)
      if (con$driver %in% "ENVI") {
         if (con$interleave=="bil") ##bil[col,band,row] -> R[col,row,band]
         {
            if ((externalReading)&&(TRUE))
            {
               xdim <- with(con,c(lines,samples,bands))
               if (con$mode=="integer")
                  val <- .Cursa("readBilLineInteger",con$fname,dim=xdim
                           ,lines=j+as.integer(min(con$indexR-1L))
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nline*samples*bands)))$res
               else
                  val <- .Cursa("readBilLineDouble",con$fname,dim=xdim
                           ,lines=j+as.integer(min(con$indexR-1L))
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nline*samples*bands)))$res
              # print(summary(val))
            }
            else
            {
               n <- with(con,nline*bands*samples)
              # print(sprintf("seek=%d",with(con,minJ*bands*sizeof*samples)))
               if (con$seek)
                  seek(con,where=with(con,minJ*bands*sizeof*samples)
                      ,origin="start",rw="r")
               val <- with(con,.readline(handle,datatype,n,endian))
              # print(summary(val))
            }
            dim(val) <- with(con,c(samples,bands,nline))
            val <- aperm(val,c(1,3,2))
         }
         else if (con$interleave=="bip") ##bip[band,col,row] -> R[col,row,band]
         {
            n <- with(con,nline*bands*samples)
            seek(con,where=with(con,minJ*bands*sizeof*samples),origin="start",rw="r")
            val <- with(con,.readline(handle,datatype,n,endian))
            dim(val) <- with(con,c(bands,samples,nline))
            val <- aperm(val,c(2,3,1))
         }
         else if (con$interleave=="bsq") ##bsq[col,row,band] -> R[col,row,band]
         {
            if ((externalReading)&&(TRUE))
            {
               xdim <- with(con,c(lines,samples,bands))
               if (con$mode=="integer")
                  val <- .Cursa("readBsqLineInteger",con$fname,dim=xdim,lines=j
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=integer(with(con,nline*samples*bands)))$res
               else
                  val <- .Cursa("readBsqLineDouble",con$fname,dim=xdim,lines=j
                           ,nline=nline,datatype=con$datatype,swap=con$swap
                           ,res=double(with(con,nline*samples*bands)))$res
            }
            else
            {
               n <- nline*con$samples
               val <- array(NA,dim=c(n,con$bands))
               for (nb in seq(con$bands))
               {
                  pos <- minJ+(nb-1)*con$lines
                  if (con$seek)
                     seek(con,where=with(con,pos*sizeof*samples),origin="start",rw="r")
                  val[,nb] <- with(con,.readline(handle,datatype,n,endian))
               }
            }
            dim(val) <- with(con,c(samples,nline,bands))
           # val <- aperm(val,c(1,2,3))
         }
      }
      else if (con$driver=="GDAL") { ## read line
         nline <- length(j)
         minJ <- (min(j)-1L)+min(con$indexR-1L)
         val <- rgdal::getRasterData(con$handle,offset=c(minJ,0)
                               ,region.dim=c(nline,con$samples))
         dim(val) <- with(con,c(samples,nline,bands))
      }
      else
         stop("read ENVI: Error in input header file ",con$interleave
             ," incorrect interleave type")
      res$value <- val[con$indexC,,,drop=FALSE]
      dimy <- dim(res$value)
      dim(res$value) <- c(dimy[1]*dimy[2],dimy[3]) ## t()
      if (!is.na(con$nodata)) {
         if (abs(con$nodata)<1)
            res$value[abs(res$value-con$nodata)<1e-27] <- NA
         else
            res$value[abs(res$value/con$nodata-1)<1e-6] <- NA
      }
      res$con$posR <- j
      res$dim <- dim(res$value)
      class(res$value) <- clValue
      return(res)
   }
   else
      stop("TODO")
   stop("UNREACHABLE CODE")
}
'.readline' <- function(Fin,datatype,size,endian)
{
   if (FALSE) ## benchmark is needed
      datatype <- datatype+1000L
   if (datatype==1001) ## data.type==1 -> 1-byte short 
      val <- readBin(Fin,integer(),n=size,size=1,signed=FALSE)
   else if (datatype==1) ## data.type==1 -> 1-byte short, unsigned
      val <- readBin(readBin(Fin,raw(),n=size*1)
                    ,integer(),n=size,size=1,signed=FALSE)
   else if (datatype==1002) ## data.type==2 -> 2-byte short, signed
      val <- readBin(Fin,integer(),n=size,size=2,endian=endian,signed=TRUE)
   else if (datatype==2) ## data.type==2 -> 2-byte short, unsigned
      val <- readBin(readBin(Fin,raw(),n=size*2)
                    ,integer(),n=size,size=2,endian=endian,signed=TRUE)
   else if (datatype==1003) ## data.type==3 -> 4-byte int
      val <- readBin(Fin,integer(),n=size,endian=endian,signed=TRUE)
   else if (datatype==3) ## data.type==3 -> 4-byte int
      val <- readBin(readBin(Fin,raw(),n=size*4)
                    ,integer(),n=size,endian=endian,signed=TRUE)
   else if (datatype==1004) ## data.type==4 -> 4-byte float 
      val <- readBin(Fin,double(),n=size,size=4,endian=endian)
   else if (datatype==4) ## data.type==4 -> 4-byte float 
      val <- readBin(readBin(Fin,raw(),n=size*4)
                    ,double(),n=size,size=4,endian=endian)
   else if (datatype==5) ## data.type==5 -> 8-byte double
      val <- readBin(Fin,double(),n=size,endian=endian)
   else if (datatype %in% c(11,1011)) ## data.type==11 -> 1-byte signed short integer
      val <- readBin(Fin,integer(),n=size,size=1,endian=endian,signed=TRUE)
   else if (datatype %in% c(12,1012)) ## data.type==12 -> 2-byte unsigned short integer
      val <- readBin(Fin,integer(),n=size,size=2,endian=endian,signed=FALSE)
   else if (datatype %in% c(13,1013)) ## data.type==13 -> 4-byte unsigned integer
   {
      val <- readBin(Fin,integer(),n=size,size=4,endian=endian,signed=!FALSE) ## need signed=FALSE!
      ind <- which(val>(2^31-1))
      if (length(ind)) {
         opW <- options(warn=1)
         warning("Datatype 'UInt32' is supported with restriction. See '?readBin'.")
         options(opW)
         val[ind] <- val[ind]-2^32
      }
   }
   else
      stop("Unsupported data data type: ",datatype)
   gc(reset=TRUE)
   val
}
