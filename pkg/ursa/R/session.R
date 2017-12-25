'session_grid' <- function(obj)
{
   if (missing(obj))
   {
      ref <- getOption("ursaSessionGrid")
      if ((is.null(ref))||(!.is.grid(ref)))
      {
        # fname <- system.file("template","default.hdr",package="ursa")
         fname <- file.path(getOption("ursaRequisite"),"template.hdr")
         if (file.exists(fname)) {
            fname <- system.file("requisite/template.hdr",package="ursa")
            if (file.exists(fname))
               ref <- .read.hdr(fname)$grid
            else
               ref <- .read.hdr("default")$grid ## read.idr
         }
         options(ursaSessionGrid=ref)
      }
      return(invisible(ref))
   }
   if (is.null(obj))
      return(options(ursaSessionGrid=NULL))
   if (.is.grid(obj)) {
      options(ursaSessionGrid=obj)
      return(invisible(obj))
   }
   if (.is.ursa_stack(obj))
      obj <- obj[[1]]
   if (is.ursa(obj)) {
      options(ursaSessionGrid=obj$grid)
      return(invisible(obj$grid))
   }
   if ((!envi_exists(obj))&&(nchar(Sys.getenv("R_IDRISI")))&&(exists("read.idr"))) {
      g1 <- do.call("read.idr",list((obj)))$grid
      options(ursaSessionGrid=g1)
      return(invisible(g1))
   }
   if (is.character(obj)) {
      a <- open_envi(obj,resetGrid=TRUE,decompress=FALSE)
      g1 <- a$grid
      close(a)
      if (!.is.grid(g1))
         return(NULL)
      options(ursaSessionGrid=g1)
      return(invisible(g1))
   }
   str(obj)
   stop('Unable to recognize paramaters for new grid')
}
#'session_grid<-' <- function(obj,value) {
#   options(ursaSessionGrid=value)
#   return(session_grid())
#}
'session_proj4' <- function() session_grid()$proj4
'session_cellsize' <- function() with(session_grid(),sqrt(resx*resy))
'session_bbox' <- function() with(session_grid()
                                 ,c(minx=minx,miny=miny,maxx=maxx,maxy=maxy))
'session_pngviewer' <- function(allow=NA) {
   opV <- getOption("ursaAllowPngViewer")
   if ((is.na(allow))||(!is.logical(allow))) {
      if (is.logical(opV))
         return(opV)
      allow <- .isRscript()
   }
   opA <- options(ursaAllowPngViewer=allow)[[1]]
   if (is.null(opV))
      opA <- allow
  # invisible(getOption("ursaAllowPngViewer"))
  # invisible(allow) ## RC
   invisible(opA)
}
'session_tempdir' <- function(dst=character()) {
   opD <- getOption("ursaTempDir")
   if ((is.character(dst))&&(length(dst))) {
      if ((file.exists(dst))&&(file.info(dst)$isdir)) {
         options(ursaTempDir=dst)
         return(invisible(dst))
      }
   }
   if (length(opD))
      return(opD)
   dst <- ifelse(.isRscript(),getwd(),tempdir())
   options(ursaTempDir=dst)
   return(dst)
}
