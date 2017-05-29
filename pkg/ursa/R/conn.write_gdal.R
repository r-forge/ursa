# 'ursa_write' <- function(...) .syn('.write_gdal',2,...)
'ursa_write' <- function(obj,fname) write_gdal(obj=obj,fname=fname)
'write_gdal' <- function (obj,...) {
   requireNamespace("rgdal",quietly=.isPackageInUse())
   res <- create_gdal(obj,...)
   if (is.null(res))
      return(invisible(-99L))
   res[] <- obj
   close(res)
   return(invisible(res$con$datatype))
}
