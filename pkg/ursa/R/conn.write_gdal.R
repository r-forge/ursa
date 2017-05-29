# 'ursa_write' <- function(...) .syn('.write_gdal',2,...)
'ursa_write' <- function(obj,fname) write_gdal(obj=obj,fname=fname)
'write_gdal' <- function (obj,...) {
   if (!.try(requireNamespace("rgdal",quietly=.isPackageInUse())))
      return(NULL)
   res <- create_gdal(obj,...)
   res[] <- obj
   close(res)
   return(invisible(res$con$datatype))
}
