'create_envi' <- function(x,...) {
   .prepare.con(x,implement="ENVI",...)
}
'create_gdal' <- function(x,...) {
   requireNamespace("rgdal")
   .prepare.con(x,implement="GDAL",...)
}
