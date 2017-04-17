'create_envi' <- function(x,...) {
   .prepare.con(x,implement="ENVI",...)
}
'create_gdal' <- function(x,...) {
   requireNamespace("rgdal",quietly=.isPackageInUse())
   .prepare.con(x,implement="GDAL",...)
}
