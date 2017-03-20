'band_blank' <- function(obj)
{
   if (!is.ursa(obj))
      return(NULL)
   z <- obj$con$posZ
   nb <- if (!is.na(z[1])) length(z) else obj$dim[2]
   res <- rep(FALSE,nb)
   if (is.matrix(obj$value))
   {
      for (i in seq_along(res)) {
         r <- unique(obj$value[,i])
         res[i] <- (length(r)==1)&&(r==0 | is.na(r))
      }
   }
   else
   {
      for (i in chunk_band(obj))
         res[i] <- apply(obj[i]$value,2,function(z) all(is.na(z)) | all(z==0))
   }
   res
}
'ursa_blank' <- function(obj) if (is.ursa(obj)) all(band_blank(obj)) else NULL
'.which.blank' <- function(obj) if (is.ursa(obj)) which(band_blank(obj)) else NULL
