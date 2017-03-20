## 'caTools::write_envi'

'write_envi' <- function(obj,...)
{
   if (.is.ursa_stack(obj))
      obj <- if (length(obj)>1) ursa_brick(obj) else obj[[1]]
   arglist <- list(...)
   isCT <- .getPrm(arglist,name="colortable",default=TRUE)
   if (!.is.colortable(obj))
      isCT <- FALSE
  # if (((isCT)&&(!obj$category))||((!isCT)&&(obj$category)))
   if (isCT+.is.category(obj)==1)
   {
      obj <- reclass(obj)
   }
   if (!isCT) {
      ursa_colortable(obj) <- character(0)
   }
   if (!FALSE) # deprecated (20161225 really?)
      res <- create_envi(obj,...)
   else {
      nodata <- .getPrm(arglist,name="(^bg$|nodata|ignore)",default=NA)
      if ((is.na(nodata))&&(anyNA(obj$value)))
         arglist$nodata <- .optimal.nodata(obj$value)
      res <- do.call("create_envi",c(list(obj),arglist))
   }
   res[] <- obj
   close(res)
   return(invisible(res$con$datatype))
}
