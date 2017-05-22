'ursa_colortable' <- function(x)
{
   if ((TRUE)&&(inherits(x,"ursaColorTable"))) ## was FALSE 20160127
      ct <- x
   else if (is.character(x)) {
      valid <- all(sapply(unname(x),function(col) {
         tryCatch(is.matrix(col2rgb(col)),error=function(e) FALSE)
      }))
      if (!valid)
         return(NULL)
      class(x) <- "ursaColorTable"
      return(x)
   }
   else if (!is.ursa(x)) {
      ct <- x$colortable
      if (!inherits(ct,"ursaColorTable")) {
         return(NULL)
      }
   }
   else 
      ct <- x$colortable
   if (!length(ct))
      return(ct)
   if (is.null(names(ct)))
      names(ct) <- as.character(seq(length(ct))-1L)
   ct
}
'[.ursaColorTable' <- function(x,i) {
   cl <- class(x)
   res <- unclass(x)[i]
   class(res) <- cl
   res
}
'ursa_colortable<-' <- function(x,value)
{
   if (!is.ursa(x))
      return(NULL)
  ## Implement? 'if (is.ursa(value,"colortable)) {x <- colorize(x,value);return(x)'
   if (is.null(value))
      value <- character(0)
   myname <- names(x$colortable)
   if ((length(myname)==length(value))&&(is.null(names(value))))
      names(value) <- myname
   class(value) <- "ursaColorTable"
   x$colortable <- value
   x
}
'print.ursaColorTable' <- function(x,...)
{
   print(unclass(x))
   cn <- names(x)
   cnd <- .deintervale(cn)
  # if ((!identical(cn,as.character(cnd)))&&(length(cn)))
   if (length(cn)!=length(cnd))
      print(.deintervale(cn),quote=FALSE)
}
'.is.colortable' <- function(obj) {
   if (is.ursa(obj))
      obj <- obj$colortable
   ((length(obj)>0)&&(inherits(obj,"ursaColorTable")))
}
'.is.nominal' <- function(obj) {
   if (!.is.colortable(obj))
      return (FALSE)
   ct <- ursa_colortable(obj)
   val <- .deintervale(ct)
   length(val)==length(ct)
}
'.is.interval' <- function(obj) {
   if (!.is.colortable(obj))
      return (FALSE)
   ct <- ursa_colortable(obj)
   val <- .deintervale(ct)
   length(val)!=length(ct)
}
'.is.category' <- function(obj) {
   if (is.ursa(obj))
      return(inherits(obj$value,"ursaCategory"))
   inherits(obj,"ursaCategory")
}
'.be.category' <- function(obj) {
   (.is.colortable(obj))&&(!.is.category(obj))
}
