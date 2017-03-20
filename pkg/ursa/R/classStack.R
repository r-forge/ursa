'as.list.ursaRaster' <- function(x,...) ursa_stack(x,...)
'unlist.ursaStack' <- function(x,recursive,use.names) ursa_brick(x)
'ursa_apply' <- function(obj,FUN,...) {
   res <- lapply(X=obj,FUN=FUN,...)
   if (.is.ursa_stack(res))
      class(res) <- "ursaStack"
   res
}
'ursa_stack' <- function(...) {
   obj <- list(...)
   if ((length(obj)==1)&&(is.ursa(obj[[1]]))) {
      obj <- obj[[1]]
      res <- vector("list",nband(obj))
     # names(res) <- bandname(obj)
      for (i in seq_along(res))
         res[[i]] <- obj[i]
      names(res) <- names(obj)
      class(res) <- "ursaStack"
      return(res)
   }
   class(obj) <- "ursaStack"
   obj
}
'ursa_brick' <- function(obj) {
   if (is.ursa(obj))
      return(obj)
   isList <-  .is.ursa_stack(obj)
   if (!isList)
      return(NULL)
   n <- sapply(obj,nband)
   res <- ursa_new(nband=sum(n),bandname=unlist(lapply(obj,bandname)))
   oname <- names(obj)
   k <- 0L
   for (i in seq_along(obj)) {
      img <- .extract(obj[[i]])
      ##~ if (.is.colortable(img)) {
         ##~ print(img)
         ##~ print(ursa_colortable(img))
         ##~ img <- reclass(img)
         ##~ print(img)
      ##~ }
      nl <- nband(img)
      k2 <- k+seq(nl)
      res[k2] <- img
      if ((!is.null(oname))&&(nl==1))
         bandname(res)[k2] <- oname[i]
      k <- k+nl
   }
  # bandname(res) <- sapply(obj,bandname)
  # class(res) <- c(class(res),"ursaBrick") ## not necessary
   res
}
'.is.ursa_stack' <- function(obj) {
   if (is.ursa(obj))
      return(FALSE)
   if (!is.list(obj))
      return(FALSE)
   all(sapply(obj,function(x) is.ursa(x) | is.null(x)))
}
'.is.ursa_brick' <- function(obj) is.ursa(obj)
