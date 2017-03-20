'.Cursa' <- function(...) {
   arglist <- list(...)
   if ("ursa" %in% loadedNamespaces())
      arglist$PACKAGE <- "ursa"
   do.call(".C",arglist)
}
