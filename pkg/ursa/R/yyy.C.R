'.Cursa' <- function(...) {
   arglist <- list(...)
   if (.isPackageInUse())
      arglist$PACKAGE <- "ursa"
   do.call(".C",arglist)
}
