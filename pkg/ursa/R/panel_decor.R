'panel_decor' <- function(...) {
   arglist <- list(...)
   opR <- options(ursaPngAuto=TRUE)
   ind <- .grep("decor",names(arglist))
   if (!length(ind))
      arglist$decor <- TRUE
   do.call("panel_coastline",arglist)
   do.call("panel_gridline",arglist)
   do.call("panel_scalebar",arglist)
   do.call("panel_annotation",arglist)
   options(opR)
   invisible(NULL)
}
