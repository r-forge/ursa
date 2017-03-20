'compose_legend' <- function(...)
{
   arglist <- list(...)
   colorbar <- .getPrm(arglist,name="^colorbar$",default=TRUE)
   if (!as.logical(colorbar))
      return(NULL)
   units <- NULL
   ind <- sapply(arglist,is.ursa,"colortable")
   if (length(which(ind))) {
      obj <- vector("list",length(ind))
      myname <- names(ind)
      names(obj) <- myname
      ind2 <- rep(TRUE,length(ind))
      for (i in seq_along(obj)) {
         if (ind[i])
            obj[[i]] <- ursa_colortable(arglist[[i]])
         else {
            if (!nchar(myname[i]))
               obj[[i]] <- as.character(arglist[[i]])
            else
               ind2[i] <- FALSE
              # obj[[i]] <- NULL
         }
      }
      obj <- obj[ind2]
      arglist <- arglist[!ind2]
      isList <- TRUE
     # arglist <- list(NA)
      units <- names(obj)
   }
   else {
      ind <- sapply(arglist[[1]],is.ursa,"colortable")
      if ((is.list(ind))&&(!length(ind)))
         return(NULL)
      if (length(which(ind))) {
         obj <- vector("list",length(ind))
         names(obj) <- names(ind)
         for (i in seq_along(obj)) {
            if (ind[i])
               obj[[i]] <- ursa_colortable(arglist[[1]][[i]])
            else
               obj[[i]] <- as.character(arglist[[1]][[i]])
         }
         isList <- TRUE
      }
      else {
         obj <- .getPrm(arglist,name="",default=NULL
                       ,class=list(c("list","ursaRaster"),"ursaRaster"))
         if (is.null(obj)) {
            obj <- .getPrm(arglist,name="",default=NULL
                          ,class=list(c("list","ursaColorTable"),"ursaColorTable")
                          ,verbose=FALSE)
            obj <- .getPrm(arglist,index=1L,class="ursaColorTable",verbose=TRUE)
            isList <- all(sapply(obj,function(x) class(x) %in% "ursaColorTable"))
            if (is.null(obj))
               return(NULL)
         }
         else {
            isList <- .is.ursa_stack(obj)
            if (isList) {
               myname <- names(obj)
               obj <- lapply(obj,ursa_colortable)
               names(obj) <- myname
            }
            else
               obj <- ursa_colortable(obj)
         }
      }
   }
  # str(obj,grid=FALSE)
   if (is.null(units))
      units <- .getPrm(arglist,name="units",class=list("expression","character")
                      ,default=NA_character_)
   skip <- getOption("ursaPngSkipLegend")
   if (isList) {
      if ((!is.expression(units))&&(is.na(units[1]))) {
         units <- names(obj)
         if (is.null(units)) {
           # units <- sapply(obj,names)
            units <- sapply(obj,function(x) {
               if (is.ursa(x))
                  return(names(x))
               NULL
            })
         }
      }
      if ((is.null(units))||(length(units)!=length(obj))) {
         if (is.null(units))
            units <- rep("",len=length(obj))
         else
            units <- rep(units,len=length(obj))
      }
      for (i in seq_along(obj)) {
         if (!(i %in% skip)) {
           # legend_colorbar(obj=obj[[i]],units=units[i],...)
            arglist <- c(quote(obj[[i]]),arglist)
           # arglist[[1]] <- quote(obj[[i]])
            if (.is.colortable(obj[[i]])) {
               arglist[["units"]] <- units[i]
               do.call("legend_colorbar",arglist)
               arglist[["units"]] <- NULL
            }
            else {
               do.call("legend_mtext",arglist)
              # legend_mtext(obj[[i]])
            }
         }
      }
   }
   else {
      myname <- names(arglist)
      if (is.null(myname))
         myname <- ""
      arglist <- arglist[match(unique(myname),myname)]
     # legend_colorbar(obj,units=units,...)
      do.call("legend_colorbar",arglist)
   }
   NULL
}
