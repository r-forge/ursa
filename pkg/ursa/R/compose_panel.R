'compose_panel' <- function(...)
{
   if (.skipPlot(TRUE))
      return(NULL)
   arglist <- list(...)
   img <- .getPrm(arglist,name="",default=NULL
                 ,class=list(c("list","ursaRaster"),"ursaRaster"))
   isList <- .is.ursa_stack(img)
   if ((is.null(img))||((!isList)&&(ursa_blank(img)))) { ## was 'missing'
      panel_new(...)
      panel_decor(...)
      return(NULL)
   }
  # annotation <- .getPrm(arglist,name="annotation",default=NA)#_character_)
  # decor <- .getPrm(arglist,name="decor",default=TRUE)
  # scalebar <- .getPrm(arglist,name="scalebar",default=FALSE)
   verbose <- .getPrm(arglist,name="verb",kwd="plot",default=NA) ## FALSE?
  # if ((!is.na(verbose))&&(verbose))
  #    str(list(annotation=annotation,verbose=verbose))
  # isBox <- getOption("ursaPngBox")
  # skip <- getOption("ursaPngSkip")
   np <- getOption("ursaPngLayout")$image
   if (isList)
   {
      ng <- length(img)
      nb <- sum(unlist(sapply(img,nband)))
      ln <- unlist(lapply(unname(img),bandname))
      units <- names(img)
      if (is.null(units))
         units <- if (FALSE) rep("",ng) else unname(ln)
   }
   else
   {
      ng <- 1L
      nb <- nband(img)
      ln <- bandname(img)
      units <- if (nb==1) ln[1] else ""
   }
   isRGB <- nb/3==np || nb/4==np || nb/2==np
   if (isRGB)
      nl <- nb/np
  # print(c(isRGB=isRGB))
  # print(c(nb=nb,np=np))
   annotation <- nb>1 & !isRGB #& !isList
   if (is.na(verbose))
      verbose <- nb>2
   txt <- NULL
   if (is.character(annotation))
      txt <- if (length(annotation)==nb) annotation else rep(annotation,nb)
   else if ((is.logical(annotation))&&(annotation))
      txt <- ln
   else
      txt <- ""
   annotation <- is.character(txt)
   k <- 0L
   myname <- names(arglist)
   if (is.null(myname))
      myname <- ""
   arglist <- arglist[nchar(myname)>0]
   if (!.lgrep("(caption|ann(otation)*)\\.label",myname))
      arglist[["caption.label"]] <- txt
   ct <- vector("list",ng)
  # names(ct) <- if (isList) ln else ""
   if (length(ct)==length(units))
      names(ct) <- units
   else
      names(ct) <- rep("",length(ct))
   ll <- do.call("compose_graticule",arglist)
   coast <- do.call("compose_coastline",arglist)
   for (j in seq(ng))
   {
      if (isList) {
         obj <- img[[j]]
         p <- do.call("colorize",c(list(obj),arglist))
         ct[[j]] <- p$colortable
         nl <- nband(obj)
      }
      else if (!isRGB) {
        # obj <- img
         p <- do.call("colorize",c(list(img),arglist))
         ct[[j]] <- p$colortable
         nl <- nband(p)
      }
      if (isRGB)
         nl <- 1
      for (i in seq(nl))
      {
         if ((verbose)&&(k==0)) {
            pb <- ursaProgressBar(min=0,max=ifelse(isRGB,nl,nb))
            setUrsaProgressBar(pb,k)
         }
         k <- k+1L
        # if (i %in% skip)
        #    next
         panel_new(...)
         if (isRGB) {
            panel_raster(img,...)
           # panel_raster(colorize(obj,...),...)
         }
         else {
           # panel_raster(colorize(obj[i],...),...)
            if (isList) {
              # p <- do.call("colorize",c(list(obj[i]),arglist))
              # ct[[j]] <- p$colortable
              # do.call("panel_raster",c(list(p),arglist))
               do.call("panel_raster",c(list(p[i]),arglist))
            }
            else {
               do.call("panel_raster",c(list(p[i]),arglist))
            }
         }
         if (FALSE) {
            do.call("panel_coastline",arglist)
            do.call("panel_graticule",arglist)
         }
         else {
            panel_coastline(coast)
            panel_graticule(ll)
         }
         do.call("panel_annotation",arglist) ## only through 'do.call'
         do.call("panel_scalebar",arglist) ## panel_scalebar(...)
         if (verbose)
            setUrsaProgressBar(pb,k)
      }
   }
   if (verbose)
      close(pb)
   if (isRGB)
      return(NULL)
   if (FALSE) {
     # compose_legend(...)
     # compose_legend(img,...)
      do.call("compose_legend",c(list(ct),arglist))
     # do.call("compose_legend",c(img,arglist)) ## FAIL
      return(NULL)
   }
   ct
}
