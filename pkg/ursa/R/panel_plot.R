'panel_plot' <- function(obj,...)
{
   if (.skipPlot(TRUE))
      return(NULL)
   if ((is.character(obj))&&(.lgrep("\\.shp(\\.zip)*$",obj)))
   {
      op <- options(warn=0)
      requireNamespace("rgdal")
      a <- .shp.read(obj)
     # a <- spTransform(a,session_grid()$proj4)
     # ret <- .panel_plot(a,add=TRUE,...)
      ret <- sp::plot(a,add=TRUE,...)
      options(op)
   }
   else if (inherits(obj,c("raster"))) {
         ret <- with(session_grid()
                    ,rasterImage(as.raster(obj),minx,miny,maxx,maxy,...))
   }
   else if (is.ursa(obj)) {
      ret <- panel_raster(obj,...)
   }
   else if (inherits(obj,"Spatial")) {
      ret <- sp::plot(obj,...)
   }
   else if (inherits(obj,"sfc")) {
      ret <- plot(obj,add=TRUE,...)
   }
   else if (inherits(obj,"sf")) {
     # ret <- plot(sf::st_geometry(obj),...)
      ret <- try(.panel_plot(sf::st_geometry(obj),add=TRUE,...))
   }
   else {
      ret <- try(.panel_plot(obj,add=TRUE,...))
      if (inherits(ret,"try-error")) {
         opW <- options(warn=1)
         warning(paste("Unable to call 'plot' method for class"
                      ,.sQuote(class(obj))
                      ,"\nIt seems that package 'methods' is required."))
         options(opW)
         ret <- NULL
      }
   }
   ret
}
'panel_box' <- function(...){
   if (.skipPlot(FALSE))
      return(NULL)
   bg <- sum(c(col2rgb(getOption("ursaPngBackground")))*c(0.30,0.59,0.11))
   if (!length(list(...)))
      box(lwd=0.5,col=ifelse(bg<128,"#FFFFFF7F","#0000007F"))
   else
      box(...)
}
'panel_lines' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   lines(...)
}
'panel_points' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   points(...)
}
'panel_text' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   text(...)
}
'panel_polygon' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   polygon(...)
}
'panel_abline' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   abline(...)
}
'panel_segments' <- function(...){
   if (.skipPlot(TRUE))
      return(NULL)
   segments(...)
}
'.panel_plot' <- function(obj,...){
   if (.skipPlot(TRUE))
      return(NULL)
   if (is.null(obj))
      return(obj)
  ## WARNING: here not only plot
  # pkg <- attr(class(obj),"package")
  # print(class(obj))
  # fun <- if (nchar(pkg)) paste0(pkg,"::","plot") else "plot"
  # if (nchar(pkg))
  #    requireNamespace(pkg)
  # require(methods)
   plot(obj,...)
  # do.call("plot",list(obj,arglist)) ## ,add=TRUE 
  # plot(obj,...) ## ,add=TRUE 
}
