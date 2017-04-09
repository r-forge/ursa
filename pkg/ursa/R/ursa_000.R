'ursa' <- function(obj,attr,...) {
   if (missing(obj)) {
      return(as.ursa(attr,...))
   }
   if (missing(attr)) {
      if (is.character(obj)) {
         if (.lgrep("grid",obj))
            return(session_grid())
         if (.lgrep("(proj|crs)",obj))
            return(session_proj4())
         if (.lgrep("(cell)",obj))
            return(session_cellsize())
      }
      return(as.ursa(obj,...))
   }
   if (!is.character(attr)) {
      return(as.ursa(obj,attr,...))
   }
   if (is.array(obj))
      return(as.ursa(obj))
   if (is.matrix(obj))
      return(as.ursa(obj))
   if (is.numeric(obj))
      return(as.ursa(obj))
   if (.is.ursa_stack(obj)) {
      if (.lgrep("brick",attr))
         return(ursa_brick(obj))
      return(NULL)
   }
   if (!is.ursa(obj))
      return(obj)
   if (.lgrep("grid",attr))
      return(ursa_grid(obj))
   if (.lgrep("con",attr))
      return(.ursa_connection(obj))
   if (.lgrep("(proj|crs)",attr))
      return(ursa_proj(obj))
   if (.lgrep("val",attr))
      return(ursa_value(obj,...))
   if (.lgrep("(color|ct)",attr))
      return(ursa_colortable(obj))
   if (.lgrep("(categ|class)",attr))
      return(names(ursa_colortable(obj)))
   if (.lgrep("^table",attr))
      return(as.table(obj))
   if (.lgrep("cell",attr))
      return(with(ursa_grid(obj),sqrt(resx*resy)))
   if (.lgrep("name",attr))
      return(bandname(obj))
   if (.lgrep("^dim$",attr))
      return(dim(obj))
   if (.lgrep("(extent|bbox)",attr))
      return(with(ursa_grid(obj),c(xmin=minx,ymin=miny,xmax=maxx,ymax=maxy)))
   if (.lgrep("(ncol|columns|samples)",attr)) {
      print("HERE")
      return(ursa_grid(obj)$columns)
   }
   if (.lgrep("(nrow|rows|lines)",attr))
      return(ursa_grid(obj)$rows)
   if (.lgrep("(info|meta(data)*)",attr))
      return(ursa_info(obj))
   return(NULL)
}
'ursa<-' <- function(obj,attr,...,value) {
   if (missing(obj))
      return(obj)
   if (!is.ursa(obj))
      return(obj)
   if (missing(attr))
      return(obj)
   if (.lgrep("grid",attr)) {
      ursa_grid(obj) <- value
      return(obj)
   }
   if (.lgrep("(proj|crs)",attr)) {
      ursa_proj(obj) <- value
      return(obj)
   }
   if (.lgrep("val",attr)) {
      ursa_value(obj,...) <- value
      return(obj)
   }
   if (.lgrep("(color|ct)",attr)) {
      ursa_colortable(obj) <- value
      return(obj)
   }
   if (.lgrep("(categ|class)",attr)) {
      names(ursa_colortable(obj)) <- value
      return(obj)
   }
   if (.lgrep("(name)",attr)) {
      bandname(obj) <- value
      return(obj)
   }
   return(NULL)
}
