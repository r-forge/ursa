\name{spatial_engine}
\alias{spatial_engine}
\alias{spatial_crs}
\alias{spatial_crs<-}
\alias{spatial_proj4}
\alias{spatial_proj4<-}
\alias{spatial_bbox}
\alias{spatial_data}
\alias{spatial_geometry}
\alias{spatial_geometry<-}
\alias{spatial_geotype}
\alias{spatial_transform}
\alias{spatial_coordinates}
\alias{spatial_fields}
\alias{spatial_area}
\title{
Wrapper functions for manipulation with non-raster objects
}
\description{
These wrappers return iniform properties or do consimilar manipulations for spatial objects of different types: simple features (package \pkg{sf}) and abstract class Spatial (package \pkg{sp}). Appropriate functionality (\emph{\dQuote{engine}}) of respective packages is used. 
 
}
\usage{
spatial_engine(obj, verbose = FALSE)

spatial_crs(obj, verbose = FALSE)
spatial_proj4(obj, verbose = FALSE)

spatial_crs(obj, verbose = FALSE) <- value
spatial_proj4(obj, verbose = FALSE) <- value

spatial_bbox(obj, verbose = FALSE)

spatial_data(obj, subset= ".+", drop = NA, verbose = FALSE)

spatial_geometry(obj, verbose = FALSE)
spatial_geometry(obj, verbose = FALSE) <- value

spatial_geotype(obj, verbose = FALSE)

spatial_transform(obj, crs, verbose = FALSE, ...)

spatial_coordinates(obj, verbose = FALSE)

spatial_fields(obj, verbose = FALSE)

spatial_area(obj, verbose = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Simple feature (package \pkg{sf}) or Spatial abstrac class (package \pkg{sp}) for all functions, excepting \code{spatial_geometry<-}. Data frame for \emph{Replace} function \code{spatial_geometry<-}.
}
  \item{crs}{
Projection EPSG code or projection PROJ.4 string.
}
  \item{subset}{
Pattern to field names (colnames) of attribute table (data frame) for subbsetting using \code{\link[base:grep]{regular expressions}}. By default, all fields are secected.
}
  \item{drop}{
Logical. Dropping column of data frame. If \code{TRUE}, then vector of data is returned. If \code{FALSE}, then structure of data is kept. Default is \code{NA}, which is interpreted as \code{TRUE} for single column and as \code{FALSE} for multiple columns.
}
  \item{value}{
Value for property assignment in \emph{replacement} functions. Either numeric EPSG code or character PROJ.4 string for \code{spatial_crs<-} and \code{spatial_proj4<-}. Spatial object or geometry of spatial object for \code{spatial_geometry<-}.
}
  \item{verbose}{
Logical. Value \code{TRUE} provides information on console. Default is \code{FALSE}.
}
  \item{\dots}{
Further arguments passed to \code{sf::st_transform} or to \code{sp::spTransform}}.
}

\value{
\code{spatial_engine} returns package name (character string \code{"sf"} or \code{"sp"}), which functionality is used for manipulation with spatial object \code{obj}.

\code{spatial_crs} and \code{spatial_proj4} are synonyms, The \emph{Extract} functions return projection string in the PROJ.4 notation; the \emph{Replace} functions change projection property of the object.

\code{spatial_bbox} returns numeric vector of length 4 with names "xmin", "ymin", "xmax" and "ymax".

\code{spatial_data} returns attribute table only, without geometry.

\code{spatial_geometry} (\emph{Extract} function) returns only geometry, which format is depended on class of \code{obj}.

\code{spatial_geometry<-} (\emph{Replace} function) addes geometry to the object.

\code{spatial_transform} does a transformation of spatial coordinates to the new CRS and returns object of the same class as class of \code{obj}.

\code{spatial_geotype} returns type of spatial data: "POINTS", "LINESTRINGS", "POLYGON", "MULTIPOLYGON", \dots.

\code{spatial_coordinates} returns simpliefied matrix or list of coordinates of original object.

\code{spatial_filds} returns column names of spatial attributive table.

\code{spatial_area} is valid for polygonal geometry. It returns area of polygons.

}
\references{
%% ~put references to the literature/web site here ~
Classes and methods in packages \pkg{\link[sf:st]{sf}} and \pkg{\link[sp:00sp]{sp}} help.
}
\author{
Nikita Platonov \email{platonov@sevin.ru}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
session_grid(NULL)
n <- 1e2
x <- runif(n,min=25,max=65)
y <- runif(n,min=55,max=65)
z <- runif(n,min=1,max=10)
da <- data.frame(x=x,y=y,z=z)
if (requireNamespace("sp")) {
   da.sp <- da
   sp::coordinates(da.sp) <- ~x+y
   sp::proj4string(da.sp) <- "+init=epsg:4326"
   print(spatial_bbox(da.sp))
   print(spatial_crs(da.sp))
}
if (requireNamespace("sf")) {
   da.sf <- sf::st_as_sf(da,coords=c("x","y"),crs=4326)
   print(spatial_bbox(da.sf))
   print(spatial_crs(da.sf))
}
}
\keyword{attribute}
