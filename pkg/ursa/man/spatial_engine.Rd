\name{spatial_engine}
\alias{spatial_engine}
\alias{spatial_crs}
\alias{spatial_proj4}
\alias{spatial_bbox}
\alias{spatial_data}
\alias{spatial_geometry}
\alias{spatial_transform}
\title{
Wrapper functions for manipulation with non-raster objects
}
\description{
These wrappers return iniform properties or do consimilar manipulations for spatial objects of different types: simple features (package \pkg{sf}) and abstract class Spatial (package \pkg{sp}). Appropriate functionality (\emph{\dQuote{engine}}) of respepective packages is used. 
 
}
\usage{
spatial_engine(obj, verbose = FALSE)

spatial_crs(obj, verbose = FALSE)
spatial_proj4(obj, verbose = FALSE)

spatial_bbox(obj, verbose = FALSE)

spatial_data(obj, verbose = FALSE)

spatial_geometry(obj, verbose = FALSE)

spatial_transform(obj, crs, verbose = FALSE, ...)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Simple feature (package \pkg{sf}) or Spatial abstrac class (package \pkg{sp}).
}
  \item{crs}{
Projection EPSG code or projection PROJ.4 string.
}
  \item{verbose}{
Logical. Value \code{TRUE} provides information on console. Default is \code{FALSE}.
}
  \item{\dots}{
Further arguments passed to \code{sf::st_transform} or to \code{sp::spTransform}}.
}

\value{
\code{spatial_engine} returns package name (character string \code{"sf"} or \code{"sp"}), which functionality is used for manipulation with spatial object \code{obj}.

\code{spatial_crs} and \code{spatial_proj4} are synonyms; they return projection string in the PROJ.4 notation.

\code{spatial_bbox} returns numeric vector of length 4 with names "xmin", "ymin", "xmax" and "ymax".

\code{spatial_data} returns attribute table only, without geometry.

\code{spatial_geometry} returns only geometry, which format is depended on class of \code{obj}.

\code{spatial_transform} does a transformation of spatial coordinates to the new CRS and returns object of the same class as class of \code{obj}.

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
