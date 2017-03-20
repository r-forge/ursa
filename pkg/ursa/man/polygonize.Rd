\name{polygonize}
\alias{polygonize}
\title{
Raster to vector (polygon) conversion.
}
\description{
Representing each raster cell as a polygon. In comparison to common GIS raster to vector conversion, where neighbor cells with the same value are combined to the single polygon, the number of output polygons is equal to number of non-\code{NA} values.
}
\usage{
polygonize(obj, fname, verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
Object of class \code{ursaRaster}.
}
  \item{fname}{
Missing or character. If specified, then ESRI Shapefile is created. Default is \link[base]{missing}.
}
  \item{verbose}{
Logical. If \code{TRUE} then convertion is attended by progress bar. Default is \code{TRUE}.
}
  \item{\dots}{
   Additional arguments, which are passed to internal function for writting ESRI Shapefile.
      \describe{
         \item{\code{compress}}{Logical. Should output ESRI Shapefile files be compressed by zip? Default is \code{FALSE}.}
   }
}
}
\details{
Some GIS software (e.g., QGIS) has broad tools for display vector data. Excepting choroplets, it is assumed that visualization of each cell separately is more attractive than displaying of polygons with different forms, which are produced, for example, by GDAL convertion utillity \href{http://www.gdal.org/gdal_polygonize.html}{gdal_polygonize.py}.

\code{polyigonize} uses \link[sp:SpatialPolygons]{"Spatial Polygons"} functions from \pkg{sp} package. ESRI Shapefile is created by \code{\link[rgdal]{writeOGR}} function from \pkg{rgdal} package.

}
\value{
If missing \code{fname}, then object of class "SpatialPolygonsDataFrame" (package \pkg{sp}).
\cr
If \code{fname} is specified, then \code{NULL}.

}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Implementation is very slow even for moderate image size. Use progress bar (\code{verbose=TRUE}) to control this process.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
a <- ursa_dummy(mul=1/16)
a <- a[a>100]
print(a)
print(band_mean(a))
b <- polygonize(a)
print(apply(b@data,2,mean,na.rm=TRUE))
print(with(ursa_grid(a),array(c(minx,miny,maxx,maxy),dim=c(2,2)
                             ,dimnames=list(c("x","y"),c("min","max")))))
print(b@bbox)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}
