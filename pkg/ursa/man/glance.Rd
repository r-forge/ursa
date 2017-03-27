\name{glance}
\alias{glance}
\alias{.glance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Command line utility for spatial view of raster or vector GIS file.
}
\description{
\code{glance} is a parser of command line arguments for non-public function \code{.glance}, which creates multi-panel plots for each attribute of vector file or for each band of raster file.
}
\usage{
glance(...)

# non-public
.glance(dsn, layer = ".*", grid = NULL, attr = ".+", len = 640, expand = 1.05,
        border = 15, lat0 = NA, lon0 = NA, resetProj = FALSE,
        proj = c("auto", "stere", "laea", "merc", "google", "longlat"),
        feature = c("auto", "attribute", "geometry"),
        basemap = c("after","before"), saturation = NA, alpha = NA, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{
Character or object of either \code{ursaRaster}, \code{Spatial}, or \code{sf} classes. If character, then data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database). If \code{Spatial} then package \pkg{methods} should be loaded before.
}
  \item{layer}{
Character or integer. If integer, then layer index. If character, then pattern (\link[base:regex]{regular expressions}) to recognize layer by name. Only one layer selection is allowed. If selected more then one layer, the error message contains indices and names of layers. Usually, datasets (e. g., "ESRI Shapefile") have only one layer. Default is \code{".*"}; interpreted as all layers.
}
  \item{grid}{
Object of class \code{ursaGrid} or NULL. Reference CRS and boundary box for visualization. If \code{NULL}, then CRS and boundary box are zoomed to layer. Default is \code{NULL}.
}
  \item{attr}{
Character. Pattern for attribute selection by name using \link[base:regex]{regular expressions}. Multiple selection is allowed. Default is \code{".+"}; all attributes.
}
  \item{len}{
Integer. Width of panel for plotting in pixels. Default is \code{640}.
}
  \item{expand}{
Numeric. Multiplier for plotting panel zoom in relation to extent of plotting geometry. Default is \code{1.05}.
}
  \item{border}{
Integer. Value in pixels of fixed margins around plotting geometry. Default is \code{15L}.
}
  \item{lat0}{
Numeric. Parallel os zero distortion. If \code{NA}, then parallel os zero distortion is determed from object geometry. Actual for \code{"+proj=stere"} projections. Default is \code{NA}.
}
  \item{lon0}{
Numeric. Central meridian, which have vertical direction on the plot. If \code{NA}, then central meridian is determed from object geometry. Default is \code{NA}.
}
  \item{resetProj}{
Logical. Value \code{TRUE} overwrites projection of vector file. Default is \code{FALSE}.
}
  \item{proj}{
Character. Projection class (\code{"stere"}, \code{"laea"}, \code{"merc"}, \code{"longlat"}) or keyword. Default is keyword \code{"auto"}; use object projection, if this projection differs from projection class \code{"+longlat"}, otherwise, projection (\code{"stere"} or \code{"merc"}) is determed internally. Keyword \code{"google"} forces to use Google Static Map for basemap; the resulted projection has class \code{"+proj=merc"}.
}
  \item{feature}{
Character. Appearance of visualization. If \code{"attribute"} then each panel attribute is plotted on separate panel (number of panels is equal to number of columns in attribute table). If \code{"geometry"} then each feature is plotted on separate panel (number of panels is equal to number of rows in attribute table). Default is \code{"auto"}; if intersects of features are found, then \code{"geometry"} is used, else "attribute".
}
   \item{basemap}{
Character. The order of basemap layer rendering in the case of \code{proj="google"}.If \code{"before"}, then basemap is plotted before object plot. If \code{"after"}, then basemap is plotted over object. 
}
   \item{saturation}{
Character. The saturation of basemap in the case of \code{proj="google"}. Default is \code{NA}; \code{saturation=0.5} for \code{basemap="before"} and \code{saturation=0.35} for \code{basemap="after"}.
}
   \item{alpha}{
Character. The opacity of plotted object. Default is \code{NA}; 0.75 for \code{basemap="before"} and \code{project="google"}, 1 - in all other cases.
}
  \item{verbose}{
Logical. Logical. Value \code{TRUE} may provide some additional information on console. Default is \code{FALSE}.
}
  \item{\dots}{
\code{glance}: Arguments, which are passed to \code{.glance} or to \code{\link[ursa:display]{display}}.
\cr\code{.glance}: Arguments, which are passed to \code{\link[ggmap:get_googlemap]{get_googlemap}}, \code{\link[ursa:colorize]{colorize}}, \code{\link[ursa:display]{display}}, \emph{etc}.

Keywords for \code{proj="google"} to form request to Google Static Maps:
   \tabular{lll}{
      \strong{Argument} \tab \strong{Default value} \tab \strong{Comment}
      \cr \code{"maptype"} \tab \code{"terrain"} \tab Allowed values in \code{c("terrain", "satellite")}
      \cr \code{"language"} \tab \code{"en-EN"} \tab
      \cr \code{"color"} \tab \code{"bw"} \tab Allowed values in \code{c("bw", "color")}
      \cr \code{"scale"} \tab \code{1} \tab valid values are in \code{c(1,2)}.
      \cr \code{"zoom"} \tab defined from object geometry \tab
   }
}
}
\details{
Command line usage implies set of arguments using pair: argument \emph{\verb{name}} and argument \emph{\verb{value}}. and values in the format "\emph{\verb{[name1=]value1 [name2]=value2}}". No spaces around \emph{\verb{=}} (equal symbol). Argument \emph{\verb{name}} can be omitted, symbol \emph{\verb{=}} is omitted too. If argument \emph{\verb{value}} has spaces, then argumetn value should be surrounded by double quotes (\code{fname="my test.shp"}). If argument \emph{\verb{value}} is matched to \R function, then such value should be surrounded by single quotes (\code{layer='density'}).

Command line usage example: \code{Rscript -e ursa::glance() 'final_more_than_032.sqlite' attr="select" resetProj=TRUE expand=1.5}

For OS Windows, bat-file can be created for raster and vector file association:
\code{Rscript -e ursa::glance() \%*}

Command line usage implies external software for PNG view \code{\link[ursa:session]{session_pngviewer}(TRUE)}.
}
\value{
\code{glance} returns integer: \code{0L} - successful, \code{10L} - call without arguments, \code{1} - suggested package \pkg{sf} is not installed
}
%%~ \references{
%%~ %% ~put references to the literature/web site here ~
%%~ }
\author{
Nikita Platonov \email{platonov@sevin.ru}
}
\note{
Package \pkg{sf} is 'Suggested' for package \pkg{ursa}. But it is requiered for function \code{.glance}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%~ \seealso{
%%~ %% ~~objects to See Also as \code{\link{help}}, ~~~
%%~ }
\examples{
session_grid(NULL)
f <- system.file("vectors","scot_BNG.shp",package="rgdal")
glance(f,proj="merc",attr="(NAME|COUNT)")
cmd <- paste("Rscript --vanilla -e ursa::glance()",dQuote(f)
            ,paste0("proj=",dQuote("merc"))
            ,paste0("attr=",dQuote("(lon|lat)")))
cat(" --------- Try in command line: -----------\n")
message(cmd)
cat(" ----------- end of quoting ---------------\n")


\dontrun{
system(cmd)}


\dontrun{

require(sp)
a <- data.frame(lat=56.74,lon=37.16,city="Dubna")
coordinates(a) <- ~lon+lat
proj4string(a) <- "+init=epsg:4326"
glance(a,proj="google",color="color",basemap="before",saturation=1,expand=50)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
