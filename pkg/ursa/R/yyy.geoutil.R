## "http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames", license?
'.deg2num' <- function(lat,lon,zoom,verbose=FALSE) {
   lat_rad <- lat*pi/180
   n <- 2^zoom
   xtile <- floor((lon+180)/360*n)
   ytile <- floor((1-log(tan(lat_rad)+(1/cos(lat_rad)))/pi)/2*n)
   if (TRUE)
      return(c(xtile,ytile))
   osm <- paste0("http://",letters[sample(seq(3),1)],".tile.openstreetmap.org")
   tile <- paste0(paste(osm,zoom,xtile,ytile,sep="/"),".png")
   message(tile)
   fname <- "tile.png"
   download.file(tile,fname,mode="wb",quiet=!verbose)
   return(tile)
}
'.untile' <- function(z=4,x=10,y=3,server=""
                     ,osm=c('1'="osm",'2'="cycle",'3'="transport"
                              ,'4'="mapsurfer",'5'="sputnik",'6'="thunderforest"
                              ,'7'="carto")
                     ,ursa=FALSE,verbose=FALSE) {
   s <- list()
   s$openstreetmap <- paste0("http://",letters[sample(seq(3),1)]
                  ,".tile.openstreetmap.org/{z}/{x}/{y}.png")
   s$cycle <- paste0("http://",letters[sample(seq(3),1)]
                    ,".tile.opencyclemap.org/cycle/{z}/{x}/{y}.png")
   s$transport <- paste0("http://",letters[sample(seq(3),1)]
                        ,".tile2.opencyclemap.org/transport/{z}/{x}/{y}.png")
   s$mapsurfer <- "http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}"
   s$sputnik <- "http://tiles.maps.sputnik.ru/tiles/kmt2/{z}/{x}/{y}.png"
   s$thunderforest <- paste0("https://",letters[sample(seq(3),1)]
                            ,".tile.thunderforest.com/landscape/{z}/{x}/{y}.png")
   s$carto <- "http://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"
   s$kosmosnimki <- paste0("http://",letters[sample(seq(4),1)]
                          ,".tile.osm.kosmosnimki.ru/kosmo/{z}/{x}/{y}.png")
   osmCr <- "\uA9 OpenStreetMap contributors"
   copyright <- rep(osmCr,length(s))
   names(copyright) <- names(s)
   copyright["openstreetmap"] <- paste0(osmCr)
   copyright["cycle"] <- paste(osmCr,"(Cycle)")
  # copyright["transport"] <- paste0("Maps \xA9 Thunderforest, Data ",osmCr)
   copyright["mapsurfer"] <- paste0(osmCr
           ,", GIScience Research Group @ Heidelberg University")
   copyright["sputnik"] <- paste0(osmCr,", \uA9 \u0420\u043E\u0441\u0442\u0435\u043B\u0435\u043A\u043E\u043C")
   copyright["thunderforest"] <- paste0("Maps \uA9 Thunderforest, Data ",osmCr)
   copyright["carto"] <- paste(osmCr,"\uA9 CARTO")
  # copyright["kosmosnimki"] <- paste0(osmCr)
   if (!(server %in% names(s))) {
      ret <- names(s)
      attr(ret,"copyright") <- copyright
      return(ret)
   }
   tile <- .gsub("{z}",z,.gsub("{y}",y,.gsub("{x}",x,s[[server]])))
   fname <- tempfile(fileext=".png")
   download.file(tile,fname,mode="wb",quiet=!verbose)
  # message(tile)
  # download.file(tile,fname,method="curl",mode="wb",quiet=FALSE
  #              ,extra="-H Accept-Language:de")
   a <- 255*png::readPNG(fname)
   file.remove(fname)
   if (TRUE) {
      dima <- dim(a)
      a <- as.integer(c(a))
      dim(a) <- dima
   }
   if (!ursa)
      return(a)
   epsg3857 <- paste("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0"
                    ,"+lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m"
                    ,"+nadgrids=@null +wktext +no_defs")
   n <- 2^z
   lon <- (x+c(0,1))/n*360-180
   lat <- atan(sinh(pi*(1-2*(y+c(0,1))/n)))*180/pi
   xy <- .project(cbind(lon,rev(lat)),epsg3857)
   dima <- dim(a)
   g1 <- regrid(ursa_grid(),setbound=c(xy)[c(1,3,2,4)]
               ,columns=dima[2],rows=dima[1],proj4=epsg3857)
   b <- as.integer(255/255*as.ursa(a,aperm=TRUE,flip=TRUE))
   ursa(b,"grid") <- g1
  # session_grid(b)
  # display(b,scale=1,coast=FALSE)
   b
}
