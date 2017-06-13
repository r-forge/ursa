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
# https://leaflet-extras.github.io/leaflet-providers/preview/
# https://leaflet-extras.github.io/leaflet-providers/leaflet-providers.js
'.untile' <- function(z=4,x=10,y=3,server=""
                     ,osm=c('1'="osm",'2'="cycle",'3'="transport"
                              ,'4'="mapsurfer",'5'="sputnik",'6'="thunderforest"
                              ,'7'="carto")
                     ,ursa=FALSE,verbose=FALSE) {
   s <- list()
   s$mapnik <- "http://{abc}.tile.openstreetmap.org/{z}/{x}/{y}.png"
   s$cycle <- "http://{abc}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png"
   s$osmfr <- "http://{abc}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png"
   s$transport <- "http://{abc}.tile2.opencyclemap.org/transport/{z}/{x}/{y}.png"
   s$mapsurfer <- "http://korona.geog.uni-heidelberg.de/tiles/roads/x={x}&y={y}&z={z}"
   s$sputnik <- "http://tiles.maps.sputnik.ru/tiles/kmt2/{z}/{x}/{y}.png"
   s$thunderforest <- "https://{abc}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png"
  # https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=<insert-your-apikey-here> 
   s$carto <- "http://a.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"
   s$kosmosnimki <- "http://{abcd}.tile.osm.kosmosnimki.ru/kosmo/{z}/{x}/{y}.png"
   s$Esri.Ocean <- "https://services.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}.jpg"
   s$Esri.Topo <- "http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}.jpg"
   s$HERE.Aerial <- paste0("https://{1234}.aerial.maps.cit.api.here.com/maptile"
                          ,"/2.1/maptile/newest/hybrid.day/{z}/{x}/{y}/256/png8?"
                          ,"app_id=",getOption("HEREapp")$id
                          ,"&app_code=",getOption("HEREapp")$code,"&lg=eng")
   osmCr <- "\uA9 OpenStreetMap contributors"
   copyright <- rep(osmCr,length(s))
   names(copyright) <- names(s)
   copyright["mapnik"] <- paste0(osmCr)
   copyright["cycle"] <- paste(osmCr,"(Cycle)")
  # copyright["transport"] <- paste0("Maps \xA9 Thunderforest, Data ",osmCr)
   copyright["mapsurfer"] <- paste0(osmCr
           ,", GIScience Research Group @ Heidelberg University")
   copyright["sputnik"] <- paste0(osmCr,", \uA9 \u0420\u043E\u0441\u0442\u0435\u043B\u0435\u043A\u043E\u043C")
   copyright["thunderforest"] <- paste0("Maps \uA9 Thunderforest, Data ",osmCr)
   copyright["carto"] <- paste0(osmCr,", \uA9 CARTO")
   copyright["kosmosnimki"] <- paste0(osmCr,", \uA9 ScanEx")
   copyright["Esri.Ocean"] <- "\uA9 Esri: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri"
   copyright["Esri.Topo"] <- "\uA9 Esri - contributors to Esri World Topo Map"
   copyright["HERE.Aerial"] <- "Map \uA9 1987-2014 HERE"
   copyright["osmfr"] <- paste("\uA9 Openstreetmap France",osmCr)
   if (!(server %in% names(s))) {
      ret <- names(s)
      attr(ret,"copyright") <- copyright
      return(ret)
   }
   tile <- .gsub("{z}",z,.gsub("{y}",y,.gsub("{x}",x,s[[server]])))
   if (.lgrep("\\{..+}",tile)) {
      dom <- unlist(strsplit(.gsub2("\\{(.+)\\}","\\1",gsub("\\{.\\}","",tile)),""))
      tile <- .gsub("{.+}",sample(dom,1),tile)
   }
   fext <- .gsub("^.+\\.(.+)$","\\1",basename(tile))
   if (!(fext %in% c("png","jpg","jpeg"))) {
      if ((.lgrep("png8",basename(tile)))||
             (server %in% c("mapsurfer")))
         fext <- "png"
      else if (server %in% c("nosuchnameyet"))
         fext <- "jpg"
      else {
         cat(paste("Unable to detect either 'png' or 'jpg' format of tile:"
                  ,s[[server]],"\n"))
         stop()
      }
   }
   fname <- tempfile(fileext=".tile")
   a <- try(download.file(tile,fname,mode="wb",quiet=!verbose))
   if (inherits(a,"try-error")) {
      message(a)
      stop()
   }
  # message(tile)
  # download.file(tile,fname,method="curl",mode="wb",quiet=FALSE
  #              ,extra="-H Accept-Language:de")
   if (fext %in% c("png"))
      a <- 255*png::readPNG(fname)
   else if (fext %in% c("jpg","jpeg"))
      a <- 255*jpeg::readJPEG(fname)
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
