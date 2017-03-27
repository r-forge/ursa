invisible({
   plutil::mysource(ursa)
   session_grid(NULL)
   if (includeToExampe <- TRUE) {
      require(sp)
      a <- data.frame(lat=56.74,lon=37.16,city="Dubna")
      coordinates(a) <- ~lon+lat
      proj4string(a) <- "+init=epsg:4326"
      glance(a,proj="google",color="color",basemap="before",saturation=1,expand=50)
   }
   q()
   f <- system.file("vectors","scot_BNG.shp",package="rgdal")
   glance(f,proj="merc",attr="(NAME|COUNT)")
   cmd <- paste("Rscript --vanilla -e ursa::glance()",dQuote(f)
               ,paste0("proj=",dQuote("merc"))
               ,paste0("attr=",dQuote("(lon|lat)")))
   cat(" --------- Try in command line: -----------\n")
   message(cmd)
   cat(" ----------- end of quoting ---------------\n")
   if (includeToExampe <- !TRUE) {
      system(cmd)
   }
})
