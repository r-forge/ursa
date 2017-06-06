# version 3.7-11

* geocoding for 180-longitute-objects is more correct, but traffic is higher

* alternate geocoding service in the case of base one failure

# version 3.7-10

* Adapation glance() for if argument "dsn" is "point" 'c(lon,lat)' and
  "boundary" 'c(minx,miny,maxx,maxy)'

# version 3.7-9

* Vectors without data table - fixed

# version 3.7-8

* Bypass for 'rgdal' usage diring examples on r-forge UNIX building machine.
  E.g.:
  Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/rforge/lib/R/3.4/rgdal/libs/rgdal.so':
  libgdal.so.1: cannot open shared object file: No such file or directory

# version 3.7-7

* Better matching for floating-point coordinates

# version 3.7-6

* Minor fixes for categories after resample

* 'glance()' is recoded

# version 3.7-5

* Adaptation for R-exts' "5.4 Registering native routines" for R-3.4.0.

# version 3.7-4

* Non-ascii for geocoding in 'glance'

* New function 'get_earthdata' for retreiving MODIS mosaics.

* Added package 'jpeg' in the category 'Imported'.

# version 3.7-3

* Non-ascii for geocoding in 'glance'

# version 3.7-2

* Introduce geocode to 'glance'. There is no relation between data and geocoded place.

* Introduce tiles to 'glance'. Now static maps and tiles for basemap in "+proj=merc"

* Dismiss from dQuote() and sQuote(), which put non-symmetrical quotes in Rgui;
   GDAL does't understad it.

* 'inst/glance' contains mock-up to create vector/raster file associantion with glance()

* 'glance' can work without package 'sf'; however "package:methods" should be in "search()"

* Rename 'panel_gridline' to 'panel_graticule'.

# version 3.7-1

* Public wrapper 'glance()' for non-public '.glance()':
     quick-look of GIS raster and vector data

# version 3.6-3

* Documentation for 'ursaProgressBar'

# version 3.6-2

* Added argument "..." to function 'read_gdal'.
      Now, if 'as.ursa(uri)' or 'display(uri)',
           then additional arguments can be passed to 'download.file'.
      For example, if you need 'mode="wb"' or ignore certificate for HTTPS

# version 3.6-1

* Added 'session_pngviewer()' and 'session_tempdir()' to follow CRAN policy.
     If "Rscript", then external software is used to open PNG;
        current directory is used to write files
     If 'interactive()' or "R CMD BATCH", no external software for PNG;
        'tempdir()' is used to write files

# version 3.5-2

* Initial submission to R-Forge
