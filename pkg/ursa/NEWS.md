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
