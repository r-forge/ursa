# version 3.6-1

* Added 'session_pngviewer()' and 'session_tempdir()' to follow CRAN policy.
     If "Rscript", then external software is used to open PNG;
        current directory is used to write files
     If 'interactive()' or "R CMD BATCH", no external software for PNG;
        'tempdir()' is used to write files

# version 3.5-2

* Initial submission to R-Forge
