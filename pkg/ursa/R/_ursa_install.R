'.buildAndInstall' <- function() {
   wd <- setwd("C:/platt/R/ursa-package")
   system("R --vanilla CMD build ursa")
   pkg <- tail(plutil::filelist("^ursa_.*\\.tar\\.gz$"))
   if (length(pkg)) {
      system(paste("R --vanilla CMD INSTALL",pkg)) ## --no-multiarch
      file.remove(pkg)
   }
   setwd(wd)
   NULL
}
invisible({
  # .a <- .argv0()
   .a <- basename(strsplit(commandArgs(FALSE)[4],"=")[[1]][2])
   if ((!is.na(.a))&&(.a=="_ursa_install.R"))
      .buildAndInstall()
   else {
     # print("mysource")
     # try(Sys.setenv(R_RMAP_TEMPLATE=
     #     file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")))
   }
   rm(.a)
   NULL
})
