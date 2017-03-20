'.ursaToolSetDummyFunction' <- function() NULL
#try(Sys.setenv(R_PLASTER_TEMPLATE=
#     file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")))
# try(Sys.setenv(R_PLASTER_TEMPLATE=system.file("inst","template",package="ursa")

.onLoad <- function(lib, pkg) {
  # print("ursa -- .onLoad")
   p <- proc.time()
   options(ursaTimeStart=p,ursaTimeDelta=p)
  # welcome2 <- .elapsedTime("ursa -- onload 1111",toPrint=FALSE)
   fpath <- file.path(chartr("\\","/",Sys.getenv("R_USER")),"template.idr")
   if (file.exists(fpath)) {
      ok <- try(Sys.setenv(R_RMAP_TEMPLATE=fpath))
      if (!inherits(ok,"try-error"))
         return(invisible(0L))
   }
   fpath <- system.file("template",package="ursa")
   try(Sys.setenv(R_RMAP_TEMPLATE=fpath))
   invisible(0L)
}
.onAttach <- function(lib, pkg) { ## FAILED for 'Rscript -e "ursa::display()"'
  # print("ursa -- .onAttach")
  # welcome <- .elapsedTime("ursa -- attach 2222",toPrint=FALSE)
  # packageStartupMessage(welcome,appendLF=FALSE)
   invisible(0L)
}
.Last.hide <- function() {
   message("ursa -- last")
   if (!FALSE)
   {
      delafter <- getOption("ursaPngDelafter")
      fileout <- getOption("ursaPngFileout")
      if ((is.logical(delafter))&&(is.character(fileout))&&(delafter)&&(file.exists(fileout)))
      {
        # dev.off()
         graphics.off()
        if (!file.remove(fileout))
           message(sprintf("'ursa' package message: Unable to remove file '%s'.",fileout))
      }
   }
   con <- showConnections(all=!FALSE)
   ind <- which(!is.na(match(con[,"class"],"file")))
   if ((!FALSE)&&(length(ind)))
   {
      con <- con[ind,,drop=FALSE]
      for (i in seq(nrow(con)))
      {
         con2 <- con[i,,drop=FALSE]
        # close(getConnection(as.integer(rownames(con2)))) ## del
         fname <- con2[,"description"]
         if (length(grep("\\.unpacked(.*)\\~$",fname)))
         {
            close(getConnection(as.integer(rownames(con2)))) ## ins
            if (!file.remove(fname))
               message(sprintf("'ursa' package message: Unable to remove file '%s'."
                              ,fname))
         }
      }
   }
}
.onUnload <- function(libpath) {
  # message("ursa -- unload")
   library.dynam.unload("ursa",libpath)
}
.onDetach <- function(libpath) {
  # message("ursa -- detach")
}
