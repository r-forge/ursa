require(plutil)
invisible({
   rd <- filelist("\\.Rd$")
   ex <- mygsub("\\.Rd$",".ex.R",rd)
   ind <- which(!file.exists(ex))
   if (length(ind))
      for (i in ind)
         file.copy("___prompt.R",ex[i])
})
