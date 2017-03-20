'head.ursaRaster' <- function(x,n=3L,...)
{
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   m <- if (n>k) k else n
   x[1:m]
}
'tail.ursaRaster' <- function(x,n=3L,...)
{
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   m <- if (n>k) k else n
  # str(x)
  # print(k-(m:1)+1)
   x[k-(m:1)+1]
}
'series' <- function(x,n=3L,s=170,...)
{
   s2 <- with(ursa_grid(x),columns*rows*4)
   m2 <- floor(s*1024*1024/2/s2)
   if (m2<n)
      n <- m2
   if (!is.na(x$con$posZ[1L]))
      k <- length(x$con$posZ)
   else
      k <- x$dim[2]
   m <- if (n>k) k else n
   m <- sort(unique(c(1:m,k-(m:1)+1)))
   x[m]
}
