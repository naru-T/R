library(raster)
library(zoo)
library(signal)
library(forecast)


###if na values = -32768 
na_func <- function(x){
  x[x == -32768 ] <- NA
  return(as.vector(x))
}

 ####SG filter-----------
 ###https://matinbrandt.wordpress.com/2014/12/02/smoothingfiltering-a-ndvi-time-series-using-a-savitzky-golay-filter-and-r/
 ###http://gis.stackexchange.com/questions/173721/reconstructing-modis-time-series-applying-savitzky-golay-filter-with-python-nump

###if na values = -32768 
####parameters located inside need to be changed. 
fun_sgfilt <- function(x, na.rm=FALSE,...) {
   x[x == -32768 ] <- NA
     v=as.vector(x)
     
     if(length(which(is.na(v)==FALSE))>2){ 
     fill <- approxfun(v, y = NULL, method = 'linear', rule=2)
     rec <- which(is.na(v))
     v[rec] <- fill(rec)
     stk.ts2 <- ts(v, start=c(1982,1), end=c(2015,24), frequency=24)
     x <- sgolayfilt(stk.ts2, p=1, n=5)
     } else {
     x <- rep(NA, length(v)) 
   }
   return(x)
 }
 
 stk.sgfilt <- calc(stk, fun_sgfilt, filename="stk_sgfilt_p1n5ts24.tif", format="GTiff", overwrite=TRUE)

 ###if na values = -32768 
 ####na.interp -----------
 fun_interp <- function(x) {
   x[x == -32768 ] <- NA
   v <- as.vector(x)
   if (any(is.na(v)) || all(x == v[1], na.rm=TRUE)){
     z <- rep(NA, length(v))
   } else {
   z <- forecast::na.interp(v)}
   return(z)
   }
 
 stk.interp <- calc(stk, fun_interp, filename="stk_interp.tif",format="GTiff", overwrite=TRUE)
 