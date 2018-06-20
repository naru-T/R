###Collections of useful r functions


## Paste function --------------------------------------------------
##useful paste function from
##https://twitter.com/Keiku/status/700503152081051649
##https://twitter.com/nick_harding/status/400686208755322881

#define function
`%+%` <- function(x,y){ paste0(x,y)}

## Example
#"A" %+% "B"%+% "C"
#"hoge_" %+% c("A" %+% 1:4 , "B" %+% 1:4)


## Remove NA included rows in sp data  --------------------------------------------------
# remove na rows by taking a great script given at http://gis.stackexchange.com/questions/89512/r-dealing-with-missing-data-in-spatialpolygondataframes-for-moran-test
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

#Example
#data.sp <- sp.na.omit(data.sp)


##opposite of %in%
#https://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))
