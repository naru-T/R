###Collections of useful r functions
#when using this functions, 
#load alike: 
#source("https://raw.githubusercontent.com/naru-T/Rscripts/master/Misc/useful_functions.r")

##NA count function ------------------------------------------------
nacount=function(x){sum( is.na(x) ) }

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

##Write data as a geopackage format --------------------------------------------------
##based on tmaptools::write_shape

write_gpkg <- function (shp, file) {
  shpname <- deparse(substitute(shp))
  dir <- dirname(file)
  base <- basename(file)
  if (!file.exists(dir)) 
    stop("unknown directory", call. = FALSE)
  if (substr(base, nchar(base) - 4, nchar(base)) == ".gpkg") 
    base <- substr(base, 1, nchar(base) - 5)
  if (inherits(shp, c("sf", "sfc"))) 
    shp <- as(shp, "Spatial")
  if (!inherits(shp, "Spatial")) 
    stop("shpname is not a Spatial object", call. = FALSE)
  rgdal::writeOGR(obj=shp, dsn = dir %+% "/" %+% file, layer = base, driver = "GPKG", overwrite_layer = TRUE)
}


##opposite of %in% ---------------------------------------------------------------
#https://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))

##spatial mean function based on k nearest neighbors ----------------------------
spatialmean_func <- function(shp, varname, nb){
  shp_nacount <- apply(shp@data[,varname],1,is.na)
  if(!all(shp_nacount==FALSE)){
  
    for(i in 1:table(shp_nacount)[2]){
    nb_specific <- nb[[which(shp_nacount)[i]]]

      shp@data[which(shp_nacount)[i], varname] <- shp@data[nb_specific,] %>% 
        as.matrix() %>% 
        c() %>% 
        mean(.,na.rm=TRUE)
    }
  }
  return(shp)
}
