###spatial data mapping

###bing image mapping
library(OpenStreetMap)
#latmin <- XXX
#longmin <- XXX
#latmax <- XXX
#lonmax <- XXX

#ex
latmin <- 44.6665715
longmin <- 84.49976 
latmax <- 44.7501149
lonmax <- 84.58357

##12 is a zoom level
bingimage <- openmap(c(latmin, longmin),
        c( latmax, lonmax), 12, 'bing')

plot(bingimage)

##plot as background in spplot
  #yourcrs =XXX
#ex
yourcrs <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
  rs <- raster(bingmap)
 # plotRGB(rs)
 # summary(rs)
  rs <- projectRaster(rs, crs=yourcrs)
  rgb <- rgb2spLayout(rs)
  
  nullrs <- rs[[1]]
  nullrs[] <-  1
  nullsp <- rasterToPolygons(nullrs, dissolve=TRUE)
  spplot(nullsp, fill=NA,col=NA,
                cex=0.4,
                colorkey=F,
                scales=list( tick.number=2, draw=TRUE,  tck = 0.5, cex=1),
                sp.layout=rgb)
