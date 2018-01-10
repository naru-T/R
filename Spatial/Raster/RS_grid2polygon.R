library(raster)
library(geojsonio)

#test data import
  f <- system.file("external/test.grd", package="raster")
  r <- raster(f)

##100 random sampling points
  r.witch.non_na <- !is.na(r[])
  r.loc <- which(!is.na(r[]))
  s <- sample(r.loc, 100, replace = FALSE)
  r.sample <- r
  r.sample[setdiff(r.loc, s)] <- NA
  
##ckeck
  plot(r.sample)
  table(is.na(r.sample[]))

##convert pickuped raster grid to polygon
  poly <- rasterToPolygons(r.sample, dissolve = FALSE, na.rm=TRUE)

##check
  spplot(poly[1,])
  spplot(poly[1:5,])

#export
  writeOGR(poly, "export.geojson",layer="poly" ,driver="GeoJSON")
