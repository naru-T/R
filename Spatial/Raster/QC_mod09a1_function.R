### Quality check function for MOD09A1 MODIS data
### Author & copyroght: Naru. Tsutsumida (naru@kais.kyoto-u.ac.jp)
### ver.1 (2 Apr. 2015)
### qcrst is a raster class file, representing quality check
# see the detail of qc binary difinition: https://lpdaac.usgs.gov/products/modis_products_table/mod09a1


#Quality check for each band by qcmask_mod09a1_band function
#1  Highest quality 
#2  dead detector; data interpolated in L1B
#3  solar zenith >= 86 degrees 
#4  solar zenith >= 85 and < 86 degrees
#5  mising input
#6  internal constant used in place of climatological data for at least one atmospheric constant
#7  correction out of bounds pixel constrained to extreme allowable value
#8  L1B data faulty
#9  not processed due to deep ocean or clouds

#Quality check for each band by qcmask_mod09a1_ov
#1  corrected product produced at ideal quality all bands
#2  corrected product produced at less than ideal quality some or all bands
#3  corrected product not produced due to cloud effects all bands
#4  corrected product not produced due to other reasons some or all bands may be fill value [Note that a value of (11) overrides a value of (01)].


qcmask_mod09a1_ov <- function(qcrst){
  ii <- which(!is.na(qcrst[])) #This is for creating mask 
  qcbit <- qcrst[ii]
  overall <- rep(NA, length(qcrst[]))
  for ( j in 1:length(ii)){
    binary <- as.integer(intToBits(qcbit[j]))
    # overall assessment
    if (binary[ 1 ] == 0 & binary[ 2 ] == 0 ) 
      overall[j] <- 1 ## (0,0) corrected product produced at ideal quality all bands
    else if (binary[ 1 ] == 0 & binary[ 2 ] == 1 )
      overall[j] <- 2 ## (0,1) corrected product produced at less than ideal quality some or all bands
    else if (binary[ 1 ] == 1 & binary[ 2 ] == 0 )
      overall[j] <- 3 ## (1,0) corrected product not produced due to cloud effects all bands
    else if (binary[ 1 ] == 1 & binary[ 2 ] == 1 )
      overall[j] <- 4 ## (1,1) corrected product not produced due to other reasons some or all bands may be fill value [Note that a value of (11) overrides a value of (01)].
  }
  # create raster
  ov.rst <- qcrst
  ov.rst[ii] <- overall
  
  return(ov.rst)
}



qcmask_mod09a1_band <- function(qcrst, band){
  ii <- which(!is.na(qcrst[])) #This is for creating mask 
  qcbit <- qcrst[ii]
  mask <- rep(NA, length(qcrst[]))
  for ( j in 1:length(ii)){
    binary <- as.integer(intToBits(qcbit[j]))
    
    #For each band
    if (binary[ 4* band -1 ] == 0 & binary[ 4* band ] == 0 & binary[ 4* band +1 ] == 0 & binary[ 4* band +2 ] == 0 )
      mask[j] <- 1 ## (0,0,0,0) Highest quality 
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 0 & binary[ 4* band +1 ] == 0 & binary[ 4* band +2 ] == 0 )
      mask[j] <- 2 ## (1,0,0,0) dead detector; data interpolated in L1B
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 0 & binary[ 4* band +1 ] == 0 & binary[ 4* band +2 ] == 1 )
      mask[j] <- 3 ## (1,0,0,1) solar zenith >= 86 degrees 
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 0 & binary[ 4* band +1 ] == 1 & binary[ 4* band +2 ] == 0 )
      mask[j] <- 4 ## (1,0,1,0) solar zenith >= 85 and < 86 degrees
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 0 & binary[ 4* band +1 ] == 1 & binary[ 4* band +2 ] == 1 )
      mask[j] <- 5 ## (1,0,1,1)  mising input
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 1 & binary[ 4* band +1 ] == 0 & binary[ 4* band +2 ] == 0 )
      mask[j] <- 6 ## (1,1,0,0) internal constant used in place of climatological data for at least one atmospheric constant
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 1 & binary[ 4* band +1 ] == 0 & binary[ 4* band +2 ] == 1 )
      mask[j] <- 7 ## (1,1,0,1) correction out of bounds pixel constrained to extreme allowable value
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 1 & binary[ 4* band +1 ] == 1 & binary[ 4* band +2 ] == 0 )
      mask[j] <- 8 ## (1,1,1,0) L1B data faulty
    else if (binary[ 4* band -1 ] == 1 & binary[ 4* band ] == 1 & binary[ 4* band +1 ] == 1 & binary[ 4* band +2 ] == 1 )
      mask[j] <- 9 ## (1,1,1,1) not processed due to deep ocean or clouds
    
  }
  
  # create raster
  bnd.rst <- qcrst
  bnd.rst[ii] <- mask
  
  return(bnd.rst)
}


qcmask_mod09a1_atm <- function(qcrst){
  ii <- which(!is.na(qcrst[])) #This is for creating mask 
  qcbit <- qcrst[ii]
  atm <- rep(NA, length(qcrst[]))  
  for ( j in 1:length(ii)){
    binary <- as.integer(intToBits(qcbit[j]))
    
    #atmospheric correction
    if (binary[ 31 ] == 0 )
      atm[j] <- 1 ## (0) atmospheric correction performed
    else 
      atm[j] <- 2 ## (1) atmospheric correction is not performed
  }
  
  # create raster
  atm.rst <- qcrst
  atm.rst[ii] <- atm
  
  return(atm.rst)
}


qcmask_mod09a1_adj <- function(qcrst){
  ii <- which(!is.na(qcrst[])) #This is for creating mask 
  qcbit <- qcrst[ii]
  adj <- rep(NA, length(qcrst[]))
  for ( j in 1:length(ii)){
    binary <- as.integer(intToBits(qcbit[j]))
    #adjacency correction
    if (binary[ 32 ] == 0 )
      adj[j] <- 1 ## (0) adjacency correction performed
    else 
      adj[j] <- 2 ## (1) adjacency correction is not performed
  }
  
  # create raster
  adj.rst <- qcrst
  adj.rst[ii] <- adj
  
  return( adj.rst)
}



