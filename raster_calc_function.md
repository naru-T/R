R stack calculation test
================
naru-T

### Test raster stack calculation.

just try to set NA values when less than 0.5 in raster stack check the processing time .

-   set up raster stack

``` r
library(raster)
```

    ## Loading required package: sp

``` r
r <- raster(ncols=3000, nrows=3000)
r1 <- setValues(r, runif(ncell(r)))
r2 <- setValues(r, runif(ncell(r)))
r3 <- setValues(r, runif(ncell(r)))
r4 <- setValues(r, runif(ncell(r)))
r5 <- setValues(r, runif(ncell(r)))
r6 <- setValues(r, runif(ncell(r)))
stk <- stack(r1,r2,r3,r4,r5,r6)
```

-   try normal approach this approach depends on RAM. it is only faster when using smaller raster data. Do not try this way if you use big raster data.

``` r
system.time(stk[stk <= 0.5 ] <- NA)
```

    ##    user  system elapsed 
    ##  17.303   7.818  25.612

``` r
summary(stk)
```

    ##              layer.1      layer.2      layer.3      layer.4      layer.5
    ## Min.    5.000002e-01 5.000001e-01 5.000002e-01 5.000001e-01 5.000001e-01
    ## 1st Qu. 6.250331e-01 6.250090e-01 6.251318e-01 6.248845e-01 6.249995e-01
    ## Median  7.500664e-01 7.501230e-01 7.500769e-01 7.499925e-01 7.499418e-01
    ## 3rd Qu. 8.751015e-01 8.751449e-01 8.750833e-01 8.750353e-01 8.750269e-01
    ## Max.    9.999999e-01 1.000000e+00 1.000000e+00 9.999996e-01 1.000000e+00
    ## NA's    4.499125e+06 4.500654e+06 4.501698e+06 4.499518e+06 4.498993e+06
    ##              layer.6
    ## Min.    5.000001e-01
    ## 1st Qu. 6.251564e-01
    ## Median  7.500294e-01
    ## 3rd Qu. 8.750883e-01
    ## Max.    9.999997e-01
    ## NA's    4.498970e+06

``` r
system.time(stk[stk <= 0.5 ] <- NA)
```

    ##    user  system elapsed 
    ##   3.801   1.644   5.561

``` r
summary(stk)
```

    ##              layer.1      layer.2      layer.3      layer.4      layer.5
    ## Min.    5.000002e-01 5.000001e-01 5.000002e-01 5.000001e-01 5.000001e-01
    ## 1st Qu. 6.250331e-01 6.250090e-01 6.251318e-01 6.248845e-01 6.249995e-01
    ## Median  7.500664e-01 7.501230e-01 7.500769e-01 7.499925e-01 7.499418e-01
    ## 3rd Qu. 8.751015e-01 8.751449e-01 8.750833e-01 8.750353e-01 8.750269e-01
    ## Max.    9.999999e-01 1.000000e+00 1.000000e+00 9.999996e-01 1.000000e+00
    ## NA's    4.499125e+06 4.500654e+06 4.501698e+06 4.499518e+06 4.498993e+06
    ##              layer.6
    ## Min.    5.000001e-01
    ## 1st Qu. 6.251564e-01
    ## Median  7.500294e-01
    ## 3rd Qu. 8.750883e-01
    ## Max.    9.999997e-01
    ## NA's    4.498970e+06

user system elapsed
0.984 0.406 1.403

-   try user-defined function approach

this does not necessary to prepare large amount of RAM.

``` r
  na_func <- function(x){
    x[x <= 0.5] <- NA
    return(as.vector(x))
  }

stk <- stack(r1,r2,r3,r4,r5,r6)
system.time(ans <- calc(stk, na_func))
```

    ##    user  system elapsed 
    ##  97.997   5.005 103.763

``` r
summary(ans)
```

    ## Warning in .local(object, ...): summary is an estimate based on a sample of 1e+05 cells (1.11% of all cells)

    ##              layer.1      layer.2      layer.3      layer.4      layer.5
    ## Min.    5.000233e-01 5.000065e-01 5.000091e-01 5.000403e-01 5.000380e-01
    ## 1st Qu. 6.248778e-01 6.253653e-01 6.253638e-01 6.235611e-01 6.269235e-01
    ## Median  7.504247e-01 7.500137e-01 7.503599e-01 7.492441e-01 7.512044e-01
    ## 3rd Qu. 8.763194e-01 8.748003e-01 8.748508e-01 8.742661e-01 8.753872e-01
    ## Max.    9.999582e-01 9.999971e-01 9.999914e-01 9.999883e-01 9.999897e-01
    ## NA's    4.498650e+06 4.488750e+06 4.485240e+06 4.491990e+06 4.493970e+06
    ##              layer.6
    ## Min.    5.000030e-01
    ## 1st Qu. 6.250083e-01
    ## Median  7.510522e-01
    ## 3rd Qu. 8.762835e-01
    ## Max.    9.999978e-01
    ## NA's    4.497120e+06

user system elapsed
10.585 0.254 10.898

#### calculation for temporal interpolating NA values in raster stack.

very nice function by <https://stat.ethz.ch/pipermail/r-sig-geo/2010-December/010216.html>
It is always a good way to use calc function, especially when using big raster data. this do not depend on big RAM space.

``` r
require(zoo)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
fill.NA <- function(x,na.rm = F){
 #x[x==0] <- NA
 if(length(which(is.na(x)==F))>2){
 fill <- approxfun(x, y=NULL, method='linear', rule=2)
 rec <- which(is.na(x))
 x[rec] <- fill(rec)
 }
 
 return(x)
}

stk_fillNA <- calc(stk, fun=fill.NA)

summary(stk_fillNA)
```

    ## Warning in .local(object, ...): summary is an estimate based on a sample of 1e+05 cells (1.11% of all cells)

    ##              layer.1      layer.2      layer.3      layer.4      layer.5
    ## Min.    2.918998e-06 3.480818e-07 9.362120e-06 1.330394e-05 1.206435e-05
    ## 1st Qu. 2.517193e-01 2.491056e-01 2.519766e-01 2.488619e-01 2.507511e-01
    ## Median  4.993553e-01 5.005053e-01 5.010606e-01 5.001872e-01 4.999782e-01
    ## 3rd Qu. 7.502012e-01 7.502437e-01 7.507557e-01 7.493662e-01 7.511734e-01
    ## Max.    9.999582e-01 9.999971e-01 9.999914e-01 9.999883e-01 9.999897e-01
    ## NA's    0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00
    ##              layer.6
    ## Min.    2.879417e-06
    ## 1st Qu. 2.517126e-01
    ## Median  4.993794e-01
    ## 3rd Qu. 7.507359e-01
    ## Max.    9.999978e-01
    ## NA's    0.000000e+00
