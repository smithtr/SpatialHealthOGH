---
title: "London Modelling Notebook"
author: "You!"
output:
  html_document: 
    keep_md: yes
---



## Set up covered so far in the slides
Load up packages and the data

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.3     v dplyr   1.0.7
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   2.0.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(sf)
```

```
## Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1
```

```r
library(INLA)
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```
## Loading required package: foreach
```

```
## 
## Attaching package: 'foreach'
```

```
## The following objects are masked from 'package:purrr':
## 
##     accumulate, when
```

```
## Loading required package: parallel
```

```
## Loading required package: sp
```

```
## This is INLA_21.02.23 built 2021-08-09 15:59:05 UTC.
##  - See www.r-inla.org/contact-us for how to get help.
##  - Save 379.7Mb of storage running 'inla.prune()'
```

```r
library(spdep)
```

```
## Loading required package: spData
```

```
## To access larger datasets in this package, install the spDataLarge
## package with: `install.packages('spDataLarge',
## repos='https://nowosad.github.io/drat/', type='source')`
```

```r
## effects and gridExtra used in slides but not really required

## only need to do this once, can also do it manually from the GitHub page
if(!any(grepl('LondonMSOA',list.files()))){
  download.file('https://github.com/smithtr/SpatialHealthOGH/raw/main/Main_Session/Data/LondonMSOA.zip',
              'LondonMSOA.zip')
  unzip('LondonMSOA.zip')
}

MSOA_London = st_read('./LondonMSOA/London_MSOAs_Vaccines.shp')
```

```
## Reading layer `London_MSOAs_Vaccines' from data source 
##   `C:\Users\trs35\OneDrive - University of Bath\SummerSchool\SpatialHealthOGH\Main_Session\LondonMSOA\London_MSOAs_Vaccines.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 983 features and 20 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: 503574.2 ymin: 155850.8 xmax: 561956.7 ymax: 200933.6
## Projected CRS: OSGB 1936 / British National Grid
```

```r
London_Vax = select(MSOA_London, vax_rate, n, IMD,young, old, geometry)%>%
  mutate(Y = round(vax_rate/100*n, digits = 0))

head(London_Vax)
```

```
## Simple feature collection with 6 features and 6 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: 530966.7 ymin: 180510.7 xmax: 551943.8 ymax: 191139
## Projected CRS: OSGB 1936 / British National Grid
##   vax_rate     n      IMD     young       old                       geometry
## 1     68.8 10098 14.72048 0.3585201 0.1978790 MULTIPOLYGON (((532135.1 18...
## 2     67.9  6098 34.76756 0.3597348 0.1760293 MULTIPOLYGON (((548881.6 19...
## 3     65.6  9364 25.14973 0.3648836 0.1507955 MULTIPOLYGON (((549102.4 18...
## 4     74.4  5482 23.56004 0.3535316 0.1953532 MULTIPOLYGON (((551550 1873...
## 5     67.8  8532 30.21297 0.3771816 0.1203438 MULTIPOLYGON (((549099.6 18...
## 6     66.0  7825 36.00746 0.3697853 0.1445930 MULTIPOLYGON (((549819.9 18...
##      Y
## 1 6947
## 2 4141
## 3 6143
## 4 4079
## 5 5785
## 6 5164
```

Basic GLM

```r
logit_base = glm(cbind(Y, n-Y)~ IMD+ I(IMD^2)+ old + young*I(young<0.4),
                 data = London_Vax, family = 'binomial')
```


Making the adjacency matrix for London

```r
London_NB = poly2nb(MSOA_London)
London_W = nb2mat(London_NB, style = 'B')
```


Add in some columns for random effects index:

```r
London_Vax_INLA = mutate(London_Vax, IDv = 1:nrow(London_Vax), IDu = IDv)
```


## Modelling with R-INLA
### Fit the INLA model

I've given you the general structure for the call. Fill in the RHS of the formula

```r
London_BYM = inla(Y ~ ..., 
                  Ntrials = n, data = London_Vax_INLA, family = 'binomial')
```

### Posterior summaries
Various ways to look at the output of INLA


```r
names(INLA)
plot(London_BYM)
London_BYM$summary.fixed
London_BYM$summary.hyperpar
London_BYM$summary.random
```

Plot of spatial random effect

```r
London_Vax$u = London_BYM$summary.random$IDu$mean
ggplot() + 
  geom_sf(data = London_Vax, aes(fill = u))
```

### Exceedence

```r
London_BYMv2 = inla(Y ~ IMD+ I(IMD^2)+ old + young*I(young<0.4) + f(IDu, model = "bym", graph = London_W),
                    Ntrials = n, data = London_Vax_INLA, family = 'binomial')

?inla.pmarginal
```

```
## starting httpd help server ... done
```

Choose your own cutoff

```r
exceed_probs = vector(length = nrow(London_Vax))
for(i in 1:nrow(London_Vax)) exceed_probs[i] = inla.pmarginal(q = ...., marginal = London_BYMv2$marginals.random$ID[[i]])

London_Vax$exceed_probs =  exceed_probs

ggplot() + 
  geom_sf(data = London_Vax, aes(fill = exceed_probs))
```



### Package versions

```r
sessionInfo()
```

```
## R version 4.1.0 (2021-05-18)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] spdep_1.1-8     spData_0.3.10   INLA_21.02.23   sp_1.4-5       
##  [5] foreach_1.5.1   Matrix_1.3-3    sf_1.0-2        forcats_0.5.1  
##  [9] stringr_1.4.0   dplyr_1.0.7     purrr_0.3.4     readr_2.0.0    
## [13] tidyr_1.1.3     tibble_3.1.3    ggplot2_3.3.5   tidyverse_1.3.1
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.4.2         sass_0.4.0         jsonlite_1.7.2     splines_4.1.0     
##  [5] gtools_3.9.2       modelr_0.1.8       bslib_0.2.5.1      assertthat_0.2.1  
##  [9] expm_0.999-6       cellranger_1.1.0   LearnBayes_2.15.1  yaml_2.2.1        
## [13] pillar_1.6.2       backports_1.2.1    lattice_0.20-44    glue_1.4.2        
## [17] digest_0.6.27      rvest_1.0.1        colorspace_2.0-2   htmltools_0.5.1.1 
## [21] pkgconfig_2.0.3    broom_0.7.9        raster_3.4-13      gmodels_2.18.1    
## [25] haven_2.4.3        scales_1.1.1       gdata_2.18.0       MatrixModels_0.5-0
## [29] tzdb_0.1.2         proxy_0.4-26       generics_0.1.0     ellipsis_0.3.2    
## [33] withr_2.4.2        cli_3.0.1          deldir_0.2-10      magrittr_2.0.1    
## [37] crayon_1.4.1       readxl_1.3.1       evaluate_0.14      fs_1.5.0          
## [41] fansi_0.5.0        nlme_3.1-152       MASS_7.3-54        xml2_1.3.2        
## [45] class_7.3-19       tools_4.1.0        hms_1.1.0          lifecycle_1.0.0   
## [49] munsell_0.5.0      reprex_2.0.1       compiler_4.1.0     jquerylib_0.1.4   
## [53] e1071_1.7-8        rlang_0.4.11       classInt_0.4-3     units_0.7-2       
## [57] grid_4.1.0         iterators_1.0.13   rstudioapi_0.13    rmarkdown_2.10    
## [61] boot_1.3-28        gtable_0.3.0       codetools_0.2-18   DBI_1.1.1         
## [65] R6_2.5.0           lubridate_1.7.10   knitr_1.33         utf8_1.2.2        
## [69] KernSmooth_2.23-20 stringi_1.7.3      Rcpp_1.0.7         vctrs_0.3.8       
## [73] coda_0.19-4        dbplyr_2.1.1       tidyselect_1.1.1   xfun_0.25
```


