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


