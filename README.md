# SpatialHealthOGH
Materials for Analysing Spatiotemporal Health Data session of the [Open Geo Hub Summer School 2021](https://opengeohub.org/summer_school_2021).

## Packages
I will use the following packages during my session
- spdep
- sf
- tidyverse (or ggplot2, tidyr and dplyr...not strictly necessary if you prefer to DIY the data manipulation and plotting)
- INLA This is not on CRAN. You can download it by running the following in your R console

```{r}
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```
In the slides for the main session, I also use the gridExtra and effects packages. These are not necessary for following the demo. 


## Challange Question
This folder contains the data behind the motivating example for my challange question. The question itself is in [this Google doc]( https://docs.google.com/document/d/1QfuvSjX6iL9ZTCmFsBeCakB3nEIJExd4RmDFjSKIcOc/edit?usp=sharing).


## Main Session 
This folder contains the materials for the session, taking place on Friday 3 September. A first draft of the slides [can be viewed here](https://bit.ly/3krot0i).
