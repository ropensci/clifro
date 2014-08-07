# Enhancing the National Climate Database with clifro

[![Build Status](https://travis-ci.org/ropensci/clifro.svg)](https://travis-ci.org/ropensci/clifro)

This package is designed to minimise the hassle in downloading data
from New Zealand's National Climate Database via 
[CliFlo](http://cliflo.niwa.co.nz/) and provide functions to easily visualise 
station locations in Google Earth, conduct simultaneous searches and plot the 
resulting data with minimal **R** interaction. The vignettes and help files are 
written with the intention that even inexperienced R users can benefit from
using `clifro`. Exporting the climate data from **R** is fairly easy and for 
more experienced useRs, automated updating of spreadsheets or databases can be 
made a lot simpler.

The `clifro` package requires the user to have a current 
[CliFlo account](http://cliflo.niwa.co.nz/pls/niwp/wsubform.intro).

## Installation in R

```R
install.packages("devtools")
library(devtools)
install_github(username = "blasee", repo = "clifro", build_vignettes = FALSE)
```

## Getting started
See the vignettes for tutorials on how to use `clifro`, including choosing 
datatypes, stations, saving locations as KML files and default plotting of the 
climate data.

```R
library(clifro)
vignette("clifro")
demo(clifro)
```
