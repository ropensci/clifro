# Enhancing the National Climate Database with clifro

[![Build Status](https://travis-ci.org/ropensci/clifro.svg)](https://travis-ci.org/ropensci/clifro)

This package is designed to minimise the hassle in downloading data
from New Zealand's National Climate Database via 
[CliFlo](http://cliflo.niwa.co.nz/) and provide functions to easily visualise 
station locations in Google Earth, conduct simultaneous searches and plot the 
resulting data with minimal **R** interaction. The vignettes and help files are 
written with the intention that even inexperienced R users can use `clifro`
easily. Exporting the climate data from **R** is fairly easy and for 
more experienced useRs, automated updating of spreadsheets or databases can be 
made much easier.

The `clifro` package requires the user to have a current 
[CliFlo account](http://cliflo.niwa.co.nz/pls/niwp/wsubform.intro), otherwise a
public user can be used. Only data from one station is available without 
subscription, i.e. free for public users, however a subscription is free and 
enables access to around 6,500 climate stations around New Zealand and the 
Pacific.

Note this package requires internet access for connecting the National Climate
Database web portal.

## Installation in R

```R
install.packages("devtools")
library(devtools)
install_github(username = "ropensci", repo = "clifro", build_vignettes = FALSE)
```

## Getting started
See the vignettes for tutorials on how to use `clifro`, including choosing 
datatypes, stations, saving locations as KML files and easy, elegant plotting of 
the climate and weather data.

```R
library(clifro)
vignette("clifro")
demo(clifro)
```
