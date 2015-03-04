<!-- README.md is generated from README.Rmd. Please edit that file -->



Enhancing the National Climate Database with *clifro*
=====================================================

[![Build Status](https://travis-ci.org/ropensci/clifro.svg)](https://travis-ci.org/ropensci/clifro)

The *clifro* package is designed to minimise the hassle in downloading data from New Zealand's National Climate Database via [CliFlo](http://cliflo.niwa.co.nz/). It does this by providing functions for the user to log in, easily choose the appropriate datatypes and stations and then query the database. Once the data have been downloaded, they are stored as specific objects in **R** with the primary aim to ensure data visualisation and exploration is done with minimal effort and maximum efficiency.

This package extends the functionality of [CliFlo](http://cliflo.niwa.co.nz/) by returning stations resulting from simultaneous searches, the ability to visualise where these climate stations are by exporting to KML files, and elegant plotting of the climate data. The vignettes and help files are written with the intention that even inexperienced R users can use *clifro* easily. Exporting the climate data from **R** is fairly easy and for more experienced useRs, automated updating of spreadsheets or databases can be made much easier.

Free CliFlo Subscription
------------------------

A current [CliFlo subscription](http://cliflo.niwa.co.nz/pls/niwp/wsubform.intro) is recommended for *clifro*, otherwise data from only station is available. The subscription is free and lasts for 2 years or 2,000,000 rows without renewal, which enables access to around 6,500 climate stations around New Zealand and the Pacific.

Note this package requires internet access for connecting to the National Climate Database web portal.

Installation in R
=================

``` {.r}
# If devtools is not installed, install and load it
install.packages("devtools")
library(devtools)

# Install and load clifro
install_github("ropensci/clifro")
library(clifro)
```

Getting Started
===============

The following small example shows some of the core functionality in *clifro*.

Where are the climate stations?
-------------------------------

We can search for climate stations anywhere in New Zealand and return the station information in the form of a KML file. For example, we can return all the climate stations (current and historic) in the greater Auckland region.

``` {.r}
all.auckland.st = cf_find_station("Auckland", search = "region", status = "all")
cf_save_kml(all.auckland.st, "all_auckland_stations")
```

![All Auckland Climate Stations](README-map.png)

Note the open stations have green markers and the closed stations have red markers.

Download and visualise public climate data
------------------------------------------

The only station available for unlimited public access to climate data is the Reefton electronic weather station (EWS). We can download the 2014 wind and rain data and easily visualise the results very easily.

``` {.r}
# Create a public user
public.cfuser = cf_user()

# Choose the datatypes
daily.wind.rain.dt = cf_datatype(c(2, 3), c(1, 1), list(4, 1), c(1, NA))

# Choose the Reefton EWS station
reefton.st = cf_station()

# Send the query to CliFlo and retrieve the data
daily.datalist = cf_query(user = public.cfuser, 
                          datatype = daily.wind.rain.dt, 
                          station = reefton.st,
                          start_date = "2012-01-01 00",
                          end_date = "2013-01-01 00")
#> connecting to CliFlo...
#> reading data...
#> UserName is = public
#> Number of charged rows output = 0
#> Number of free rows output = 732
#> Total number of rows output = 732
#> Copyright NIWA 2015 Subject to NIWA's Terms and Conditions
#> See: http://cliflo.niwa.co.nz/pls/niwp/doc/terms.html
#> Comments to: cliflo@niwa.co.nz

# Have a look at what data is now available
daily.datalist
#> List containing clifro data frames:
#>               data      type              start                end rows
#> df 1) Surface Wind  9am only (2012-01-01  9:00) (2012-12-31  9:00)  366
#> df 2)         Rain     Daily (2012-01-01  9:00) (2012-12-31  9:00)  366

# Plot the data using default plotting methods
plot(daily.datalist)     # For the first dataframe  (Surface Wind)
```

![](README-rain-wind-example-1.png)

``` {.r}
plot(daily.datalist, 2)  # For the second dataframe (Rain)
```

![](README-rain-wind-example-2.png)

For more details and tutorials, see the vignettes and demo for how to use *clifro*, including choosing datatypes, stations, saving locations as KML files and easy, elegant plotting for various different climate and weather data.

``` {.r}
# View the clifro demo
demo(clifro)

# Read the 'Introduction to clifro' vignette
vignette("clifro")
```
