## ----, echo=FALSE--------------------------------------------------------
library(clifro)

## ------------------------------------------------------------------------
# Equivalent to searching for status = "open" on CliFro
# Note the search string is not case sensitive
cf.find.station("takaka", status = "all")

## ------------------------------------------------------------------------
cf.find.station("takaka", status = "open")

## ------------------------------------------------------------------------
cf.find.station("f028", search = "network", status = "all")

## ------------------------------------------------------------------------
# Partial match for the Queenstown region
open.queenstown.stations = cf.find.station("queen", search = "region")

## ------------------------------------------------------------------------
takaka.town.st = cf.find.station(lat = -40.85, long = 172.8, rad = 10, search = "latlong")
takaka.town.st[, -c(8, 9)]

# We may rather order the stations by distance from the township
takaka.town.st[order(takaka.town.st$distance), -c(8, 9)]

## ------------------------------------------------------------------------
# Create a clifro datatype for hourly rain
hourly.rain.dt = cf.datatype(3, 1, 2)
hourly.rain.dt

# Conduct the search
cf.find.station("takaka", datatype = hourly.rain.dt)

## ------------------------------------------------------------------------
my.composite.search = takaka.town.st + cf.find.station("kaitaia", 
                                                       search = "region", 
                                                       datatype = hourly.rain.dt)
my.composite.search

# How long have these stations been open for?
transform(my.composite.search, ndays = round(end - start))[, c(1, 10)]

## ----,message=FALSE------------------------------------------------------
# First, search for the stations
all.auckland.st = cf.find.station("auckland", search = "region", status = "all")

# Then save these as a KML
cf.save.kml(all.auckland.st, file_name = "all_auckland_stations")

