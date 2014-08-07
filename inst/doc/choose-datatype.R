## ----, echo=FALSE--------------------------------------------------------
library(clifro)
surfaceWind.dt = cf_datatype(2, 1, 4, 3)

menu.opts = function(title, options){
  cat(paste(title, "",
              paste(seq_along(options), options, sep = ": ", 
                    collapse = "\n"), sep = "\n"))
}

## ----, eval=FALSE--------------------------------------------------------
#  surfaceWind.dt = cf_datatype()
#  
#  # If you prefer pointing and clicking - turn the graphics option on:
#  surfaceWind.dt = cf_datatype(graphics = TRUE)

## ----, echo=FALSE--------------------------------------------------------
menu.opts("Daily and Hourly Observations", 
          c("Combined Observations", "Wind", "Precipitation", 
                           "Temperature and Humidity", "Sunshine and Radiation", 
                           "Weather", "Pressure", "Clouds", 
                           "Evaporation / soil moisture"))

## ----, echo=FALSE--------------------------------------------------------
menu.opts("Wind", c("Surface wind", "Max Gust"))

## ----, echo=FALSE--------------------------------------------------------
menu.opts("Surface wind options", c("WindRun", "HlyWind", "3HlyWind", "9amWind")
          )

## ----, echo=FALSE--------------------------------------------------------
menu.opts("Choose another option?", c("yes", "no"))

## ----, echo=FALSE--------------------------------------------------------
menu.opts("Units", c("m/s", "km/hr", "knots"))

## ------------------------------------------------------------------------
surfaceWind.dt

## ------------------------------------------------------------------------
surfaceWind.dt = cf_datatype(2, 1, 4, 3)
surfaceWind.dt

## ------------------------------------------------------------------------
surfaceWind.dt = cf_datatype(2, 1, c(2, 4), 3)
surfaceWind.dt

## ------------------------------------------------------------------------
# Hourly and 9am surface wind (knots)
surfaceWind.dt = cf_datatype(2, 1, c(2, 4), 3)

# Hourly and daily rainfall
rainfall.dt = cf_datatype(3, 1, c(1, 2))

# Hourly counts of lightning flashes
lightning.dt = cf_datatype(6, 1, 1)

# Daily and hourly grass temperature extremes
temperatureExtremes.dt = cf_datatype(4, 2, c(5, 6))

# Note: only the surface wind datatype requires combo options

## ----, tidy=FALSE--------------------------------------------------------
query1.dt = cf_datatype(c(2, 3, 6, 4), 
                        c(1, 1, 1, 2),
                        list(c(2, 4), c(1, 2), 1, c(5, 6)),
                        c(3, NA, NA, NA))
query1.dt

## ------------------------------------------------------------------------
query1.dt = surfaceWind.dt + rainfall.dt + lightning.dt + 
  temperatureExtremes.dt
query1.dt

## ----, eval=FALSE--------------------------------------------------------
#  # To add another datatype using the menu:
#  query1.dt + cf_datatype()
#  
#  # Is equivalent to:
#  query1.dt + cf_datatype(NA, NA, NA, NA)
#  
#  # Therefore is equivalent to adding a column of NA's to the above table:
#  query1.dt = cf_datatype(c(2, 3, 6, 4, NA),
#                                c(1, 1, 1, 2, NA),
#                                list(c(2, 4), c(1, 2), 1, c(5, 6), NA),
#                                c(3, NA, NA, NA, NA))
#  
#  # Half an unknown wind datatype i.e. we know first selection = 2 but nothing
#  # further:
#  rain.dt = cf_datatype(2) # 0r cf_datatype(2, NA, NA, NA)

