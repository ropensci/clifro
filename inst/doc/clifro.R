## ----, echo=FALSE--------------------------------------------------------
library(clifro)

## ----,eval=FALSE---------------------------------------------------------
#  me = cf.user("username", "password")

## ------------------------------------------------------------------------
my.dts = cf.datatype(select_1 =     c(7,  4,  3,  2), 
                     select_2 =     c(1,  2,  1,  1), 
                     check_box = list(3,  1,  1,  4), 
                     combo_box =    c(NA, NA, NA, 1))
my.dts

## ------------------------------------------------------------------------
my.stations = cf.station(5814, 4241, 2112, 1962)
my.stations[, 1:5]

## ----,eval=FALSE---------------------------------------------------------
#  cf.datalist = cf.query(user = me,
#                         datatype = my.dts,
#                         station = my.stations,
#                         start_date = "2012-01-01 00",
#                         end_date = "2014-01-01 00")
#  cf.datalist

## ----,eval=FALSE---------------------------------------------------------
#  plot(cf.datalist, which = 1, ggtheme = "bw")
#  
#  # Equivalent to
#  # plot(cf.datalist[1], ggtheme = "bw")

## ----,eval=FALSE---------------------------------------------------------
#  # Load the ggplot2 library
#  library(ggplot2)
#  
#  # Add a loess smoother with a span of a third of the window
#  plot(cf.datalist, which = 1, ggtheme = "bw",
#       strip.text = element_text(size = 14)) +
#    geom_smooth(method = "loess", span = 1/3)

## ----,eval=FALSE---------------------------------------------------------
#  plot(cf.datalist, which = 2, ggtheme = "linedraw")

## ----,eval=FALSE---------------------------------------------------------
#  plot(cf.datalist, which = 3, ggtheme = "light")

## ----, eval=FALSE--------------------------------------------------------
#  # Don't plot the soil deficit and runoff
#  plot(cf.datalist, which = 3, include_runoff = FALSE, ggtheme = "light")

## ----,eval=FALSE---------------------------------------------------------
#  # Defaults to windrose
#  plot(cf.datalist, which = 4)

## ----,eval=FALSE---------------------------------------------------------
#  # Plot the wind speeds through time
#  plot(cf.datalist, which = 4, wind_plot = "speed", ggtheme = "classic")

## ----,eval=FALSE---------------------------------------------------------
#  # Plot wind direction contours
#  plot(cf.datalist, which = 4, wind_plot = "direction", contours = 10, ggtheme = "light")

## ----,eval=FALSE---------------------------------------------------------
#  # Export the data as separate CSV files to the current working directory
#  getwd()
#  for (i in seq_along(cf.datalist))
#    write.csv(cf.datalist[i],
#              file = tempfile(paste0(cf.datalist[i]@dt_name, "_"),
#                              tmpdir = normalizePath("."),
#                              fileext = ".csv"))

