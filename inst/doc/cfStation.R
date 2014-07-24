## ----, echo=FALSE--------------------------------------------------------
library(clifro)

## ------------------------------------------------------------------------
lake.tekapo.df = cf.station(12709, 35567, 39557, 4630, 24945, 4616, 4602)
lake.tekapo.df[, c("name", "agent", "start", "end", "open")]

## ------------------------------------------------------------------------
added.stations.df = lake.tekapo.df + cf.station() + cf.find.station("lighthouse", status = "all")
added.stations.df[, c("name", "agent", "start", "end", "open")]

## ----,message=FALSE,fig.width=9,fig.height=9, cache=TRUE-----------------
library(ggmap)

# Conduct the search
auckland.df = cf.find.station("auckland", search = "region", status = "all")

# Add a column to colour the open and closed stations
auckland.df$colour = factor(auckland.df$open, labels = c("Closed", "Open"))

# Reverse the dataframe rows so the open stations get plotted on top of the
# closed stations
auckland.df = auckland.df[nrow(auckland.df):1, ]

# Obtain the map of the greater Auckland suitably scaled to fit the stations
auckland.map = ggmap(get_map("Auckland", maptype = "hybrid", zoom = 8))

# Plot the resulting map with the stations and station density
auckland.map %+% auckland.df + 
  stat_density2d(aes(colour = colour), alpha = .8) +
  geom_point(aes(colour = colour), alpha = .5) +
  scale_colour_discrete("Status", c("Closed", "Open")) +
  theme(legend.title = element_text(face = "bold"))

