# Create a public user ----------------------------------------------------

public.user = cf.user()
public.user

# Select datatypes --------------------------------------------------------

# 9am Surface wind (m/s)
wind.dt = cf.datatype(2, 1, 4, 1)

# Daily Rain
rain.dt = cf.datatype(3, 1, 1)

# Daily temperature extremes
temp.dt = cf.datatype(4, 2, 1)

# Combine them together
all.dts = wind.dt + rain.dt + temp.dt
all.dts 

# Select the Reefton Ews station ------------------------------------------

reefton.st = cf.station()
reefton.st

# Submit the query --------------------------------------------------------

# Retrieve all data from ~ six months ago at 9am
reefton.data = cf.query(public.user, all.dts, reefton.st, paste(as.Date(Sys.time()) - 182, "9"))
reefton.data


# Plot the data -----------------------------------------------------------

plot(reefton.data, which = 1)
plot(reefton.data, which = 1, wind_plot = "speed")
plot(reefton.data, which = 1, wind_plot = "direction")
plot(reefton.data, which = 2)
plot(reefton.data, which = 2, include_runoff = FALSE)
plot(reefton.data, which = 3)
