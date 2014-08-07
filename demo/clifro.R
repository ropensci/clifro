# Create a public user ----------------------------------------------------

public.user = cf_user()
public.user

# Select datatypes --------------------------------------------------------

# 9am Surface wind (m/s)
wind.dt = cf_datatype(2, 1, 4, 1)

# Daily Rain
rain.dt = cf_datatype(3, 1, 1)

# Daily temperature extremes
temp.dt = cf_datatype(4, 2, 1)

# Combine them together
all.dts = wind.dt + rain.dt + temp.dt
all.dts 

# Select the Reefton Ews station ------------------------------------------

reefton.st = cf_station()
reefton.st

# Submit the query --------------------------------------------------------

# Retrieve all data from ~ six months ago at 9am
reefton.data = cf_query(public.user, all.dts, reefton.st, paste(as.Date(Sys.time()) - 182, "9"))
reefton.data


# Plot the data -----------------------------------------------------------

plot(reefton.data, which = 1)
plot(reefton.data, which = 1, wind_plot = "speed")
plot(reefton.data, which = 1, wind_plot = "direction")
plot(reefton.data, which = 2)
plot(reefton.data, which = 2, include_runoff = FALSE)
plot(reefton.data, which = 3)
