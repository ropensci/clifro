#' @include cfDataList.R
NULL


# Wind --------------------------------------------------------------------

#' Plot a windrose
#' 
#' Plot a windrose showing the wind speed and direction for given sites.
#' 
#' This is the default plotting method for \code{cfData} objects but is also
#' intended to be used as a stand-alone function for any wind dataset. A 
#' different windrose is plotted for each level of the faceting variable which
#' is coerced to a factor if necessary. The facets will generally be the station 
#' where the data were collected, seasons or dates. Currently only one faceting 
#' variable is allowed and is passed to \code{\link[ggplot2]{facet_wrap}} with
#' the formula \code{~facet}.
#' 
#' The wind summary dataframe returned if \code{wind_summary} is \code{TRUE} 
#' contains the percentage of calm days (\code{speed <= calm_days}), percentage 
#' of variable days (\code{speed == variable_days}) and quantiles calculated
#' from the empirical cumulative distribution functions for each facet.
#' 
#' @section Colour Selection:
#' For black and white windroses that may be preferred if plots are to be used 
#' in journal articles for example, recommended \code{ggtheme}s are \code{'bw'}, 
#' \code{'linedraw'}, \code{'minimal'} or \code{'classic'} and 
#' the \code{col_pal} should be \code{'Greys'}. Otherwise, any of the sequential 
#' \code{\link[RColorBrewer]{RColorBrewer}} colour palettes are recommended for 
#' colour plots.
#' 
#' @return a \code{ggplot} object or a \code{\link{data.frame}} if 
#' \code{wind_summary} is \code{TRUE}.
#' 
#' @param speed numeric vector of wind speeds.
#' @param direction numeric vector of wind directions.
#' @param facet facets used to plot the various windroses. Only one faceting 
#'              variable is currently allowed, see 'Details' below.
#' @param calm_wind the upper limit for wind speed that is considered calm.
#' @param variable_wind numeric code for variable winds (if applicable).
#' @param n_directions the number of direction bins to plot.
#' @param n_speeds the number of equally spaced wind speed bins to plot. This is
#'                 used if \code{spd_cuts} is missing.
#' @param spd_cuts numeric vector containing the cut points for the wind speed 
#'                 intervals.
#' @param legend_title character string to be used for the legend title.
#' @param col_pal character string indicating the name of the 
#'                \code{\link[RColorBrewer]{RColorBrewer}} colour palette to be 
#'                used for plotting, see 'Recommendations' below.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'              to be used for plotting, see 'Recommendations' below.
#' @param wind_summary if \code{TRUE} a dataframe is returned containing a summary of 
#'                     the wind speeds for each level of the facet.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
#'  
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales percent_format
#' @importFrom plyr ddply .
#' @importFrom ggplot2 ggplot coord_polar geom_bar cut_interval aes 
#' scale_x_discrete scale_fill_manual theme_grey theme_bw theme_classic 
#' theme_gray theme_linedraw theme_light theme_minimal element_blank 
#' element_text
#' @export
windrose = function(speed, direction, facet, calm_wind = 0, 
                    variable_wind = 990, n_directions = 12, n_speeds = 5, 
                    spd_cuts, legend_title = "Wind Speed", col_pal = "GnBu", 
                    ggtheme = "grey", wind_summary = FALSE, ...){
  
  if (!is.numeric(speed))
    stop("wind speed needs to be numeric")
  
  if (!is.numeric(direction))
    stop("wind direction needs to be numeric")
  
  if (!is.character(facet) && !is.factor(facet))
    stop("the faceting variable needs to be character or factor")
  
  if (is.character(facet))
    facet = factor(facet)
  
  if (any(
    (direction > 360 | direction < 0) & (direction != variable_wind))
  )
    stop("wind directions can't be outside the interval [0, 360]")
  
  if (!is.numeric(n_directions) || length(n_directions) != 1)
    stop("n_directions must be a numeric vector of length 1")
  
  if (!is.numeric(n_speeds) || length(n_speeds) != 1)
    stop("n_speeds must be a numeric vector of length 1")
  
  if (!is.logical(wind_summary) || length(wind_summary) != 1)
    stop("wind_summary must either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  if (!is.numeric(variable_wind) || length(variable_wind) != 1)
    stop("variable_wind must be a numeric vector of length 1")
  
  if (!is.numeric(calm_wind) || length(calm_wind) != 1)
    stop("calm_wind must be a numeric vector of length 1")
  
  if (!is.character(legend_title) || length(legend_title) != 1)
    stop("legend title must be a single character string")
  
  if (n_directions > 180){
    n_directions = 180
    warning("using the maximum number of wind directions; 180")
  }
  
  if (n_directions < 4){
    n_directions = 4
    warning("using the minimum number of wind directions; 4")
  }
  
  if (!missing(spd_cuts) && length(spd_cuts) < 3){
    warning("using the minimum 3 speed cuts")
    spd_cuts = 3
  }
  
  ## Optimising the input - select values for n_directions so that bins center
  ## on all N, E, S and W
  optimal_n_dir = seq(1, 45, 2) * 4
  if (is.na(match(n_directions, optimal_n_dir))){
    n_directions = optimal_n_dir[which.min(abs(n_directions - optimal_n_dir))]
    message("using the closest optimal number of wind directions (", 
            n_directions, ")")
  }
  
  ## Remove the variable winds
  not_variable = (direction != variable_wind)
  speed = speed[not_variable]
  direction = direction[not_variable]
  facet = facet[not_variable]
  
  ## Create factor variable for wind direction intervals
  dir_bin_width = 360 / n_directions
  dir_bin_cuts = seq(dir_bin_width / 2, 360 - dir_bin_width / 2, dir_bin_width)
  dir_intervals = findInterval(c(direction, dir_bin_cuts), dir_bin_cuts)
  dir_intervals[dir_intervals == n_directions] = 0
  factor_labs = paste(c(tail(dir_bin_cuts, 1), head(dir_bin_cuts, -1)),
                      dir_bin_cuts, sep = ", ")
  dir_bin = head(factor(dir_intervals, labels = paste0("(", factor_labs, "]")), 
                 -n_directions)
  
  
  ## Create a factor variable for wind speed intervals
  if (!missing(spd_cuts)){
    if (spd_cuts[1] > min(speed))
      spd_cuts = c(0, spd_cuts)
    
    if (tail(spd_cuts, 1) < max(speed))
      spd_cuts = c(spd_cuts, max(speed))
    spd_bin = cut(speed, spd_cuts)
  } else
    spd_bin = cut_interval(speed, n_speeds)
  
  spd_cols = brewer.pal(length(levels(spd_bin)), col_pal)
  
  if (length(spd_cols) != length(levels(spd_bin)))
    spd_bin = cut_interval(speed, length(spd_cols))
  
  ## Create the dataframe suitable for plotting
  ggplot_df = as.data.frame(table(dir_bin, spd_bin, facet))
  ggplot_df = ddply(ggplot_df, .(facet), 
                    transform, 
                    proportion = Freq / sum(Freq))
  
  if (wind_summary){
    calm_days = by(speed, facet, FUN = function(x) 
      sum(x <=  calm_wind) / length(x))
    variable_days = by(speed, facet, FUN = function(x) 
      sum(x ==  990) / length(x))
    speed_ecdf = do.call(cbind, by(speed, facet, FUN = function(x) 
      quantile(ecdf(x))))
    return(round(rbind(speed_ecdf, 
                       'calm (%)' = calm_days * 100, 
                       'variable (%)' = variable_days * 100), 1))
  }
  
  ## (gg)Plot me
  ggplot(data = ggplot_df, aes(x = dir_bin, fill = spd_bin, y = proportion)) + 
    geom_bar(stat = "identity") + 
    scale_x_discrete(breaks = levels(ggplot_df$dir_bin)[seq(1, n_directions, 
                                                            n_directions / 4)],
                     labels = c("N", "E", "S", "W"), drop = FALSE) + 
    scale_fill_manual(name = legend_title, values = spd_cols) +
    coord_polar(start = 2 * pi - pi / n_directions) +
    facet_wrap(~facet) + 
    scale_y_continuous(labels = percent_format()) + 
    eval(call(paste0("theme_", ggtheme))) +
    theme(axis.title = element_blank(),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          ...)
}

#' Plot Wind Direction Contours
#' 
#' Fit a 2D kernel estimated surface and plot the contours through time.
#' 
#' This is an optional
#' plot method for \code{cfData} objects which can be chosen by 
#' specifying \code{wind_plot = 'direction'} in the \code{plot} function 
#' call.
#' 
#' @usage plot(x, wind_plot = "direction", ggtheme = "grey", contours = 10, 
#' n_col = 1, ...)
#' 
#' @importFrom lubridate ymd_hm
#' @importFrom ggplot2 ggplot stat_density2d scale_y_continuous facet_wrap
#' ylab scale_alpha_continuous theme aes theme_grey theme_bw theme_classic 
#' theme_gray theme_linedraw theme_light theme_minimal
#' @param x a cfData object containing wind data
#' @param wind_plot either \code{windrose}, \code{speed} or \code{direction} 
#' depending on the type of wind plot
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param contours the number of contour lines to draw.
#' @param n_col the number of columns of plots.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
direction_plot = function(df, ggtheme = "grey", contours = 10, n_col = 1, y_lab, 
                          ...){
  
  if (!is.numeric(n_col) || length(n_col) != 1)
    stop("number of columns must be a single number")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  if (!is.numeric(contours) || length(contours) != 1)
    stop("contours must be a single number")
  
  ggplot(df, aes(x = date, y = direction)) + 
    stat_density2d(aes(alpha = ..level..), 
                   bins = contours, size = 1) + 
    scale_y_continuous(limits = c(0, 360), 
                       breaks = seq(0, 360, 90), 
                       labels = c("N", "E", "S", "W", "N")) + 
    facet_wrap(~station, ncol = n_col) + 
    eval(call(paste0("theme_", ggtheme))) + 
    ylab(y_lab) +
    scale_alpha_continuous(guide = FALSE) +
    theme(axis.title.x = element_blank(), ...)
}

#' Plot Wind Speed Time Series
#' 
#' Plot the wind speed through time, with a +/- standard
#' deviation interval.
#' 
#' This is an optional
#' plot method for \code{cfData} objects which can be chosen by 
#' specifying \code{wind_plot = 'speed'} in the \code{plot} function call.
#' 
#' @usage plot(x, wind_plot = "speed", ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom lubridate ymd_hm
#' @importFrom ggplot2 ggplot geom_ribbon geom_line facet_wrap xlab ylab theme
#' aes element_blank theme_grey theme_bw theme_classic theme_gray theme_linedraw 
#' theme_light theme_minimal
#' 
#' @param x a cfData object containing wind data
#' @param wind_plot either \code{windrose}, \code{speed} or \code{direction} 
#' depending on the type of wind plot
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
speed_plot = function(df, y_lab, ggtheme = "grey", free_y = FALSE, 
                      ...){
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  if (!is.character(y_lab) || length(y_lab) != 1)
    stop("y_lab must be a single character string")
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
  
  include_sd = !is.na(match("spd.sd", names(df)))
  if (include_sd)
    include_sd = include_sd && all(!is.na(df$spd.sd))
  
  p = ggplot(data = df, aes(x = date))
  if (include_sd)
    p = p + geom_ribbon(aes(ymin = speed - spd.sd, 
                             ymax = speed + spd.sd), 
                         alpha = .2)
  else
    message("unable to plot the standard deviation region due to lack of data")
  
  p = p + geom_line(aes(y = speed), colour = "#0066CC") +
    eval(call(paste0("theme_", ggtheme))) + 
    ylab(y_lab) +
    theme(axis.title.x = element_blank(), 
          ...)
  
  if (free_y)
    p = p + facet_wrap(~station, scales = "free_y")
  else
    p = p + facet_wrap(~station)
  p
}


# Precipitation -----------------------------------------------------------

#' Plot Rain Timeseries
#' 
#' Plot the amount of rainfall (mm) through time, with 
#' optional soil available water capacity and runoff amounts.
#' 
#' @usage plot(x, include_runoff = TRUE, ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom ggplot2 ggplot aes facet_wrap geom_ribbon scale_fill_discrete ylab
#' geom_line geom_point scale_colour_manual theme theme_grey theme_bw 
#' theme_classic theme_gray theme_linedraw theme_light theme_minimal
#' 
#' @param x a cfData object containing rain data.
#' @param include_runoff a logical indicating whether to plot the soil moisture
#' deficit and runoff as well as the rainfall.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
rain_plot = function(df, include_runoff = TRUE, ggtheme = "grey", free_y = FALSE, 
                     ...){
  
  if (!is.logical(include_runoff) || length(include_runoff) != 1)
    stop("include_runoff must be either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
   
  include_runoff = "deficit" %in% names(df) && include_runoff
  
  if (include_runoff){
    df$deficit = -df$deficit
    df = melt(df[, c("station", "date", "amount", "runoff", "deficit")], 
              id = c("date", "station"))
    df$variable = factor(df$variable, 
                         labels = c("Rain", "Soil runoff", "Soil deficit (AWC)"))
  }
  
  p = ggplot(df, aes(date))
  
  if (include_runoff)
    p = p + 
    geom_ribbon(aes(ymin = 0, ymax = value, fill = variable), alpha = .5) +
    ylab("Amount (mm)")
  else
    p = p + geom_ribbon(aes(ymin = 0, ymax = amount)) +
    ylab("Rain (mm)")
  
  if (free_y)
    p = p + facet_wrap(~station, scales = "free_y")
  else
    p = p + facet_wrap(~station)
  
  p = p + 
    eval(call(paste0("theme_", ggtheme))) + 
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(),
          ...)
  p
}

#' Plot Screen Observations
#' 
#' Plot the screen observations data (deg C) through time.
#' 
#' @usage plot(x, ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line ylab theme facet_wrap theme_grey 
#' theme_bw theme_classic theme_gray theme_linedraw theme_light theme_minimal
#' 
#' @param x a cfData object containing screen observations data.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
screenobs_plot = function(df, ggtheme = "grey", free_y = FALSE, ...){
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  
  clifro_df = melt(df, id.vars = 1:2, measure.vars = c(3, 4, 6))

  p = ggplot(clifro_df, aes(date, value))
  
  if (free_y)
    p = p + facet_wrap(~station, scales = "free_y")
  else
    p = p + facet_wrap(~station)
  
  p = p + 
    geom_line(aes(colour = variable), size = 1, alpha = .7) +
    ylab(expression("Temperature ("*degree*"C)")) +
    eval(call(paste0("theme_", ggtheme))) + 
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(), 
          ...)
  p
}

#' Plot Temperature Extremes
#' 
#' Plot the temperature extremes (deg C) through time.
#' 
#' @usage plot(x, ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom ggplot2 ggplot aes geom_polygon facet_wrap geom_line ylab theme
#' theme_grey theme_bw theme_classic theme_gray theme_linedraw theme_light 
#' theme_minimal
#' 
#' @param x a cfData object containing max/min temperature data.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
maxmin_plot = function(df, ggtheme = "grey", free_y = FALSE, ...){
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  max_min_df = with(df, data.frame(
    date = rep(date, 2),
    outline = c(Maximum, Minimum),
    station = factor(rep(as.character(station), 2)),
    max_min_fac = rep(c("Maximum", "Minimum"), each = nrow(df))
  ))
  
  p = ggplot(df, aes(x = date)) + 
    #geom_polygon(data = max_min_df, aes(y = outline), alpha = .4) +
    geom_line(data = max_min_df, aes(y = outline, colour = max_min_fac))
  
  if (free_y)
    p = p + 
    facet_wrap(~station, scales = "free_y")
  else
    p = p + 
    facet_wrap(~station)
  
  if (!all(is.na(df$Mean))){
    p = p + 
      geom_line(aes(y = Mean, colour = "Mean"), data = df, size = 1)
  }
  
  p = p + 
    ylab(expression("Temperature ("*degree*"C)")) +
    eval(call(paste0("theme_", ggtheme))) + 
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(), 
          ...)
  p
}

#' Plot Earth Temperatures
#' 
#' Plot the earth temperatures (deg C) for a given depth through time.
#' 
#' @usage plot(x, ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom ggplot2 ggplot aes geom_line ylab theme element_blank theme_grey 
#' theme_bw theme_classic theme_gray theme_linedraw theme_light theme_minimal
#' facet_wrap
#' 
#' @param x a cfData object containing earth temperature data.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
earthtemp_plot = function(df, ggtheme = "grey", free_y = FALSE, y_lab, ...){
  
  if (!missing(y_lab))
    if(!is.character(y_lab) || length(y_lab) != 1)
      stop("y_lab must be a single character string")
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  p = ggplot(df, aes(date, temp)) + 
    geom_line(colour = "#0066CC", size = 1) +
    eval(call(paste0("theme_", ggtheme))) + 
    ylab(y_lab) +
    theme(axis.title.x = element_blank(),
          ...)
  
  if (free_y)
    p = p + 
    facet_wrap(~station, scales = "free_y")
  else
    p = p + 
    facet_wrap(~station)
  
  p
}

#' Plot Sunshine Hours
#' 
#' Plot the duration of accumulated bright sunshine hours through time.
#' 
#' @usage plot(x, ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom ggplot2 ggplot aes geom_line ylab theme element_blank theme_grey 
#' theme_bw theme_classic theme_gray theme_linedraw theme_light theme_minimal
#' facet_wrap
#' 
#' @param x a cfData object containing sunshine data.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
sunshine_plot = function(df, ggtheme = "grey", free_y = FALSE, y_lab, ...){

  if (!missing(y_lab))
    if(!is.character(y_lab) || length(y_lab) != 1)
      stop("y_lab must be a single character string")
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  p = ggplot(df, aes(date, amount)) +
    geom_line(colour = "#0066CC", size = 1) +
    eval(call(paste0("theme_", ggtheme))) + 
    ylab(y_lab) +
    theme(axis.title.x = element_blank(),
          ...)
  
  if (free_y)
    p = p + 
    facet_wrap(~station, scales = "free_y")
  else
    p = p + 
    facet_wrap(~station)
  p
}

#' Plot Mean Sea Level Atmospheric Pressure
#' 
#' Plot the MSL atmospheric pressure through time.
#' 
#' @usage plot(x, ggtheme = "grey", free_y = FALSE, ...)
#' 
#' @importFrom ggplot2 ggplot aes geom_line ylab theme element_blank theme_grey 
#' theme_bw theme_classic theme_gray theme_linedraw theme_light theme_minimal
#' facet_wrap
#' 
#' @param x a cfData object containing pressure data.
#' @param ggtheme character string indicating the \code{\link[ggplot2]{ggtheme}} 
#'                to be used for plotting.
#' @param free_y logical value indicating if the range of the y axis should be 
#'               allowed to differ for each station.
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}.
pressure_plot = function(df, ggtheme = "grey", free_y = FALSE, y_lab, ...){
  
  if (!missing(y_lab))
    if(!is.character(y_lab) || length(y_lab) != 1)
      stop("y_lab must be a single character string")
  
  if (!is.logical(free_y) || length(free_y) != 1)
    stop("free_y must be either TRUE or FALSE")
  
  if (!is.character(ggtheme) || length(ggtheme) != 1)
    stop("ggtheme must be a single character string")
  
  p = ggplot(df, aes(date, pressure)) +
    geom_line() +
    eval(call(paste0("theme_", ggtheme))) + 
    ylab(y_lab) +
    theme(axis.title.x = element_blank(),
          ...)
  
  if (free_y)
    p = p + 
    facet_wrap(~station, scales = "free_y")
  else
    p = p + 
    facet_wrap(~station)
  p
}
#  ------------------------------------------------------------------------

#' Default \pkg{clifro} Plotting
#' 
#' Plot \pkg{clifro} data based on the datatype.
#' 
#' @usage plot(x, which, wind_plot = "windrose", ...)
#' 
#' @param x a \code{cfData} or \code{cfDataList} object.
#' @param which a single number representing the dataframe in the list to plot.
#' Only used if \code{x} is a \code{cfDataList} object.
#' @param wind_plot if \code{x} contains wind data then either \code{windrose},
#' \code{speed} or \code{direction} determines the type of plot. See the table
#' below.
#' @param ... arguments passed onto the different plotting methods.
#' 
#' This method is intended to simplify the data visualisation and exploration 
#' of CliFlo data. The type of plot is determined by the type of the data output 
#' from a \pkg{clifro} query. The following table links the datatypes to the 
#' corresponding plot methods:
#' 
#' \tabular{ll}{ 
#' \strong{Datatype} \tab \strong{Method}\cr
#' Wind \tab \code{\link{windrose}} for windrose plots\cr
#' Wind \tab \code{\link{speed_plot}} for wind speed plots\cr
#' Wind \tab \code{\link{direction_plot}} for direction contour plots\cr
#' Rain \tab \code{\link{rain_plot}} for rainfall plots\cr
#' Screen Obs \tab \code{\link{screenobs_plot}} for dry and wet bulb 
#' temperature plots\cr
#' Max/Min Temp \tab \code{\link{maxmin_plot}} for maximum, minimum and 
#' average temperature plots\cr
#' Earth Temp \tab \code{\link{earthtemp_plot}} for earth temperature 
#' plots\cr
#' Sunshine \tab \code{\link{sunshine_plot}} for accumulated sunshine hour 
#' plots\cr
#' Pressure \tab \code{\link{pressure_plot}} for mean sea level atmospheric 
#' pressure plots\cr
#' }
#' 
#' 
#' @seealso \code{\link{cf.query}} to retrieve the CliFlo data.
#' @name plot
#' @aliases plot,cfData-method
#' @aliases plot-cfData
#' @aliases plot-clifro
#' @importFrom methods setMethod
#' @export
setMethod("plot",
          signature(x = "cfData"),
          function (x, wind_plot = "windrose", ...)
          {
            clifro_df = create_dataframe(x)
            ## Wind data plots
            if (tolower(x@dt_name) %in% c("surface wind", "max gust")){
              
              if (all(is.na(clifro_df$direction)) && all(is.na(clifro_df$speed)))
                stop("no wind speed or direction data to plot")
              
              if(wind_plot == "windrose"){
                if (all(is.na(clifro_df$direction))){
                  message("no wind direction data for the windrose - ",
                          "plotting wind speed instead")
                  return(speed_plot(clifro_df, y_lab = cf_label(x), ...))
                }
                
                if (all(is.na(clifro_df$speed))){
                  message("no wind speed data for the windrose - ",
                          "plotting direction contours instead")
                  return(direction_plot(clifro_df, y_lab = cf_label(x), ...))
                }

                return(windrose(clifro_df$speed, 
                                clifro_df$direction, 
                                clifro_df$station, 
                                legend_title = cf_label(x),
                                ...))
              }
              
              ## Wind speed timeseries plot
              
              if (wind_plot == "speed"){
                if (all(is.na(clifro_df$speed))){
                  message("no wind speed data for the windrose ",
                          "plotting wind direction contours instead")
                  return(direction_plot(clifro_df, 
                                        y_lab = paste(cf_label(x), 
                                                      "direction (degrees true)"), 
                                        ...))
                }
                return(speed_plot(clifro_df, y_lab = cf_label(x), ...))
              }
              
              ## Wind direction contour plot
              
              if (wind_plot == "direction"){
                if (all(is.na(clifro_df$direction))){
                  message("no wind direction data for the windrose - ",
                          "plotting wind speed instead")
                  return(speed_plot(clifro_df, y_lab = cf_label(x), ...))
                }
                return(direction_plot(clifro_df,
                                      y_lab = paste(cf_label(x), 
                                                    "direction (degrees true)"), 
                                      ...))
                
              }
            } ## end windplots
            
            ## Rain
            if (tolower(x@dt_name) == "rain")
              return(rain_plot(clifro_df, ...))
            
            ## Snow
            if (tolower(x@dt_name) == "snow")
              stop('No default plot for snow, from CliFlo:
                   \t\t\t"These data, recorded from 1992, are very unreliable', 
                   'and are probably not worth analysing"')
            
            ## Screen Observations
            if (tolower(x@dt_name) == "screenobs")
              return(screenobs_plot(clifro_df, ...))
            
            ## Temperature
            if (tolower(x@dt_name) == "max_min")
              return(maxmin_plot(clifro_df, ...))
              
            if (tolower(x@dt_name) == "earth temp"){
              return(earthtemp_plot(clifro_df, y_lab = cf_label(x), ...))
            }
            
            if (tolower(x@dt_name) %in% c("sunshine", "radiation")){
              if (x@dt_type == "Ten-min UV")
                stop("no default plot for UV radiation")
              return(sunshine_plot(clifro_df, y_lab = cf_label(x), ...))
            }
            
            if (tolower(x@dt_name) == "pressure")
              return(pressure_plot(clifro_df, y_lab = cf_label(x), ...))
            
            stop("no default plot for ", x@dt_name, " data")
          })

#' @importFrom methods setMethod
#' @export
setMethod("plot",
          signature(x = "cfDataList"),
          function (x, which, wind_plot = "windrose", ...)
          {
            if (length(which) != 1 || !is.numeric(which))
              stop("which must be a single number")
            
            if (which > length(x))
              stop("the clifro data list only contains ", length(x), 
                   " dataframes")
            plot(x[which], wind_plot = wind_plot, ...)
          })
