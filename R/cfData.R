# Internals ---------------------------------------------------------------

# Create a dataframe based on the datatypes suitable for plotting
#
# object a cfData object

create_dataframe = function(object){
  stopifnot(is(object, "cfData"))
  clifro_df = as(object, "data.frame")
  
  ## Wind
  if (tolower(object@dt_name) == "max gust")
    names(clifro_df)[1:4] = c("station", "date", "direction", "speed")
    
  if (tolower(object@dt_name) == "surface wind")
    names(clifro_df)[c(1:4, 6)] = c("station", "date", "direction", "speed", 
                         "spd.sd")
  
  ## Precipitation
  if (object@dt_name == "Rain")
    names(clifro_df)[1:3] = c("station", "date", "amount")
  
  if (!is.na(pmatch("Deficit", object@names)))
    names(clifro_df)[5:6] = c("deficit", "runoff")
  
  ## ScreenObs
  if (tolower(object@dt_name) == "screenobs")
    names(clifro_df) = c("station", "date", "Dry bulb", "Wet bulb", "rh", 
                         "Dew point")
  
  ## Temperature and Humidity
  if (tolower(object@dt_name) == "max_min")
    names(clifro_df)[c(1:3, 5, 7, 9)] = c("station", "date", "Maximum", 
                                          "Minimum", "Grass minimum", "Mean")
  
  if (tolower(object@dt_name) == "earth temp")
    names(clifro_df) = c("station", "date", "temp", "depth")
  
  if (tolower(object@dt_name) %in% c("sunshine", "radiation"))
    names(clifro_df)[1:3] = c("station", "date", "amount")
  
  if (tolower(object@dt_name) == "pressure")
    names(clifro_df)[1:3] = c("station", "date", "pressure")

  clifro_df
}

# Internal function to create meaningful labels for each datatype
#
# object a cfData object

cf_label = function(object){
  stopifnot(is(object, "cfData"))
  
  data_label = paste(object@dt_type, tolower(object@dt_name))
  object@dt_type = gsub("^ *", "", object@dt_type)
  ## Wind
  if (tolower(object@dt_name) %in% c("surface wind", "max gust")){
    
    if (!is.na(pmatch("wind run", tolower(object@dt_type)))){
      data_label = "9am wind run (km)"
    } else {
      spd_col = pmatch("Speed", object@names)
      dt_units = strsplit(object@names[spd_col], "(", fixed = TRUE)[[1]][2]
      data_label = paste(object@dt_type, tolower(object@dt_name), 
                         paste0("(", dt_units))
    }
  }
  
  ## Precipitation
  if (object@dt_name == "Snow")
    data_label = "Snow"
  
  if (object@dt_name == "Rain")
    data_label = paste(object@dt_type, "rain (mm)")
  
  ## ScreenObs
  if (tolower(object@dt_name) == "screenobs")
    data_label = paste(object@dt_type, "screen observations (deg C)")
  
  ## Earth Temperature
  if (tolower(object@dt_name) == "earth temp")
    data_label = paste("Earth temperature at", object@dt_type, "(deg C)")
  
  ## Sunshine and Radiation
  if (tolower(object@dt_name) == "sunshine")
    data_label = paste(object@dt_type, "sunshine (hours)")
  
  if (tolower(object@dt_name) == "radiation")
    data_label = paste(object@dt_type, "radiation (MJ/m2)")
  
  ## Pressure
  if (tolower(object@dt_name) == "pressure")
    data_label = paste(object@dt_type, "atmospheric MSL pressure (hPa)")
  
  data_label
}

# cfData Class ------------------------------------------------------------

#' @importFrom methods setClass
setClass("cfData", slots = c(dt_name = "character",
                             dt_type = "character"),
         contains = "dataFrame")

#' @importFrom lubridate ymd_hm
#' @importFrom methods setAs
#' @importFrom stats setNames
setAs("cfData", "data.frame",
      function(from){
        df_names = gsub(")", "", from@names, fixed = TRUE)
        df_names = gsub("(", ".", df_names, fixed = TRUE)
        df_names = gsub("/", "", df_names, fixed = TRUE)
        cf_df = setNames(data.frame(from, stringsAsFactors = FALSE),
                         df_names)
        cf_df[, 1] = factor(cf_df[, 1])
        cf_df[, 2] = ymd_hm(cf_df[, 2], tz = "Pacific/Auckland")
        cf_df
      })
# Methods -----------------------------------------------------------------

#' @importFrom methods setClass
setMethod("show",
          signature(object = "cfData"),
          function (object)
          {
            cat(cf_label(object), "\n")
              print(head(as(object, "data.frame"), 4))
              if (nrow(object) > 4){
                n_omitted = nrow(object) - 4
                cat(paste("[~~ omitted", n_omitted,"rows ~~]\n"))
              }
          }
)