# Internals ---------------------------------------------------------------

# Internal function to show the available regions. 
# 
# This is scraped from the 'Region' combo box on the 'Find Stations' page. If
# region is missing then the user the available regions are shown and the user
# is prompted for a selection using menu.
#
# region: a string to be partially matched to the available regions
#' @importFrom selectr querySelectorAll
#' @importFrom XML htmlParse xmlApply xmlGetAttr
#' @importFrom utils menu
cf_region = function(region){
  regions = htmlParse("http://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn_html")
  region.xml = querySelectorAll(regions, "option[value^='-']")
  region.names = unlist(xmlApply(region.xml, xmlValue))
  
  if (!missing(region)){
    region.names.lower = tolower(region.names)
    region = tolower(region)
    region.choice = pmatch(region, region.names.lower)
    
    if (is.na(region.choice)){
      warning("region not matched - ignoring", immediate. = TRUE)
      region.choice = menu(region.names, title = "Regions")
      if (region.choice)
        return(xmlGetAttr(region.xml[[region.choice]], "value"))
    } else {
      return(xmlGetAttr(region.xml[[region.choice]], "value"))
    }
  } else {
    region.choice = menu(region.names, title = "Regions")
    if (region.choice)
      return(xmlGetAttr(region.xml[[region.choice]], "value"))
  }
}

# Internal function to save a KML file of stations returned from searches
#
# This function is used when save_KML in the cf.find.station function is TRUE.
#
# df       :  the dataframe containing all the station information
# file_name:  the name of the KML file
# file_path:  the path for the KML file to be saved
#
# This function returns a KML file with the open stations shown as blue markers
# and the closed stations as red markers. 
# 
# see also cf.save.kml.
#' @importFrom XML newXMLDoc newXMLNode saveXML

save_KML = function(df, file_name, file_path){
  doc = newXMLDoc()
  kml.node = 
    newXMLNode("kml", doc = doc, 
               namespaceDefinitions = c("http://www.opengis.net/kml/2.2", 
                                        gx = "http://www.google.com/kml/ext/2.2", 
                                        kml = "http://www.opengis.net/kml/2.2", 
                                        atom = "http://w3.org/2005/Atom"))
  Document.node = newXMLNode("Document", parent = kml.node)
  newXMLNode("name", gsub("_", " ", strsplit(file_name, ".kml")[[1]]),
             parent = Document.node)
  newXMLNode("open", 1, parent = Document.node)
  
  ## Define the hover state for open stations
  newXMLNode("StyleMap", attrs = c(id = "hoverStateOpen"),
             parent = Document.node, 
             newXMLNode("Pair", 
                        newXMLNode("key", "normal"), 
                        newXMLNode("styleUrl", "#normalStateOpen")), 
             newXMLNode("Pair", 
                        newXMLNode("key", "highlight"), 
                        newXMLNode("styleUrl", "#highlightStateOpen")
             )
  )
  
  ## Define the hover state for closed stations
  newXMLNode("StyleMap", attrs = c(id = "hoverStateClosed"),
             parent = Document.node, 
             newXMLNode("Pair", 
                        newXMLNode("key", "normal"), 
                        newXMLNode("styleUrl", "#normalStateClosed")), 
             newXMLNode("Pair", 
                        newXMLNode("key", "highlight"), 
                        newXMLNode("styleUrl", "#highlightStateClosed")
             )
  )
  
  ## Define the highlight state for open stations
  newXMLNode("Style", attrs = c(id = "highlightStateOpen"), 
             parent = Document.node,
             newXMLNode("IconStyle", 
                        newXMLNode("scale", "1.3"),
                        newXMLNode("Icon", 
                                   newXMLNode("href",
                                              "http://maps.google.com/mapfiles/ms/micons/blue-dot.png"))),
             newXMLNode("LabelStyle", 
                        newXMLNode("scale", "1.3")))
  
  ## Define the highlight state for closed stations
  newXMLNode("Style", attrs = c(id = "highlightStateClosed"), 
             parent = Document.node,
             newXMLNode("IconStyle", 
                        newXMLNode("scale", "1.3"),
                        newXMLNode("Icon", 
                                   newXMLNode("href",
                                              "http://maps.google.com/mapfiles/ms/micons/red-dot.png"))),
             newXMLNode("LabelStyle", 
                        newXMLNode("scale", "1.3")))
  
  ## Define the normal state for open stations
  newXMLNode("Style", attrs = c(id = "normalStateOpen"), 
             parent = Document.node,
             newXMLNode("IconStyle", 
                        newXMLNode("scale", "0.9"),
                        newXMLNode("Icon", 
                                   newXMLNode("href",
                                              "http://maps.google.com/mapfiles/ms/micons/blue-dot.png"))),
             newXMLNode("LabelStyle", 
                        newXMLNode("scale", "0.7")))
  
  ## Define the normal state for closed stations
  newXMLNode("Style", attrs = c(id = "normalStateClosed"), 
             parent = Document.node,
             newXMLNode("IconStyle", 
                        newXMLNode("scale", "0.9"),
                        newXMLNode("Icon", 
                                   newXMLNode("href",
                                              "http://maps.google.com/mapfiles/ms/micons/red-dot.png"))),
             newXMLNode("LabelStyle", 
                        newXMLNode("scale", "0.7")))
  
  df$hoverstate = "#hoverStateClosed"
  df$hoverstate[df$open] = "#hoverStateOpen"
  
  closed.folder = newXMLNode("Folder", parent = Document.node)
  newXMLNode("name", "Closed", parent = closed.folder)
  for (i in which(!df$open)){
    newXMLNode("Placemark",
               newXMLNode("name", df$name[i]),
               newXMLNode("description", paste0("Agent:     ", df$agent[i], "\n",
                                               "Network:    ", df$network[i], "\n",
                                               "Start date: ", df$start[i], "\n",
                                               "End date:   ", df$end[i], "\n")),
               newXMLNode("styleUrl", df$hoverstate[i]),
               newXMLNode("Point", 
                          newXMLNode("coordinates", 
                                     paste(df$lon[i], 
                                           df$lat[i], 0, sep = ",")
                          )
               ), parent = closed.folder
    )
  }
  
  open.folder = newXMLNode("Folder", parent = Document.node)
  newXMLNode("name", "Open", parent = open.folder)
  for (i in which(df$open)){
    newXMLNode("Placemark",
               newXMLNode("name", df$name[i]),
               newXMLNode("description", paste0("Agent:     ", df$agent[i], "\n",
                                                "Network:    ", df$network[i], "\n",
                                                "Start date: ", df$start[i], "\n",
                                                "End date:   ", df$end[i], "\n")),
               newXMLNode("styleUrl", df$hoverstate[i]),
               newXMLNode("Point", 
                          newXMLNode("coordinates", 
                                     paste(df$lon[i], 
                                           df$lat[i], 0, sep = ",")
                          )
               ), parent = open.folder
    )
  }
  
  if (file_name == "my_stations_"){
    xml_file = tempfile(file_name, file_path, ".kml")
    saveXML(doc, file = xml_file)
  }
  else{
    if (!grepl(".kml$", file_name))
      file_name = paste0(file_name, ".kml")
    xml_file = file.path(file_path, file_name)
    saveXML(doc, file = xml_file)
  }

  message(paste("output KML file:", xml_file))

  if (Sys.which("google-earth") == "")
    message("You may like to install Google Earth to view the locations")
}

# Save KML files ----------------------------------------------------------

#' Save \code{cfStation} Locations to a KML File
#' 
#' Save \code{cfStation} locations to a KML file
#' 
#' The \code{cf.save.kml} function is for \code{cfStation} 
#' objects to allow for the spatial visualisation of the selected stations. The 
#' resulting KML file is saved and can then be opened by programs like Google 
#' Earth (TM). The resultant KML file has the station names and locations shown 
#' with blue markers for open and red markers for closed stations. The agent 
#' numbers, network ID's and date ranges are displayed as the descriptions.
#' 
#' If no file name is specified, unique names are produced in the current \R 
#' working directory.
#' 
#' @note The .kml suffix is appended automatically if it isn't already present
#' in the \code{file_name} argument.
#' 
#' @param station \code{cfStation} object containing one or more stations
#' @param file_name file name for the resulting KML file
#' @param file_path file path for the resulting KML file
#' 
#' @export
#' @seealso \code{\link{cf.station}} and \code{vignette("cfStation")} for 
#' working with known stations, otherwise \code{\link{cf.find.station}} and 
#' \code{vignette("choose-station")} for creating \code{cfStation} objects when
#' the agent numbers are unknown.
#' 
#' @examples
#' \dontrun{
#' # A selection of four Auckland region stations down the East Coast to the 
#' # upper Waitemata Harbour; Leigh 2 Ews, Warkworth Ews, Tiri Tiri Lighthouse 
#' # and Henderson
#' my.stations = cf.station(17838, 1340, 1401, 12327)
#' my.stations
#' 
#' # Save these stations to a KML file
#' cf.save.kml(my.stations)
#' 
#' # Where is the subscription-free Reefton Ews station?
#' cf.save.kml(cf.station(), file_name = "reeftonEWS")
#' 
#' # Save all the Christchurch stations (using partial matching)
#' all.chch.st = cf.find.station("christ", status = "all", search = "region")
#' cf.save.kml(all.chch.st, file_name = "all_Chch_stations")
#' }
cf.save.kml = function(station, file_name = "my_stations_", 
                       file_path = "."){
  if (!is(station, "cfStation"))
    stop("station must be a cfStation object")
  if (grepl(".kml$", file_name))
    file_name = gsub(".kml$", "", file_name)
  save_KML(station, file_name = file_name, file_path = normalizePath(file_path))
}

# Find stations -----------------------------------------------------------

#' Search for Clifro Stations
#' 
#' Find the agent number for stations based on name, region, location or network
#' number.
#' 
#' The \code{cf.find.station} function is a convenience function for finding 
#' CliFlo stations in \R. It uses the CliFlo "Find Stations" page 
#' (\url{http://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn_html}) to do the 
#' searching meaning the stations are not stored within \pkg{clifro}.
#' 
#' If \code{datatype} is missing then the search is conducted 
#' without any reference to datatypes. If it is supplied then the 
#' search will only return stations that have any or all of the supplied 
#' datatypes, depending on \code{combine}. The default behaviour is to searche 
#' for stations based on pattern matching the station name and return only the 
#' open stations.
#' 
#' If the \code{latlong} search type is used the function expects a named 
#' arguments with names (partially) matching latitude, 
#' longitude and radius. If the arguments are passed in without names they must
#' be in order of latitude, longitude and radius.
#' 
#' @return \code{cfStation} object
#' @param ... arguments to pass into the search, these differ depending on 
#' \code{search}.
#' @param search one of \code{name}, \code{network}, \code{region} or 
#' \code{latlong} indicating the type of search to be conducted.
#' @param datatype \code{cfDatatype} object for when the search is based on 
#' datatypes.
#' @param combine character string \code{"all"} or \code{"any"} indicating if the 
#' stations contain all or any of the selected datatypes for when the search is 
#' based on datatypes.
#' @param status character string indicating \code{"open"}, \code{"closed"} or 
#' \code{"all"} stations be returned by the search.
#' 
#' @note Since the searching is done by CliFlo there are obvious restrictions. 
#' Unfortunately the pattern matching for station name does not provide 
#' functionality for regular expressions, nor does it allow simultaneous 
#' searches although \pkg{clifro} does provide some functionality, see 
#' 'Examples' below.
#' 
#' @export
#' @aliases find-cfStation
#' @importFrom RCurl getCurlHandle postForm
#' @importFrom XML htmlParse xmlValue xmlSApply
#' @importFrom selectr querySelectorAll
#' @importFrom lubridate with_tz now round_date %--% dseconds
#' @importFrom stats na.exclude
#' @seealso \code{\link{cf.save.kml}}, \code{\link{cf.station}}, 
#' \code{vignette("choose-station")} and \code{vignette("cfStation")}
#' @examples
#' \dontrun{
#' # Return all open stations with 'island' in the name (pattern match search)
#' # Note this example uses all the defaults
#' cf.find.station("island")
#' 
#' # Return all the closed stations from Queenstown (region search)
#' cf.find.station("queen", search = "region", status = "closed")
#' 
#' # Return all open stations within a 10km radius of the Beehive in Wellington
#' # From Wikipedia: latitude 41.2784 S, longitude 174.7767 E
#' cf.find.station(lat = -41.2784, long = 174.7767, rad = 10, 
#'                 search = "latlong")
#' 
#' # Return all stations that share A42 in their network ID (network ID search)
#' cf.find.station("A42", search = "network", status = "all")
#' 
#' # Is the Reefton EWS station open and does it collect daily rain and wind 
#' # data? (using datatypes in the search)
#' 
#' # Daily rain and wind datatypes
#' daily.dt = cf.datatype(c(2, 3), c(1, 1), list(4, 1), c(1, NA))
#' 
#' cf.find.station("reefton EWS", datatype = daily.dt)      # Yes
#' 
#' # Return all stations sharing A42 in their network ID 
#' \emph{OR} all the open stations within 10km of the Beehive in Wellington
#' (note this is not an AND query).
#' cf.find.station("A42", search = "network", status = "all") +
#' cf.find.station(lat = -41.2784, long = 174.7767, rad = 10, 
#'                 search = "latlong")
#' 
#' # Note these are all ordered by open stations, then again by their end dates
#' }
cf.find.station = function(..., 
                           search = c("name", "region", "network",
                                           "latlong"),
                           datatype,
                           combine = c("all", "any"),
                           status = c("open", "closed", "all")) {
  search = match.arg(arg = search)
  status = match.arg(arg = status)
  search_string = c(...)
  include_distances = FALSE
  
  if (search == "latlong"){
    if (length(search_string) != 3)
      stop("need exactly one longitude, latitude and radius")
    
    lat_long_rad = c("latitude", "longitude", "radius")
    
    if(is.null(names(search_string))){
      names(search_string) = lat_long_rad
      message(paste("using", paste(lat_long_rad, search_string,
                                   sep = " = ", collapse = ", ")))
    }
    
    partial.match = pmatch(names(search_string), lat_long_rad)
    
    if (any(is.na(partial.match)))
      stop("names must be partially matched to latitude, longitude and radius")
    
    if (any(is.na(as.numeric(search_string))))
      stop("latitude and longitude (and radius) need to be in decimal formats")
    
    lat = as.character(search_string[partial.match == 1])
    long = as.character(search_string[partial.match == 2])
    rad = as.character(search_string[partial.match == 3])
    
    if (abs(as.numeric(long)) > 180 || abs(as.numeric(lat)) > 90)
      stop("use sensible latitudes and longitudes")
  } else {
    if (length(search_string) > 1)
      stop("only one search can be conducted at a time")
    search_string = as.character(c(...))
  }
  
  param.list = 
    switch(search, 
           name = list(cstype = "name", cstnstr = search_string),
           region = {
             if (length(search_string) == 0)
               search_region = search_string = cf_region()
             else
               search_region = cf_region(search_string)
             
             search_string = unlist(strsplit(search_region, ","))[3]
             list(cstype = "region", cRegion = search_region)
           },
           network = list(cstype = "net", cNet = search_string),
           latlong = {
             include_distances = TRUE
             list(cstype = "latlongc", clat1 = lat, clong1 = long, 
                  crad = rad)
           }
    )
  
  param.list = c(param.list, mimeselection = "htmltable", 
                 Submit = "Get Station List", status = status)
  
  if (!missing(datatype)){
    if (!is(datatype, "cfDatatype"))
      stop("datatype has to be a cfDatatype object")
    
    user = cf.user()
    cf_login(user)
    on.exit(cf_logout(user, msg = FALSE))
    combine = switch(match.arg(arg = combine),
                     all = "and",
                     any = "or")
    cf_update_dt(datatype)
    cookies = file.path(tempdir(), user@username)
    curl = getCurlHandle(followlocation = TRUE, 
                         timeout = 100, 
                         useragent = 
                           paste("clifro", R.Version()$version.string),
                         cookiefile = cookies,
                         cookiejar = cookies)
    param.list = c(param.list, ccomb_dt = combine)
    doc = htmlParse(
      postForm("http://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn",
               .params = param.list, curl = curl))
  } else
    doc = htmlParse(postForm("http://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn_nodt", 
                             .params = param.list))
  
  agent_name_xml = querySelectorAll(doc, "a.st[href*='wstn.stn_details?']")
  
  if (length(agent_name_xml) == 0)
    stop("no climate stations were found matching your search criteria", 
         call. = FALSE)
  
  network_xml = querySelectorAll(doc, "a.st[href*='wstn.data_availibility']")
  lat_long_xml = querySelectorAll(doc, "a.st[href*='?cstype=']")
  
  ## The following is a little messy but I can't figure out how else to do this!
  start_end = distances = 
    sapply(querySelectorAll(doc, "td.stnextdata + td.stnextdata"), xmlValue)
  which_are_also_dates = start_end == "-"
  start_end = dmy(start_end, quiet = TRUE, tz = "Pacific/Auckland")
  start_end[which_are_also_dates] = with_tz(round_date(now(), "month"),
                                            tz = "Pacific/Auckland")
  start_end = na.exclude(start_end)
  
  if (include_distances)
    if (!missing(datatype)){
      distances = as.numeric(distances[seq(8, length(distances), by = 8)])
    } else {
      distances = as.numeric(distances[seq(7, length(distances), by = 7)])
    }    
  else
    distances = numeric(length(network_xml))
  
  start_dates = start_end[seq(1, length(start_end), by = 2)]
  end_dates = start_end[seq(2, length(start_end), by = 2)]
  
  ## Open stations in clifro have end dates less than 4 weeks ago
  span = end_dates %--% now()
  open_station = (as.numeric(dseconds(span)) / (604800 * 4) < 4)
  
  
  ## Account for CliFlo giving outdated stations for certain datatypes
  if ((status == "open" && !any(open_station)) || (status == "closed" &&
                                                     all(open_station)))
    stop("no climate stations were found matching your search criteria",
         call. = FALSE)
  
  if (status == "open" && any(!open_station)){
    start_dates = start_dates[open_station]
    end_dates = end_dates[open_station]
    keep_2 = rep(open_station, each = 2)
    agent_name_xml = agent_name_xml[keep_2]
    network_xml = network_xml[open_station]
    lat_long_xml = lat_long_xml[keep_2]
    distances = distances[open_station]
    open_station = open_station[open_station]
  }
  
  if (status == "closed" && any(open_station)){
    start_dates = start_dates[!open_station]
    end_dates = end_dates[!open_station]
    keep_2 = rep(!open_station, each = 2)
    agent_name_xml = agent_name_xml[keep_2]
    network_xml = network_xml[!open_station]
    lat_long_xml = lat_long_xml[keep_2]
    distances = distances[!open_station]
    open_station = open_station[!open_station]
  }

  agent_name = sapply(agent_name_xml, xmlValue)
  lat_long = as.numeric(sapply(lat_long_xml, xmlValue))
  
  new("cfStation", 
      data.frame(
          name = agent_name[seq(2, length(agent_name), by = 2)],
          network = sapply(network_xml, xmlValue),
          agent = as.numeric(agent_name[seq(1, length(agent_name), by = 2)]),
          start_date = start_dates,
          end_date = end_dates,
          open_station = open_station,
          distances = distances,
          latitude = lat_long[seq(1, length(agent_name), by = 2)],
          longitude = lat_long[seq(2, length(agent_name), by = 2)],
          stringsAsFactors = FALSE, check.names = TRUE
      ))
}