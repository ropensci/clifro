# Internals ---------------------------------------------------------------

# Internal function to show the available regions.
#
# This is scraped from the 'Region' combo box on the 'Find Stations' page. If
# region is missing then the user the available regions are shown and the user
# is prompted for a selection using menu.
#
# region: a string to be partially matched to the available regions
#' @importFrom xml2 read_html
#' @importFrom httr GET
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom utils menu
cf_region = function(region){
  
  get_stn_html = GET("https://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn_html")
  region_xml = html_nodes(read_html(get_stn_html), "td.p_selected select.general option")[-1]
  region_names = html_text(region_xml)
  
  if (!missing(region)){
    region_names_lower = tolower(region_names)
    region = tolower(region)
    region.choice = pmatch(region, region_names_lower)

    if (is.na(region.choice)){
      warning("region not matched - ignoring", immediate. = TRUE)
      region.choice = menu(region_names, title = "Regions")
      if (region.choice)
        return(html_attr(region_xml[region.choice], "value"))
    } else {
      return(html_attr(region_xml[region.choice], "value"))
    }
  } else {
    region.choice = menu(region_names, title = "Regions")
    if (region.choice)
      return(html_attr(region_xml[region.choice], "value"))
  }
}

# Internal function to save a KML file of stations returned from searches
#
# This function is used when save_KML in the cf_find_station function is TRUE.
#
# df       :  the dataframe containing all the station information
# file_name:  the name of the KML file
# file_path:  the path for the KML file to be saved
#
# This function returns a KML file with the open stations shown as blue markers
# and the closed stations as red markers.
#
# see also cf_save_kml.
#' @importFrom xml2 xml_new_root xml_add_child xml_add_sibling xml_parent
#'                  xml_root write_xml
#' @importFrom magrittr %>%

save_KML = function(df, file_name, file_path){
  df$hoverstate = "#hoverStateClosed"
  df$hoverstate[df$open] = "#hoverStateOpen"
  df$name = as.character(df$name)

  doc = xml_new_root("kml",
                     xmlns = "http://www.opengis.net/kml/2.2",
                     "xmlns:gx" = "http://www.google.com/kml/ext/2.2",
                     "xmlns:kml" = "http://www.opengis.net/kml/2.2",
                     "xmlns:atom" = "http://w3.org/2005/Atom") %>%
    xml_add_child("Document") %>%
    xml_add_child("name", gsub("_", " ", strsplit(file_name, ".kml")[[1]])) %>%
    xml_add_sibling("open", 1) %>%

    ## Define the hover state for open stations
    xml_add_sibling("StyleMap", id = "hoverStateOpen") %>%
    xml_add_child("Pair") %>%
    xml_add_child("key", "normal") %>%
    xml_add_sibling("styleUrl", "#normalStateOpen") %>%
    xml_parent() %>%
    xml_add_sibling("Pair") %>%
    xml_add_child("key", "highlight") %>%
    xml_add_sibling("styleUrl", "#highlightStateOpen") %>%
    xml_parent() %>%
    xml_parent() %>%

    ## Define the hover state for closed stations
    xml_add_sibling("StyleMap", id = "hoverStateClosed") %>%
    xml_add_child("Pair") %>%
    xml_add_child("key", "normal") %>%
    xml_add_sibling("styleUrl", "#normalStateClosed") %>%
    xml_parent() %>%
    xml_add_sibling("Pair") %>%
    xml_add_child("key", "highlight") %>%
    xml_add_sibling("styleUrl", "#highlightStateClosed") %>%
    xml_parent() %>%
    xml_parent() %>%

    ## Define the highlight state for open stations
    xml_add_sibling("Style", id = "highlightStateOpen") %>%
    xml_add_child("IconStyle") %>%
    xml_add_child("scale", "1.3") %>%
    xml_add_sibling("Icon") %>%
    xml_add_child("href", "http://maps.google.com/mapfiles/kml/paddle/grn-circle.png") %>%
    xml_parent() %>%
    xml_parent() %>%
    xml_add_sibling("LabelStyle") %>%
    xml_add_child("scale", "1.3") %>%
    xml_parent() %>%
    xml_parent() %>%

    ## Define the highlight state for closed stations
    xml_add_sibling("Style", id = "highlightStateClosed") %>%
    xml_add_child("IconStyle") %>%
    xml_add_child("scale", "1.3") %>%
    xml_add_sibling("Icon") %>%
    xml_add_child("href", "http://maps.google.com/mapfiles/kml/paddle/red-circle.png") %>%
    xml_parent() %>%
    xml_parent() %>%
    xml_add_sibling("LabelStyle") %>%
    xml_add_child("scale", "1.3") %>%
    xml_parent() %>%
    xml_parent() %>%

    ## Define the normal state for open stations
    xml_add_sibling("Style", id = "normalStateOpen") %>%
    xml_add_child("IconStyle") %>%
    xml_add_child("scale", "0.9") %>%
    xml_add_sibling("Icon") %>%
    xml_add_child("href", "http://maps.google.com/mapfiles/kml/paddle/grn-circle.png") %>%
    xml_parent() %>%
    xml_parent() %>%
    xml_add_sibling("LabelStyle") %>%
    xml_add_child("scale", "0.7") %>%
    xml_parent() %>%
    xml_parent() %>%

    ## Define the normal state for closed stations
    xml_add_sibling("Style", id = "normalStateClosed") %>%
    xml_add_child("IconStyle") %>%
    xml_add_child("scale", "0.9") %>%
    xml_add_sibling("Icon") %>%
    xml_add_child("href", "http://maps.google.com/mapfiles/kml/paddle/red-circle.png") %>%
    xml_parent() %>%
    xml_parent() %>%
    xml_add_sibling("LabelStyle") %>%
    xml_add_child("scale", "0.7") %>%
    xml_parent() %>%
    xml_parent() %>%

    xml_add_sibling("Folder") %>%
    xml_add_child("name", "Closed")

  for (i in which(!df$open)) {
    doc = doc %>%
      xml_add_sibling("Placemark") %>%
      xml_add_child("name", df$name[i]) %>%
      xml_add_sibling("description", paste0("Agent:     ", df$agent[i], "\n",
                                            "Network:    ", df$network[i], "\n",
                                            "Start date: ", df$start[i], "\n",
                                            "End date:   ", df$end[i], "\n")) %>%
      xml_add_sibling("styleUrl", df$hoverstate[i]) %>%
      xml_add_sibling("Point") %>%
      xml_add_child("coordinates", paste(df$lon[i], df$lat[i], 0, sep = ",")) %>%
      xml_parent() %>%
      xml_parent()
  }

  doc = doc %>%
    xml_parent() %>%
    xml_add_sibling("Folder") %>%
    xml_add_child("name", "Open")

  for (i in which(df$open)) {
    doc = doc %>%
      xml_add_sibling("Placemark") %>%
      xml_add_child("name", df$name[i]) %>%
      xml_add_sibling("description", paste0("Agent:     ", df$agent[i], "\n",
                                            "Network:    ", df$network[i], "\n",
                                            "Start date: ", df$start[i], "\n",
                                            "End date:   ", df$end[i], "\n")) %>%
      xml_add_sibling("styleUrl", df$hoverstate[i]) %>%
      xml_add_sibling("Point") %>%
      xml_add_child("coordinates", paste(df$lon[i], df$lat[i], 0, sep = ",")) %>%
      xml_parent() %>%
      xml_parent()
  }

  doc = doc %>%
    xml_root()

  if (file_name == "my_stations_"){
    xml_file = tempfile(file_name, file_path, ".kml")
    write_xml(doc, file = xml_file)
  } else {
    if (!grepl(".kml$", file_name))
      file_name = paste0(file_name, ".kml")
    xml_file = file.path(file_path, file_name)
    write_xml(doc, file = xml_file)
  }

  message(paste("output KML file:", xml_file))
}

# Save KML files ----------------------------------------------------------

#' Save Clifro Station Information to a KML File
#'
#' Save \code{\link{cfStation}} object information to a KML file.
#'
#' The \code{cf_save_kml} function is for \code{\link{cfStation}}
#' objects to allow for the spatial visualisation of the selected stations. The
#' resulting KML file is saved and can then be opened by programs like Google
#' Earth (TM). The resultant KML file has the station names and locations shown
#' with green markers for open and red markers for closed stations. The agent
#' numbers, network ID's and date ranges are contained within the descriptions
#' for each station.
#'
#' If no file name is specified, unique names are produced in the current \R
#' working directory.
#'
#' @note The \code{.kml} suffix is appended automatically if it isn't already
#' present in the \code{file_name} argument.
#'
#' @param station \code{cfStation} object containing one or more stations
#' @param file_name file name for the resulting KML file
#' @param file_path file path for the resulting KML file
#'
#' @export
#' @seealso \code{\link{cf_station}} and \code{vignette("cfStation")} for
#' working with stations when the agent numbers are known, otherwise
#' \code{\link{cf_find_station}} and code{vignette("choose-station")} for
#' creating \code{cfStation} objects when the agent numbers are unknown.
#'
#' @examples
#' \dontrun{
#' # A selection of four Auckland region stations down the East Coast to the
#' # upper Waitemata Harbour; Leigh 2 Ews, Warkworth Ews, Tiri Tiri Lighthouse
#' # and Henderson
#' my.stations = cf_station(17838, 1340, 1401, 12327)
#' my.stations
#'
#' # Save these stations to a KML file
#' cf_save_kml(my.stations)
#'
#' # Double click on the file to open with a default program (if available). All
#' # the markers are green, indicating all these stations are open.
#'
#' # Where is the subscription-free Reefton Ews station?
#' cf_save_kml(cf_station(), file_name = "reeftonEWS")
#'
#' # It's located in the sou'west quadrant of Reefton town, in the upper, western
#' # part of the South Island, NZ.
#'
#' # Find all the open and closed Christchurch stations (using partial matching)
#' all.chch.st = cf_find_station("christ", status = "all", search = "region")
#'
#' # How many stations in total?
#' nrow(all.chch.st)
#'
#' # Save all the Christchurch stations
#' cf_save_kml(all.chch.st, file_name = "all_Chch_stations")
#' }
#' @importFrom methods is
cf_save_kml = function(station, file_name = "my_stations_",
                       file_path = "."){
  if (!is(station, "cfStation"))
    stop("station must be a cfStation object")
  if (grepl(".kml$", file_name))
    file_name = gsub(".kml$", "", file_name)
  save_KML(as(station, "data.frame"), file_name = file_name,
           file_path = normalizePath(file_path))
}

# Find stations -----------------------------------------------------------

#' Search for Clifro Stations
#'
#' Search for \pkg{clifro} stations based on name, region, location or network
#' number, and return a \code{cfStation} object.
#'
#' The \code{cf_find_station} function is a convenience function for finding
#' CliFlo stations in \R. It uses the CliFlo
#' \href{https://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn_html}{Find Stations}
#' page to do the searching, and therefore means that the stations are not
#' stored within \pkg{clifro}.
#'
#' If \code{datatype} is missing then the search is conducted
#' without any reference to datatypes. If it is supplied then the
#' search will only return stations that have any or all of the supplied
#' datatypes, depending on \code{combine}. The default behaviour is to search
#' for stations based on pattern matching the station name and return only the
#' open stations.
#'
#' If the \code{latlong} search type is used the function expects named
#' arguments with names (partially) matching latitude,
#' longitude and radius. If the arguments are passed in without names they must
#' be in order of latitude, longitude and radius (see examples).
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
#' searches although \pkg{clifro} does provide some extra functionality, see
#' the 'OR query Search' example below.
#'
#' @export
#' @importFrom httr GET modify_url
#' @importFrom xml2 read_html
#' @importFrom lubridate now %--% dseconds
#' @importFrom rvest html_table
#' @seealso \code{\link{cf_save_kml}} for saving the resulting stations as a KML
#' file, \code{\link{cf_station}} for creating \code{\link{cfStation}} objects
#' when the agent numbers are known, \code{vignette("choose-station")} for a
#' tutorial on finding \pkg{clifro} stations and \code{vignette("cfStation")}
#' for working with \code{\link{cfStation}} objects.
#' @examples
#' \dontrun{
#' # Station Name Search ------------------------------------------------------
#' # Return all open stations with 'island' in the name (pattern match search)
#' # Note this example uses all the defaults
#'
#' island_st = cf_find_station("island")
#' island_st
#'
#' # Region Search ------------------------------------------------------------
#' # Return all the closed stations from Queenstown (using partial matching)
#'
#' queenstown.st = cf_find_station("queen", search = "region", status = "closed")
#' queenstown.st
#'
#' # Long/Lat Search ----------------------------------------------------------
#' # Return all open stations within a 10km radius of the Beehive in Wellington
#' # From Wikipedia: latitude 41.2784 S, longitude 174.7767 E
#'
#' beehive.st = cf_find_station(lat = -41.2784, long = 174.7767, rad = 10,
#'                              search = "latlong")
#' beehive.st
#'
#' # Network ID Search --------------------------------------------------------
#' # Return all stations that share A42 in their network ID
#'
#' A42.st = cf_find_station("A42", search = "network", status = "all")
#' A42.st
#'
#' # Using Datatypes in the Search --------------------------------------------
#' # Is the Reefton EWS station open and does it collect daily rain and/or wind
#' # data?
#'
#' # First, create the daily rain and wind datatypes
#' daily.dt = cf_datatype(c(2, 3), c(1, 1), list(4, 1), c(1, NA))
#' daily.dt
#'
#' # Then combine into the search. This will only return stations where at least
#' # one datatype is available.
#' cf_find_station("reefton EWS", datatype = daily.dt)  # Yes
#'
#' # OR Query Search ----------------------------------------------------------
#' # Return all stations sharing A42 in their network ID *or* all the open
#' # stations within 10km of the Beehive in Wellington (note this is not
#' # currently available as a single query in CliFlo).
#'
#' cf_find_station("A42", search = "network", status = "all") +
#' cf_find_station(lat = -41.2784, long = 174.7767, rad = 10,
#'                 search = "latlong")
#'
#' # Note these are all ordered by open stations, then again by their end dates
#' }
cf_find_station = function(...,
                           search = c("name", "region", "network",
                                           "latlong"),
                           datatype,
                           combine = c("all", "any"),
                           status = c("open", "closed", "all")) {
  search = match.arg(arg = search)
  status = match.arg(arg = status)
  search_string = c(...)
  include_distances = FALSE
  cert = system.file("CurlSSL/cacert.pem", package = "RCurl")

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
             list(cstype = "region",
                  cRegion = search_region)
           },
           network = list(cstype = "net", cNet = search_string),
           latlong = {
             include_distances = TRUE
             list(cstype = "latlongc", 
                  clat1 = lat, 
                  clong1 = long,
                  crad = rad)
           }
    )

  param.list = c(param.list, 
                 mimeselection = "htmltable",
                 Submit = "Get Station List", 
                 status = status)

  if (!missing(datatype)){
    if (!is(datatype, "cfDatatype"))
      stop("datatype has to be a cfDatatype object")

    user = cf_user()
    cf_login(user)
    on.exit(cf_logout(user, msg = FALSE))
    combine = switch(match.arg(arg = combine),
                     all = "and",
                     any = "or")
    cf_update_dt(datatype)
    param.list = c(param.list, ccomb_dt = combine)
    my_form = GET(modify_url(url = "https://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn", 
                             query = param.list))
  } else {
    my_form = GET(modify_url(url = "https://cliflo.niwa.co.nz/pls/niwp/wstn.get_stn_nodt", 
                             query = param.list))
  }

  if (is.raw(my_form))
    my_form = rawToChar(my_form)

  doc = read_html(my_form)

  doc_table = tail(html_table(doc, header = TRUE, fill = TRUE), 1)[[1]]
  
  start_date_col = which(grepl("^Start", names(doc_table)))
  end_date_col = which(grepl("^End", names(doc_table)))
  
  if (missing(datatype)) {
    doc_table[[start_date_col]] = dmy(doc_table[[start_date_col]], tz = "NZ")
    doc_table[[end_date_col]][doc_table[[end_date_col]] == "-"] = format(Sys.Date(), "%d-%m-%Y")
    doc_table[[end_date_col]] = dmy(doc_table[[end_date_col]], tz = "NZ")
  } else {
    doc_table[[start_date_col]] = dmy(doc_table[[start_date_col]], tz = "NZ")
    doc_table[[end_date_col]][doc_table[[end_date_col]] == "-"] = format(Sys.Date(), "%d-%m-%Y")
    doc_table[[end_date_col]] = dmy(doc_table[[end_date_col]], tz = "NZ")
  }

  ## Open stations in clifro have end dates less than 4 weeks ago
  span = doc_table[[end_date_col]] %--% now(tzone = "NZ")
  open_station = (as.numeric(dseconds(span)) / (604800) < 4)

  ## Account for CliFlo giving outdated stations for certain datatypes
  if ((status == "open" && !any(open_station)) || (status == "closed" &&
                                                   all(open_station)))
    stop("no climate stations were found matching your search criteria",
         call. = FALSE)

  if (status == "open"){
    doc_table = doc_table[open_station, ]
    open_station = open_station[open_station]
  }

  if (status == "closed")
    doc_table = doc_table[!open_station, ]
  
  station_df = data.frame(
    name = doc_table$Name,
    network = doc_table$NetworkNumber,
    agent = doc_table$AgentNumber,
    start_date = doc_table[[start_date_col]],
    end_date = doc_table[[end_date_col]],
    open_station = switch(status,
                          open = TRUE,
                          closed = FALSE,
                          all = open_station),
    distances = NA,
    latitude = doc_table$`Lat(dec deg)`,
    longitude = doc_table$`Long(dec deg)`,
    stringsAsFactors = FALSE, check.names = TRUE
  )
  
  if (include_distances) {
    station_df$distances = doc_table$DistKm
  }

  new("cfStation",
      station_df)
}
