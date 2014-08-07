#' @include cfDataList.R
NULL

# Parallel universe ------------------------------------------------------
cf_parallel = new.env()

# Store the Last Clifro Query 
set_history = function(value) cf_parallel[["last_cf_query"]] = value

#' Retrieve Last Query Result from CliFlo
#' 
#' Retrieve the last query submitted to CliFlo instead of querying the database
#' again and losing subscription rows.
#' 
#' This function is a back up for when the clifro query has been submitted and 
#' the data returned but has not been assigned. This saves the 
#' user resubmitting queries and using more rows from their subscription than 
#' needed.
#' 
#' @note Only the data from the last query is saved.
#' 
#' @export
cf_last_query = function() cf_parallel[["last_cf_query"]]

#  ------------------------------------------------------------------------

#' Retrieve Data from the National Climate Database
#' 
#' Query the National Climate Database via CliFlo based on the \pkg{clifro} user 
#' and selected datatypes, stations and dates.
#' 
#' The \code{cf_query} function is used to send the query built up from the 
#' \pkg{clifro} user and the selected \pkg{clifro} datatypes and stations to the 
#' National Climate Database via CliFlo.
#'
#' @param user a \code{\link{cfUser}} object.
#' @param datatype a \code{\link{cfDatatype}} object containing the datatypes to 
#'                 be retrieved.
#' @param station a \code{\link{cfStation}} object containing the stations where 
#'                the datatypes will be retrieved from.
#' @param start_date a character, Date or POSIXt object indicating the start 
#'                   date. If a character string is supplied the date format 
#'                   should be in the form \code{yyyy-mm-dd-hh} unless 
#'                   \code{date_format} is specified.
#' @param end_date a character, Date or POSIXt object indicating the start 
#'                 date. If a character string is supplied the date format 
#'                 is \code{yyyy-mm-dd-hh} unless \code{date_format} is 
#'                 specified. Defaults to \code{\link[lubridate]{now}}.
#' @param date_format a character string matching one of \code{"ymd_h"}, 
#'                    \code{"mdy_h"}, \code{"ydm_h"} or \code{"dmy_h"} 
#'                    representing the \code{\link[lubridate]{lubridate}} 
#'                    date parsing function.
#' @param tz the timezone for which the start and end dates refer to. Conversion
#'           to Pacific/Auckland time is done automatically through the 
#'           \code{\link[lubridate]{with_tz}} function. Defaults to
#'           "Pacific/Auckland".
#' @param quiet logical. When \code{TRUE} the function evaluates without 
#'              displaying customary messages. Messages from CliFlo are still 
#'              displayed.
#' 
#' @importFrom lubridate with_tz force_tz ymd_h mdy_h ydm_h dmy_h is.POSIXt year
#' month day hour
#' @importFrom RCurl getCurlHandle postForm
#' @importFrom selectr querySelector
#' @importFrom XML xmlValue htmlParse
#' @export
cf_query = function(user, datatype, station, start_date, end_date = now(tz),
                    date_format = "ymd_h", 
                    tz = "Pacific/Auckland", quiet = FALSE){
  
  if (!is(user, "cfUser"))
    stop("user must be a cfUser")
  if(!is(datatype, "cfDatatype"))
    stop("datatype must be a cfDatatype")
  if(!is(station, "cfStation"))
    stop("station must be a cfStation")
  
  if (user@username == "public" && is.na(match(3925, station@.Data[[3]])))
    stop("public users can only access data from Reefton EWS (3925)")
  
  date_format = match.arg(date_format, c("ymd_h", "mdy_h", "ydm_h", "dmy_h"))
  if (is.character(start_date)){
    start_date = eval(call(date_format, start_date, quiet = TRUE))
    if (is.na(start_date))
      stop("start date was not parsed with ", date_format)
  }
  if (is.character(end_date)){
    end_date = eval(call(date_format, end_date, quiet = TRUE))
    if (is.na(end_date))
      stop("end date was not parsed with ", date_format)
  }
  
  if (!(is.POSIXt(start_date) || is.POSIXt(end_date)))
    stop("start and end dates must be either character or POSIXt objects")
  
  cf_login(user)
  on.exit(cf_logout(user, msg = FALSE))
  cookies = file.path(tempdir(), user@username)
  curl = getCurlHandle(followlocation = TRUE,
                       timeout = 100, 
                       useragent = 
                         paste("clifro", R.Version()$version.string),
                       cookiefile = cookies, 
                       cookiejar = cookies)
  all_dt_params = c(datatype@dt_param, unlist(datatype@dt_sel_option_params))
  if (!quiet)
    message("waiting for CliFlo...")
  if (nrow(station) > 20){
    station = station[1:20]
    message("using the first 20 stations")
  }
  doc = postForm("http://cliflo.niwa.co.nz/pls/niwp/wgenf.genform1_proc",
                 cselect = "wgenf.genform1?fset=defdtype",
                 auswahl = "wgenf.genform1?fset=defagent",
                 agents = paste(station$agent, collapse = ","),
                 dateauswahl = "wgenf.genform1?fset=defdate",
                 date1_1=year(start_date),
                 date1_2=month(start_date),
                 date1_3=day(start_date),
                 date1_4=hour(start_date),
                 date2_1=year(end_date),
                 date2_2=month(end_date),
                 date2_3=day(end_date),
                 date2_4=hour(end_date),
                 formatselection = "wgenf.genform1?fset=deffmt",
                 TSselection = "NZST",
                 dateformat = "0",
                 Splitdate = "N",
                 mimeselection = "texttab",
                 cstn_id = "N",
                 cdata_order = "SD",
                 submit_sq = "Send Query",
                 .params = all_dt_params,
                 curl = curl)
  
  is_HTML = grepl("<!DOCTYPE HTML PUBLIC", doc, fixed = TRUE)
  if (is_HTML){
    error_msg = xmlValue(querySelector(htmlParse(doc), "h3"))
    if (!is.na(error_msg))
      stop(error_msg)
  }
  
  if (!quiet)
    message("reading data...")
  
  all_lines = readLines(textConnection(doc))
  table_limits = head(which(all_lines == ""), -2)
  table_names = all_lines[head(table_limits, -1) + 1]
  if (grepl("No rows", tail(table_names, 1), fixed = TRUE))
    stop(gsub("?", ".", tail(table_names, 1), fixed = TRUE))
  dt_names = sapply(strsplit(table_names, ":"), "[", 1)
  dt_types = sapply(strsplit(table_names, ":"), "[", 2)
  tail_msg = paste(all_lines[(tail(table_limits, 1) + 1):length(all_lines)], 
                   collapse = "\n")
  table_limits = split(sort(c(head(table_limits, -1), tail(table_limits, -1))),
                       rep(seq_along(table_limits)[-1], each = 2))
  
  data_list = lapply(table_limits, 
                     function(x) 
                       read.table(textConnection(all_lines[(x[1] + 2):(x[2] - 1)]), 
                                  sep = "\t", header = TRUE, na.strings = "-",
                                  check.names = FALSE))
  nrows = sapply(data_list, nrow)
  data_list = data_list[nrows != 0]
  dt_names = dt_names[nrows != 0]
  dt_types = dt_types[nrows != 0]
  nrows = nrows[nrows != 0]
  head_names = lapply(data_list, names)
  
  clifro_data_list = vector("list", length(data_list))
  
  for (i in seq_along(data_list)){
    clifro_data_list[[i]] = new("cfData",
                                dt_name = dt_names[i],
                                dt_type = dt_types[i],
                                names = head_names[[i]],
                                row.names = paste(seq_len(nrows[i])),
                                as(data_list[[i]], "list"))
  }
  if (!quiet)
    message(tail_msg)
  
  cf_data_list = new("cfDataList", clifro_data_list)
  
  if (length(clifro_data_list) == 1){
    set_history(cf_data_list[[1]])
    return(cf_data_list[[1]])
  }
  set_history(cf_data_list)
  cf_data_list
}
