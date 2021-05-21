# Parallel universe ------------------------------------------------------

# Set up a package-specific environment to store local variables.
cf_parallel = new.env()

#  ------------------------------------------------------------------------
#' Store curl options for use within \pkg{clifro}
#'
#' The \code{cf_curl_opts} function stores specific curl options that are used
#' for all the \pkg{clifro} queries.
#'
#' @param ... a name-value pairs that are passed to \code{RCurl curlOptions}
#' @param .opts a named list or \code{CURLOptions} object that are passed to \code{RCurl curlOptions}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify options for use in all the curl handles created in clifro
#' cf_curl_opts(.opts = list(proxy = "http://xxxxx.yyyy.govt.nz:8080",
#'                           proxyusername  = "uid",
#'                           proxypassword  = "pwd",
#'                           ssl.verifypeer = FALSE))
#' # Or alternatively:
#' cf_curl_opts(proxy = "http://xxxxx.yyyy.govt.nz:8080",
#'              proxyusername  = "uid",
#'              proxypassword  = "pwd",
#'              ssl.verifypeer = FALSE)
#' }
cf_curl_opts = function(..., .opts = list()){
  .Deprecated(new = "httr::set_config()", package = "clifro")
}

#  ------------------------------------------------------------------------

#' From CliFlo to \pkg{clifro}: Enhancing The National Climate Database With \R
#'
#' Import data from New Zealand's National Climate Database via CliFlo into \R
#' for exploring, analysis, plotting, exporting to KML, CSV, or other software.
#'
#' The \pkg{clifro} package is intended to simplify the process of data
#' extraction, formatting and visualisation from the
#' \href{https://cliflo.niwa.co.nz/}{CliFlo web portal}. It
#' requires the user to build a query consisting of 3 main components; the user,
#' the datatype(s) and the station(s). These are
#' then combined using the \code{\link{cf_query}} function that sends the query
#' to the CliFlo database and returns the results that can easily be plotted
#' using generic plotting functions.
#'
#' This package requires the user to already have a current subscription to the
#' National Climate Database unless a public user is sought, where data is
#' limited to Reefton Ews. Subscription is free and can obtained from
#' \url{https://cliflo.niwa.co.nz/pls/niwp/wsubform.intro}.
#'
#' @seealso \code{\link{cf_user}}, \code{\link{cf_datatype}}, and
#'   \code{\link{cf_station}} for choosing the clifro user, datatypes and
#'   stations, respectively.
#' @name clifro
#' @aliases clifro-package
#' @docType package
#' @keywords package
#' @examples
#' \dontrun{
#' # Create a public user ----------------------------------------------------
#'
#' public.user = cf_user() # Defaults to "public"
#' public.user
#'
#' # Select datatypes --------------------------------------------------------
#'
#' # 9am Surface wind (m/s)
#' wind.dt = cf_datatype(2, 1, 4, 1)
#'
#' # Daily Rain
#' rain.dt = cf_datatype(3, 1, 1)
#'
#' # Daily temperature extremes
#' temp.dt = cf_datatype(4, 2, 2)
#'
#' # Combine them together
#' all.dts = wind.dt + rain.dt + temp.dt
#' all.dts
#'
#' # Select the Reefton Ews station ------------------------------------------
#'
#' reefton.st = cf_station()
#' reefton.st
#'
#' # Submit the query --------------------------------------------------------
#'
#' # Retrieve all data from ~ six months ago at 9am
#' reefton.data = cf_query(public.user, all.dts, reefton.st,
#'                         paste(as.Date(Sys.time()) - 182, "9"))
#' reefton.data
#'
#'
#' # Plot the data -----------------------------------------------------------
#'
#' # Plot the 9am surface wind data (first dataframe in the list) ---
#' reefton.data[1]
#'
#' # all identical - although passed to different methods
#' plot(reefton.data)    #plot,cfDataList,missing-method
#' plot(reefton.data, 1) #plot,cfDataList,numeric-method
#' plot(reefton.data[1]) #plot,cfData,missing-method --> plot,cfWind,missing-method
#'
#' speed_plot(reefton.data)
#' direction_plot(reefton.data)
#'
#' # Plot the daily rain data (second dataframe in the list) ---
#' reefton.data[2]
#'
#' # With runoff and soil deficit
#' plot(reefton.data, 2)
#'
#' # Just plot amount of rain (mm)
#' plot(reefton.data, 2, include_runoff = FALSE)
#'
#' # Plot the hourly temperature data (third dataframe in the list) ---
#' plot(reefton.data, 3)
#'
#' # Pass an argument to ggplot2::theme
#' library(ggplot2) # for element_text()
#' plot(reefton.data, 3, text = element_text(size = 18))
#' }
NULL

# Validation (internals) --------------------------------------------------

#' Validation Functions For The \code{cfUser} Class
#'
#' These internal functions are used by the \code{\link{cf_user}} constructor
#' function to ensure the user has a valid subscription to CliFlo.
#'
#' \code{cf_login} initiates a curl handle storing the cookies in the current
#' \R session's temporary directory. It then POSTs the user credentials to the
#' CliFlo login page and stores the resultant \code{h1} heading to check for the
#' string 'Info'. The cookies are kept for future (immediate) use.
#'
#' \code{cf_logout} points the curl handle to the existing cookie session
#' initiated with \code{cf_login}. It reads the header information from the
#' cliflo logout page to ensure no HTTP error and logs the user out on
#' cliflo and deletes the cookies. This should be (is) called immediately after
#' \code{cf_login} in any function requiring a login, using
#' \code{\link{on.exit}} to ensure the user isn't still logged in on the server,
#' after the function call, for any reason.
#'
#' \code{valid_cfuser} is the validation function for the \code{cfUser} class
#' and uses  \code{cf_login} to ensure the credentials are authenticated on the
#' CliFlo server and then (\code{cf_})logs out immediately afterwards. It also
#' ensures the user provides exactly one username and password - except for
#' 'public' users.
#'
#' @param object S4 object which inherits the \code{cfUser} class
#'
#'@param msg Display a 'successful logout' message, defaults to
#' \code{TRUE}.
#' 
#' @param ... Other options passed to the \code{\link[httr]{GET}} or \code{\link[httr]{POST}} functions.
#'
#' @importFrom httr POST modify_url stop_for_status user_agent timeout
#' @importFrom rvest html_node html_text
#' @importFrom xml2 read_html
#' @importFrom utils packageVersion
#' @keywords internal
#' @aliases cf_logout cf_login
#' @name valid_cfuser
#' @rdname valid_cfuser
#' @examples
#' \dontrun{
#' cf_user("public")                    # Returns a valid object
#' cf_user("bad_name", "bad_password")    # Bad Login
#' }
cf_login = function(object, ...) {
  r = POST(modify_url("https://cliflo.niwa.co.nz", 
                      path = "/pls/niwp/wa.logindb"),
           body = list(cusername = object@username,
                       cpwd = rot(object@password, 3),
                       submit = "login"),
           user_agent(paste("clifro", packageVersion("clifro"), sep = "/")),
           timeout(10), ...)
  
  stop_for_status(r)
  
  html_title = html_text(html_node(read_html(r), "title"))
  return(html_title != "Bad Login")
}

#' @rdname valid_cfuser
#' @importFrom httr warn_for_status GET modify_url
#' @importFrom utils packageVersion

cf_logout = function(object, msg = TRUE, ...) {
  r = GET(modify_url("https://cliflo.niwa.co.nz", 
                     path = "/pls/niwp/wa.logout"),
          user_agent(paste("clifro", packageVersion("clifro"), sep = "/")),
          timeout(10), ...)

  warn_for_status(r)
  
  if (msg)
    message("logout successful")
}

#' @rdname valid_cfuser
valid_cfuser = function(object){
  length_username = length(object@username)
  length_password = length(object@password)
  errors = character()

  if (length_username != 1){
    msg = "Exactly one username must be specified"
    errors = c(errors, msg)
  }

  if (tolower(object@username) != "public" && length_password != 1){
    msg = "Exactly one password must be specified"
    errors = c(errors, msg)
  }

  if (tolower(object@username) != "public"){
    login_OK = cf_login(object)
    if (login_OK)
      on.exit(cf_logout(object, msg = FALSE))
  } else
    login_OK = TRUE

  if (!login_OK){
    msg = "Bad Login"
    errors = c(errors, msg)
  }

  if (length(errors) == 0)
    TRUE
  else
    errors
}

# cfUser class ------------------------------------------------------------

#' @rdname cfUser-class
#' @name cfUser-class
#' @aliases cf_user
#' @importFrom methods setClass
setClass("cfUser",
         representation = representation(username = "character",
                                         password = "character"),
         validity = valid_cfuser)

#' The Clifro User Object
#'
#' Create a \code{cfUser} object to allow the user to log into CliFlo from \R
#' and  build their query.
#'
#' An object inheriting from the \code{cfUser} class is created by the constructor
#' function \code{cf_user}. The user must have an active subscription to cliflo
#' in order to create a valid object, unless a 'public' user is sought.
#' Visit \url{https://cliflo.niwa.co.nz/} for more information and to subscribe
#' to cliflo.
#'
#' @param username a character string to be used as the cliflo username
#' @param password a character string to be used as the cliflo password
#'
#' @note For the 'public' user (see examples) only the Reefton Ews station data
#' is available.
#'
#' @importFrom methods new
#' @rdname cfUser-class
#' @name cfUser-class
#' @aliases cfUser
#' @aliases cfUser-class
#' @return \code{cfUser} object
#' @export
#' @seealso \code{\link{valid_cfuser}} for details on the validation of
#' \code{cfUser} and \code{\link{summary,cfUser-method}} to summarise user
#' information.
#' @examples
#' \dontrun{
#' public.cfuser = cf_user(username = "public")
#' public.cfuser
#' }
cf_user = function(username = "public", password = character()){
  new("cfUser", username = username, password = password)
}

# Initialize the cfUser with a cryptic password
#' @importFrom methods setMethod validObject
setMethod("initialize", "cfUser", function(.Object, username, password){
  .Object@username = username
  if (length(password) == 1){
    .Object@password = rot(password, 60)
  }
  else
    .Object@password = password

  validObject(.Object)
  return(.Object)
})

# Methods -----------------------------------------------------------------

#'@importFrom methods setGeneric
if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...) standardGeneric("summary"))

#' Summarise User Information
#'
#' Show the subscription status for the \pkg{clifro} user
#'
#' @param object an object of class \code{cfUser}.
#'
#' @importFrom httr GET modify_url
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom lubridate dmy now with_tz
#' @aliases summary,cfUser-method
#' @export
setMethod("summary", signature(object = "cfUser"),
          function(object){
  if (object@username == "public")
    return(object)
  cf_login(object)
  on.exit(cf_logout(object, msg = FALSE))
  
  user_info = GET(modify_url("https://cliflo.niwa.co.nz",
                             path = "pls/niwp/wa.subscr_info?sub=t"))
  
  user_info_text = html_text(html_nodes(read_html(user_info), "div"))
  subscription_expiry = dmy(user_info_text[2], tz = "Pacific/Auckland")
  
  user_info_numbers = html_text(html_nodes(read_html(user_info), "div b"), trim = TRUE)
  
  time_diff = subscription_expiry - with_tz(now(), "Pacific/Auckland")
  
  cat(paste0(user_info_text[1], "\n",
             "Subscription status:\n\n",
             "Your subscription expires on: ", format(subscription_expiry, "%d-%B-%Y"), " (", format(round(time_diff, 1)),
             ")\n", "You have used ", user_info_numbers[1],
             " rows (", user_info_numbers[2], ") ",
             "from a subscription total of ",
             user_info_numbers[3], " rows.\n",
             "Remaining rows: ",
             user_info_numbers[4], ".\n",
             "Your subscription level is: ", user_info_numbers[5], "\n"))
})

# Show
#' @importFrom methods setMethod
setMethod("show", "cfUser", function(object){
  status = "Authenticated clifro User\n"
  if (tolower(object@username) == "public")
    message("public user - only data from Reefton Ews (3925) available")
  else
    cat(paste0(status, "Username is: ", object@username, "\n"))
})

## Internal function to hide password
rot <- function(ch, k) {
  p0 <- function(...) paste(c(...), collapse = "")
  A <- c(letters, LETTERS, " '", paste(0:9))
  I <- seq_len(k)
  chartr(p0(A), p0(c(A[-I], A[I])), ch)
}
