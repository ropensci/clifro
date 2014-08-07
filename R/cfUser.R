## Internal function to hide password
rot <- function(ch, k) {
  p0 <- function(...) paste(c(...), collapse = "")
  A <- c(letters, LETTERS, " '", paste(0:9))
  I <- seq_len(k)
  chartr(p0(A), p0(c(A[-I], A[I])), ch)
}

#  ------------------------------------------------------------------------

#' From CliFlo to \pkg{clifro}: Enhancing The National Climate Database With R
#' 
#' Imports data from New Zealand's National Climate Database via CliFlo into \R
#' and provides data visualisation functions.
#' 
#' The \pkg{clifro} package is intended to simplify the process of data 
#' extraction, formatting and visualisation from the CliFlo web portal. It 
#' requires the user to build a query consisting of 3 main components; the user, 
#' the datatype(s) and the station(s). These are
#' then combined using the \code{\link{cf_query}} function which sends the query 
#' to the CliFlo database and returns the results that can easily be plotted
#' using the \code{\link[clifro]{plot}} function.
#' 
#' This package requires the user to already have a current subscription to the
#' National Climate Database unless a public user is sought where data is 
#' limited to Reefton Ews. Subscription is free and can obtained from
#' \url{http://cliflo.niwa.co.nz/pls/niwp/wsubform.intro}.
#' 
#' @name clifro
#' @aliases clifro-package
#' @docType package
#' @keywords package
#' @example demo/clifro.R
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
#' cliflo and deletes the cookies. This should be (is) called at the end of any 
#' function using \code{cf_login} to ensure the user isn't still logged in
#' on the server, even though they may have closed the \R session.
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
#' @importFrom RCurl getCurlHandle postForm getURL
#' @importFrom XML htmlParse xmlValue
#' @importFrom selectr querySelector
#' @keywords internal
#' @aliases cf_logout cf_login
#' @name valid_cfuser
#' @rdname valid_cfuser
#' @examples
#' \dontrun{
#' cf_user("public")                    # Returns a valid object
#' cf_user("bad_name", "bad_password")    # Bad Login
#' }

cf_login = function(object){
  cookies = file.path(tempdir(), object@username)
  curl = getCurlHandle(followlocation = TRUE,
                       cookiejar = cookies, 
                       cookiefile = cookies,
                       useragent = paste("clifro", R.Version()$version.string),
                       timeout = 100)
  if (object@username == "public"){
    login_html = htmlParse(getURL(
      "http://cliflo.niwa.co.nz/pls/niwp/wgenf.genform1",
      curl = curl
    ))
    result = "Info"
  }
  else{
    login_html = htmlParse(postForm(
      "http://cliflo.niwa.co.nz/pls/niwp/wa.logindb",
      cusername = object@username,
      cpwd = rot(object@password, 3),
      ispopup = "false", 
      submit = "login",
      curl = curl))
    result = xmlValue(querySelector(login_html, "h1"))
  }
  rm(curl)
  gc()
  return(grepl("Info", result))
}

#' @rdname valid_cfuser
#' @importFrom RCurl getCurlHandle getURLContent getURL
cf_logout = function(object, msg = TRUE){
  cookies = file.path(tempdir(), object@username)
  curl = getCurlHandle(followlocation = TRUE, 
                       timeout = 100, 
                       useragent = 
                         paste("clifro", R.Version()$version.string),
                       cookiefile = cookies,
                       cookiejar = cookies)
  
  header = getURLContent("http://cliflo.niwa.co.nz/pls/niwp/wa.logout",
                         curl = curl, header = TRUE)
  if (!grepl("OK", header$header[11]))
    stop("HTTP error")
  
  getURL("http://cliflo.niwa.co.nz/pls/niwp/wa.logout", curl = curl)
  
  file.remove(cookies)
  if (msg)
    message("Logout successful")
}

#' @rdname valid_cfuser
valid_cfuser = function(object){
  length_username = length(object@username)
  length_password = length(object@password)
  errors = character()
  #   errors <- character()
  #   if (!is.numeric(object@.Data)) {
  #     msg <- "Span length must be numeric."
  #     errors <- c(errors, msg)
  #   }
  
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

#' The Clifro User
#' 
#' Allow the user to log into cliflo from \R to build their query.
#' 
#' An object inheriting from the cfUser class is created by the constructor 
#' function \code{cf_user}. The user must have an active subscription to cliflo 
#' in order to create a valid object, unless a 'public' user is sought. 
#' Visit \url{http://cliflo.niwa.co.nz/} for more information and to subscribe 
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
#' @export
#' @seealso \code{\link{valid_cfuser}} for details on the validation of cfUser
#' and \code{\link[clifro]{summary}} to summarise user information.
#' @examples
#' public.cfuser = cf_user(username = "public")
#' public.cfuser
cf_user = function(username = "public", password = character()){
  new("cfUser", username = username, password = password)
}

# Initialize the cfUser with a cryptic password
#' @importFrom methods setMethod
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
#' Summarise User Information
#' 
#' Show the subscription status for the \pkg{clifro} user
#' 
#' @param object an object of class \code{cfUser}.
#' 
#' @return Invisibly returns a list with the following components:
#' \tabular{ll}{
#' \strong{days.remain} \tab a time interval showing the number of 
#' subscription days remaining \cr
#' \strong{rows.used} \tab total number of used rows \cr
#' \strong{rows.total} \tab total number of rows in the subscription \cr
#' \strong{rows.remain} \tab number of rows remaining the in subscription\cr
#' \strong{subscription} \tab subscription status\cr
#' }
#' @importFrom RCurl getCurlHandle getForm
#' @importFrom selectr querySelectorAll querySelector
#' @importFrom XML htmlParse xmlValue
#' @importFrom lubridate dmy round_date now with_tz
#' @name summary
#' @aliases summary-methods
#' @aliases summary-clifro
#' @aliases summary,cfUser-method
#' @export
setMethod("summary", signature(object = "cfUser"), 
          function(object){
  if (object@username == "public")
    return(object)
  cf_login(object)
  on.exit(cf_logout(object, msg = FALSE))
  cookies = file.path(tempdir(), object@username)
  curl = getCurlHandle(followlocation = TRUE, 
                       timeout = 100, 
                       useragent = 
                         paste("clifro", R.Version()$version.string),
                       cookiefile = cookies, 
                       cookiejar = cookies)
  user_info_xml = 
    getForm("http://cliflo.niwa.co.nz/pls/niwp/wa.subscr_info",
            sub = "t",
            curl = curl)
  user_info_html = querySelectorAll(htmlParse(user_info_xml),
                                    "body.popup > div")
  info = gsub("  |   |    |     ", " ", sapply(user_info_html, xmlValue))
  rows = sapply(querySelectorAll(user_info_html[[3]], "b"), xmlValue)
  rows = gsub(",", "", rows)
  subscription_level = xmlValue(querySelector(user_info_html[[5]], "b"))
  expiry = strsplit(info[2], ": ")[[1]][2]
  rows_used = as.numeric(rows[1])
  total_rows = as.numeric(rows[3])
  time_diff = dmy(expiry, tz = "Pacific/Auckland") - 
    with_tz(round_date(now(), "month"), "Pacific/Auckland")
  cat(paste0("Username is: ", object@username, "\n",
               "Subscription status:\n\n",
               "Your subscription expires on: ", expiry, " (", format(time_diff), 
               ")\n", "You have used ", 
               format(rows_used, big.mark = ",", scientific = FALSE),
               " rows (", round(rows_used / total_rows * 100, 1), "%)\n",
               "from a subscription total of ", 
               format(total_rows, big.mark = ",", scientific = FALSE), " rows.\n",
               "Remaining rows: ", 
               format(total_rows - rows_used, big.mark = ",",
                      scientific = FALSE), ".\n",
               "Your subscription level is: ", subscription_level, "\n"))
  invisible(list(days.remain = time_diff, rows.used = rows_used, 
                 rows.total = total_rows, rows.remain = total_rows - rows_used,
                 subscription = subscription_level))
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
