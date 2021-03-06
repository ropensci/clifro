% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfUser.R
\name{valid_cfuser}
\alias{valid_cfuser}
\alias{cf_login}
\alias{cf_logout}
\title{Validation Functions For The \code{cfUser} Class}
\usage{
cf_login(object, ...)

cf_logout(object, msg = TRUE, ...)

valid_cfuser(object)
}
\arguments{
\item{object}{S4 object which inherits the \code{cfUser} class}

\item{...}{Other options passed to the \code{\link[httr]{GET}} or \code{\link[httr]{POST}} functions.}

\item{msg}{Display a 'successful logout' message, defaults to
\code{TRUE}.}
}
\description{
These internal functions are used by the \code{\link{cf_user}} constructor
function to ensure the user has a valid subscription to CliFlo.
}
\details{
\code{cf_login} initiates a curl handle storing the cookies in the current
\R session's temporary directory. It then POSTs the user credentials to the
CliFlo login page and stores the resultant \code{h1} heading to check for the
string 'Info'. The cookies are kept for future (immediate) use.

\code{cf_logout} points the curl handle to the existing cookie session
initiated with \code{cf_login}. It reads the header information from the
cliflo logout page to ensure no HTTP error and logs the user out on
cliflo and deletes the cookies. This should be (is) called immediately after
\code{cf_login} in any function requiring a login, using
\code{\link{on.exit}} to ensure the user isn't still logged in on the server,
after the function call, for any reason.

\code{valid_cfuser} is the validation function for the \code{cfUser} class
and uses  \code{cf_login} to ensure the credentials are authenticated on the
CliFlo server and then (\code{cf_})logs out immediately afterwards. It also
ensures the user provides exactly one username and password - except for
'public' users.
}
\examples{
\dontrun{
cf_user("public")                    # Returns a valid object
cf_user("bad_name", "bad_password")    # Bad Login
}
}
\keyword{internal}
