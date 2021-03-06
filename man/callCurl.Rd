% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callCurl.R
\name{callCurl}
\alias{callCurl}
\title{a wrapper of the httr GET/POST functions}
\usage{
callCurl(
  method = "GET",
  url = NULL,
  body = NULL,
  proxyUrl = getOption("rejustify.proxyUrl"),
  proxyPort = getOption("rejustify.proxyPort")
)
}
\arguments{
\item{method}{Method of the call (GET or POST).}

\item{url}{Address of the API.}

\item{body}{Request body in case of using POST method.}

\item{proxyUrl}{Url address of the proxy server.}

\item{proxyPort}{Communication port of the proxy server.}
}
\value{
API response or errors
}
\description{
The function offers a basic functionality of commands GET/POST from \code{httr}
package to communicate with the APIs. If needed, the proxy settings can be given explicitly, or set
in global variables \code{rejustify.proxyUrl} and \code{rejustify.proxyPort}.
}
