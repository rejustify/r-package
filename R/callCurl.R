#' a wrapper of the httr GET/POST functions
#'
#' @description The function offers a basic functionality of commands GET/POST from \code{httr}
#' package to communicate with the APIs. If needed, the proxy settings can be given explicitly, or set
#' in global variables 'rejustify.proxyUrl' and 'rejustify.proxyPort'.
#'
#' @param method Method of the call (GET or POST).
#' @param url Address of the API.
#' @param body Request body in case of using POST method.
#' @param proxyUrl Url address of the proxy server.
#' @param proxyPort Communication port of the proxy server.
#'
#' @return API response or errors
#'
#' @importFrom httr use_proxy add_headers GET POST
#' @export

callCurl = function(method    = "GET",
                    url       = NULL,
                    body      = NULL,
                    proxyUrl  = getOption("rejustify.proxyUrl"),
                    proxyPort = getOption("rejustify.proxyPort") ) {

  #check the method type
  if( !( toupper(method) == "GET" | toupper(method) == "POST" ) ) {
    callFun = NULL
  } else {
    callFun = match.fun( toupper( method ) )
  }

  response <- NULL
  if( !is.null(proxyUrl) ) {
    tryCatch({
      response <- callFun(url,
                          use_proxy(proxyUrl, proxyPort),
                          add_headers('Content-Type'='application/json'),
                          body = body,
                          encode = "raw" )
    }, error = function(e) {
      stop(
        paste0(
          "There was a problem when accessing the API."
        ),
        call. = FALSE
      )
    })
  } else {
    tryCatch({
      response <- callFun(url,
                          add_headers('Content-Type'='application/json'),
                          body = body,
                          encode = "raw" )
    }, error = function(e) {
      stop(
        paste0(
          "There was a problem when accessing the API."
        ),
        call. = FALSE
      )
    })
  }
  return(response)
}
