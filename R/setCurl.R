#' sets the default configuration for curl calls
#'
#' @description The command stores the connection details into memory to be easier accessed by
#' rejustify package.
#'
#' @param mainUrl Main address for rejustify API calls. Default is set to
#' \code{http://api.rejustify.com}, but depending on the customer needs, the address may change.
#' @param proxyUrl Address of the proxy server.
#' @param proxyPort Port for communication with the proxy server.
#'
#' @examples
#' #setting up connection through proxy
#' rejustify::setCurl(proxyUrl = "PROXY_ADDRESS", proxyPort = 8080)
#'
#' @export

setCurl = function(mainUrl   = "http://api.rejustify.com",
                   proxyUrl  = getOption("rejustify.proxyUrl"),
                   proxyPort = getOption("rejustify.proxyPort") ) {

  #set the configuration values
  options( rejustify.mainUrl   = mainUrl )
  options( rejustify.proxy     = ifelse(is.null(proxyUrl), FALSE, TRUE) )
  options( rejustify.proxyUrl  = proxyUrl )
  options( rejustify.proxyPort = proxyPort )
}
