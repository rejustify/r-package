#curl configuration
setCurl = function(mainUrl   = "http://api.rejustify.com",
                   proxy     = getOption("rejustify.proxy"),
                   proxyUrl  = getOption("rejustify.proxyUrl"),
                   proxyPort = getOption("rejustify.proxyPort") ) {

  #set the configuration values
  options(rejustify.mainUrl   = mainUrl)
  options(rejustify.proxy     = proxy)
  options(rejustify.proxyUrl  = proxyUrl)
  options(rejustify.proxyPort = proxyPort)
}
