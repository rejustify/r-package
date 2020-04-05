#' communicates with rejustify/getTableLabels API endpoint
#'
#' @description The function returns the list of available labels for data set \code{table}
#' delivered by \code{provider}. If not specified otherwise by \code{dim}, the labels are returned for all dimensions.
#' For the list of available provider/tables you can refer to \code{rejustify.com/repos}.
#'
#' @param provider Relevant provider (required).
#' @param table Relevant provider (required).
#' @param dim Relevant dimension from \code{provider}/\code{table} (optional).
#' @param url API url. By default read from global variables.
#'
#' @return list containing the pairs of value code and label
#' @examples
#' #API setup
#' setCurl()
#' labels('IMF','WEO')
#' labels('IMF','WEO','WEO Country')
#'
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export

labels = function( provider = NULL,
                   table    = NULL,
                   dim      = NULL,
                   url      = getOption("rejustify.mainUrl") ) {

  ###########
  # consistency checks
  ###########

  if( is.null(provider) | is.null(table) ) {
    stop(
      paste0(
        "Incorrect provider or table."
      ),
      call. = FALSE
    )
  }

  #get all labels
  url       <- paste0(url, "/getTableLabels?provider=",provider,"&table=",table)
  response  <- callCurl("GET", url)

  if(response$status_code == 200) {

    response  <- content(response, as="text")
    labels    <- fromJSON(response)

    #clean values for Primary Measure
    labels$structure$labels$`Primary Measure`<-NULL

    #display values
    if(is.null(dim)) {
      return( labels$structure$labels )
    } else {
      return( labels$structure$labels[[dim]] )
    }

  } else {
    stop(
      paste0(
        "Unrecognized response from API. Check if the provider/table values are correct."
      )
    )
  }

}
