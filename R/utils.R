#' checks if a variable has non-missing values
#'
#' @description The function checks if a variable is either null, NA, or has an
#' assigned value. In case of vectors, the condition is set on all values.
#'
#' @param x Variable to test.
#'
#' @examples
#' x <- NULL
#' isMissing(x)

isMissing = function(x) {
  if(!is.null(x)) {
    if( all( is.na(x) ) ) {
      return(TRUE)
    } else if( all( x == "NA" ) | all( x == "" ) ) {
      return(TRUE)
    } else {
        return(FALSE)
    }
  } else {
    return(TRUE)
  }
}
