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
