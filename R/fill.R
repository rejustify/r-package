#' \code{fill} submits the dataset and structure to the fill API endpoint
#' to retrieve the desired extra data points. At the current stage
#' dataset must be rectangular, and structure should be in the shape proposed
#' analyze function.
#'
#' @examples
#' fill(df, structure, modify)
#'
fill = function( df    = NULL,
                 structure = NULL,
                 keys  = NULL,
                 meta  = NULL,
                 shape = "vertical",
                 inits = 1,
                 fast  = TRUE,
                 sep   = ",",
                 db    = FALSE,
                 accu  = 0.75,
                 token = getOption("rejustify.token"),
                 email = getOption("rejustify.email"),
                 url   = getOption("rejustify.mainUrl") ) {

  ###########
  #structure preparation
  ###########

  tryCatch({
    structure <- as.data.frame(structure, stringsAsFactor = F)
    structure[is.na( structure )] <- "NA" #correct for missing values
  }, error = function(e) {
    stop(
      paste0(
        "Incorrect structure."
      ),
      call. = FALSE
    )
  })

  ###########
  #data preparation
  ###########

  #adjust the data format
  if(shape == "vertical") {
    names <- colnames(df)
  } else {
    names <- rownames(df)
  }

  #verify the data shape
  tryCatch({
    df <- as.matrix( df )
    colnames(df) <- NULL
    rownames(df) <- NULL
  }, error = function(e) {
    stop(
      paste0(
        "The shape of the dataset is currently not supported."
      ),
      call. = FALSE
    )
  })

  #set column/row headers
  if( is.null( names ) ) {
    if(shape == "vertical") {
      names <- paste( "Column", seq(1: ncol(df)) )
    } else {
      names <- paste( "Row", seq(1: nrow(df)) )
    }
    warning(
      paste0(
        "The dataset doesn't have column/row names."
      ))
  }

  #create dataset
  if(shape == "vertical") {
    df <- rbind( names, df)
    rownames(df) <- NULL
  } else {
    df <- cbind( names, df)
    colnames(df) <- NULL
  }

  ###########
  #API call
  ###########

  #prepare the payload query
  payload  <- list(structure   = structure,
                   data        = df,
                   keys        = NULL,
                   meta        = NULL,
                   userToken   = token,
                   email       = email,
                   dbAllowed   = db,
                   minAccuracy = accu,
                   sep         = sep,
                   direction   = shape,
                   inits       = inits)

  #url <- 'http://loadbalancer-b-12788309.eu-west-1.elb.amazonaws.com/fill?user_id=cloud@rejustify.com&db_allowed=0&min_accuracy=0.75&form=reduced'
  #url <- 'http://localhost:5762/fill?user_id=cloud@rejustify.com&db_allowed=0&min_accuracy=0.75&form=reduced'
  #url   = getOption("rejustify.mainUrl")

  #call API
  url       <- paste(url, "/fill", sep="")
  response  <- callCurl("POST", url, payload)

  #checking if git works well check 222ss

  #handle response
  if(response$status_code == 200) {

    response <- content(response)

    if(length(response$structure) > 0) {
      data     <- response$structure$out$data[[1]]
      keys     <- response$structure$out$keys[[1]]
      meta     <- response$structure$out$meta[[1]]
      labels   <- response$structure$out$labels[[1]]

      out <- list( 'data' = data,
                   'keys' = keys,
                   'meta' = meta,
                   'labels' = labels)

    } else {
      stop(
        paste0(
          "Unrecognized response from API."
        )
      )
    }
  } else {
    out <- list( status_code = response$status_code,
                 content     = content(response) )
  }

  return(out)
}
