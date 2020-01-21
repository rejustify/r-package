#' \code{analyze} submits the dataset to the analyze API endpoint and
#' returns the proposed structure of the data. At the current stage
#' dataset must be rectangular.
#'
#' @param df The dataset to be analyzed. Must be matrix-convertible.
#' @param shape It informs the API whether the data set should be read in by
#' columns (vertical) or by rows (horizontal). The default is vertical.
#' @param inits It informs the API how many initial rows (or columns in
#' horizontal data), correspond to the header description. The default
#' is inits=1.
#' @param fast Informs the API on how big a sample of original data should be.
#' The larger the sample, the more precise but overall slower the algorithm.
#' Under the \code{fast = TRUE} the API samples 15% of data, under the
#' \code{fast = FALSE} option it is 50%. Default is TRUE.
#' @param sep The header can also be described by single field values,
#' separated by a given character separator, for instance 'GDP, Austria, 1999'.
#' The option informs the API which separator should be used to split the
#' initial header string into corresponding dimensions. The default is ','.
#' @param db It is TRUE if the user accepts rejustify to track her/his activity
#' to enhance the performance of the AI algorithms. The default is FALSE.
#' @param token API token. By default read from global variables.
#' @param email E-mail address for the account. By default read from global variables.
#' @param url API url. By default read from global variables.
#' @return rejustify API response or errors
#' @examples
#' analyze(df)
#'
analyze = function( df    = NULL,
                    shape = "vertical",
                    inits = 1,
                    fast  = TRUE,
                    sep   = ",",
                    db    = FALSE,
                    token = getOption("rejustify.token"),
                    email = getOption("rejustify.email"),
                    url   = getOption("rejustify.mainUrl") ) {

  ###########
  #data consistency checks
  ###########

  #check shape value
  if( !(shape == "vertical" | shape == "horizontal") ) {
    stop(
      paste0(
        "Unrecognized shape value. It should be vertical/horizontal."
      ),
      call. = FALSE
    )
  }

  #check the inits value
  tryCatch({
    inits <- floor(inits)
  }, error = function(e) {
    stop(
      paste0(
        "Unrecognized inits value. It should be integer."
      ),
      call. = FALSE
    )
  })

  #check fast value
  if( typeof(fast) != 'logical'  ) {
    stop(
      paste0(
        "Unrecognized fast value. It should be TRUE/FALSE."
      ),
      call. = FALSE
    )
  }

  #check fast value
  if( nchar(sep) > 3  ) {
    stop(
      paste0(
        "Unrecognized separator value. It should be not longer than 3 characters."
      ),
      call. = FALSE
    )
  }

  #check fast value
  if( typeof(db) != 'logical'  ) {
    stop(
      paste0(
        "Unrecognized db value. It should be TRUE/FALSE."
      ),
      call. = FALSE
    )
  }

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
  payload  <- toJSON( list(data        = df,
                   userToken   = token,
                   dataShape   = shape,
                   inits       = inits,
                   fast        = fast,
                   email       = email,
                   sep         = sep,
                   dbAllowed   = db), na = 'null')

  #call API
  url       <- paste(url, "/analyze", sep="")
  response  <- callCurl("POST", url, payload)

  #handle response
  if(response$status_code == 200) {
    structure <- content(response)

    #process the output
    out <- data.frame( id       = integer(),
                       column   = integer(),
                       name     = character(),
                       empty    = integer(),
                       class    = character(),
                       feature  = character(),
                       cleaner  = character(),
                       format   = character(),
                       p_class  = double(),
                       provider = character(),
                       table    = character(),
                       p_data   = double(), stringsAsFactors=FALSE)

    if(length(structure$structure) > 0) {
      for( i in 1:length(structure$structure) ) {
        x <- structure$structure[[i]]

        #get structure
        out[i,"id"]       <- ifelse( isMissing(x[["id"]])      , NA, x[["id"]] )
        out[i,"column"]   <- ifelse( isMissing(x[["column"]])  , NA, x[["column"]] )
        out[i,"name"]     <- ifelse( isMissing(x[["name"]])    , NA, x[["name"]] )
        out[i,"empty"]    <- ifelse( isMissing(x[["empty"]])   , NA, x[["empty"]] )
        out[i,"class"]    <- ifelse( isMissing(x[["class"]])   , NA, x[["class"]] )
        out[i,"feature"]  <- ifelse( isMissing(x[["feature"]]) , NA, x[["feature"]] )
        out[i,"cleaner"]  <- ifelse( isMissing(x[["cleaner"]]) , NA, x[["cleaner"]] )
        out[i,"format"]   <- ifelse( isMissing(x[["format"]])  , NA, x[["format"]] )
        out[i,"p_class"]  <- ifelse( isMissing(x[["p_class"]]) , NA, x[["p_class"]] )
        out[i,"provider"] <- ifelse( isMissing(x[["provider"]]), NA, x[["provider"]] )
        out[i,"table"]    <- ifelse( isMissing(x[["table"]])   , NA, x[["table"]] )
        out[i,"p_data"]   <- ifelse( isMissing(x[["p_data"]])  , NA, x[["p_data"]] )
      }
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

  return( out )
}
