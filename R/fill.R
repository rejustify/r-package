#' communicates with rejustify/fill API endpoint
#'
#' @description The function submits the request to the API fill endpoint
#' to retrieve the desired extra data points. At the current stage
#' dataset must be rectangular, and structure should be in the shape proposed
#' analyze function. The minimum required by the endpoint is the data set and
#' the corresponding \code{structure}. You can browse the available resources at
#' \code{https://rejustify.com/repos}). Other features, including private
#' resources and models, are taken as defined for the account.
#'
#' The API defines the submitted data set as \code{x} and any server-side data set as \code{y}.
#' The corresponding structures are marked with the same principle, as \code{structure.x} and
#' \code{structure.y}, for instance. The principle rule of any data manipulation is to never change
#' data \code{x} (except for missing values), but only adjust \code{y}.
#'
#' @param df The data set to be analyzed. Must be matrix-convertible. If data frame,
#' the dimension names will be taken as the row/column names. If matrix, the row/column
#' names will be ignored, and the header will be set from matrix values in line with \code{inits}
#' and \code{sep} specification.
#' @param structure Structure of the \code{x} data set, characterizing classes, features, cleaners and formats
#' of the columns/rows, and data provider/tables for empty columns. Perfectly, it should come from \code{analyze}
#' endpoint.
#' @param default Default values used to lock dimensions in data \code{y} which will be not used for matching against
#' data \code{x}. Each empty column to be filled, characterized by \code{default$column.id.x}, must contain description of
#' the default values. If missing, the API will propose the default values in line with the history of how it was
#' used in the past.
#' @param keys The matching keys and matching methods between dimensions in \code{x} and {y} data sets. The elements in
#' \code{keys} are determined based on information provided in data \code{x} and \code{y}, for each empty column. The details
#' behind both data structures can be visualized by \code{structure.x} and \code{structure.y}.
#'
#' Matching keys are given consecutively, i.e. the first element in \code{id.x} and \code{name.x} corresponds
#' to the first element in \code{id.y} and \code{name.y}, and so on. Dimension names are given for better readability of
#' the results, however, they are not necessary for API recognition. \code{keys} return also data classification in
#' element \code{class} and the proposed matching method for each part of \code{id.x} and \code{id.y}.
#'
#' Currently, API suports 6 matching methods: \code{synonym-proximity-matching}, \code{synonym-matching}, \code{proximity-matching}, \code{time-matching},
#' \code{exact-matching} and \code{value-selection}, which are given in a diminishing order of complexitiy. \code{synonym-proximity-matching}
#' uses the proximity between the values in data \code{x} and \code{y} to the coresponding values in rejustify dictionary. If the proximity
#' is above threshold \code{accu} and there are values in \code{x} and \code{y} pointing to the same element in the dictionary, the values will
#' be matched. \code{synonym-matching} and \code{proximity-matching} use similar logic of either of the steps described for
#' \code{synonym-proximity-matching}. \code{time-matching} aims at standardizing the time values to the same format before matching. For proper
#' functioning it requires an accurate characterization of date format in \code{structure.x} (\code{structure.y} is already classified by rejustify).
#' \code{exact-matching} will match two values only if they are identical. \code{value-selection} is a quasi matching method which for single-valued
#' dimension \code{x} will return single value from \code{y}, as suggested by \code{default} specification. It is the most efficient
#' matching method for dimensions which do not show any variability.
#' @param shape It informs the API whether the data set should be read by
#' columns (\code{vertical}) or by rows (\code{horizontal}). The default is \code{vertical}.
#' @param inits It informs the API how many initial rows (or columns in
#' horizontal data), correspond to the header description. The default
#' is \code{inits=1}.
#' @param sep The header can also be described by single field values,
#' separated by a given character separator, for instance 'GDP, Austria, 1999'.
#' The option informs the API which separator should be used to split the
#' initial header string into corresponding dimensions. The default is \code{sep=','}.
#' @param learn It is \code{TRUE} if the user accepts rejustify to track her/his activity
#' to enhance the performance of the AI algorithms (it is not enabled by default). To change this option
#' for all API calls run \code{setCurl(learn=TRUE)}.
#' @param accu Acceptable accuracy level on a scale from 0 to 1. It is used in the matching algorithms
#' to determine string similarity. The default is \code{accu=0.75}.
#' @param form Requests the data to be returned either in \code{full}, or \code{partial} shape.
#' The former returns the original data with filled empty columns. The latter returns only the filled columns.
#' @param token API token. By default read from global variables.
#' @param email E-mail address for the account. By default read from global variables.
#' @param url API url. By default read from global variables.
#'
#' @return list consisting of 5 elements: \code{data}, \code{structure.x}, \code{structure.y}, \code{keys} and \code{default}
#'
#' @examples
#' #API setup
#' setCurl()
#'
#' #register token/email
#' register(token = "YOUR_TOKEN", email = "YOUR_EMAIL")
#'
#' #sample data set
#' df <- data.frame(year = c("2009", "2010", "2011"),
#'                  country = c("Poland", "Poland", "Poland"),
#'                  `gross domestic product` = c(NA, NA, NA),
#'                  check.names = FALSE, stringsAsFactors = FALSE)
#'
#' #endpoint analyze
#' st <- analyze(df)
#'
#' #endpoint fill
#' df1 <- fill(df, st)
#'
#' @importFrom httr content
#' @importFrom jsonlite toJSON
#' @export

fill = function( df,
                 structure,
                 keys      = NULL,
                 default   = NULL,
                 shape = "vertical",
                 inits = 1,
                 sep   = ",",
                 learn = getOption("rejustify.learn"),
                 accu  = 0.75,
                 form  = 'full',
                 token = getOption("rejustify.token"),
                 email = getOption("rejustify.email"),
                 url   = getOption("rejustify.mainUrl") ) {

  ###########
  #data consistency checks
  ###########

  #learning check
  if( typeof(learn) != 'logical'  ) {
    stop(
      paste0(
        "Unrecognized learn value. It should be TRUE/FALSE."
      ),
      call. = FALSE
    )
  }

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

  #check sep value
  if( nchar(sep) > 3  ) {
    stop(
      paste0(
        "Unrecognized separator value. It should be not longer than 3 characters."
      ),
      call. = FALSE
    )
  }

  #check learn value
  if( typeof(learn) != 'logical'  ) {
    stop(
      paste0(
        "Unrecognized learn value. It should be TRUE/FALSE."
      ),
      call. = FALSE
    )
  }

  #check accu value
  if( typeof(accu) != 'double'  ) {
    stop(
      paste0(
        "Unrecognized accuracy value. It should be double."
      ),
      call. = FALSE
    )
  }

  #check shape value
  if( !(form == "full" | shape == "reduced") ) {
    stop(
      paste0(
        "Unrecognized form value. It should be full/reduced"
      ),
      call. = FALSE
    )
  }

  ###########
  #structure preparation
  ###########

  tryCatch({
    structure <- as.data.frame(structure, stringsAsFactor = F)
    #structure[is.na( structure )] <- "NA" #correct for missing values
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

  #set shape dummy if data frame
  ss <- 0
  if(is.data.frame(df)) {
    ss <- 1
  }

  #adjust the data format
  if(shape == "vertical") {
    if(ss == 1) {
      names <- colnames(df)
    } else {
      names <- df[1:inits,]
    }
  } else {
    if(ss == 1) {
      names <- rownames(df)
    } else {
      names <- df[,1:inits]
    }
  }

  #verify the data shape
  tryCatch({
    if(!is.matrix(df)) {
      df <- as.matrix( df )
    }
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
      names <- paste( "_column", seq(1: ncol(df)) )
    } else {
      names <- paste( "_row", seq(1: nrow(df)) )
    }
    warning(
      paste0(
        "The dataset doesn't have column/row names."
      ))
  }

  #create dataset
  if(ss == 1) {
    if(shape == "vertical") {
        df <- rbind( names, df)
    } else {
      df <- cbind( names, df)
    }
  }

  ###########
  #API call
  ###########

  #prepare the payload query
  payload <- toJSON( list(structure    = structure,
                           data        = df,
                           keys        = keys,
                           meta        = default,
                           userToken   = token,
                           email       = email,
                           dataForm    = form,
                           dbAllowed   = as.integer(learn),
                           minAccuracy = accu,
                           sep         = sep,
                           direction   = shape,
                           inits       = inits), na = 'null', auto_unbox = T)

  #call API
  url       <- paste(url, "/fill", sep="")
  response  <- callCurl("POST", url, payload)

  #handle response
  if(response$status_code == 200) {

    response <- content(response)

    #consistency check, depdning on API version
    if( !is.null( response$structure ) ) {
      response <- response$structure
    }

    if(length(response$out) > 0) {

      #get data values
      data <- data.frame( matrix( unlist( response$out$data ), nrow=length(response$out$data), byrow=T), stringsAsFactors = F, check.names = F)
      if(shape == "vertical") {
        if(ss == 1) {
          colnames(data) <- names
        } else {
          data <- rbind(names, as.matrix( data ) )
        }
      } else {
        if(ss == 1) {
          rownames(data) <- names
        } else {
          data <- cbind(names, as.matrix( data ) )
        }
      }

      #get structure.x
      structure.x <- data.frame( id       = integer(),
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

      for( i in 1:length(response$out$structure) ) {
        x <- response$out$structure[[i]]
        structure.x[i,"id"]       <- ifelse( isMissing(x[["id"]])      , NA, x[["id"]] );
        structure.x[i,"column"]   <- ifelse( isMissing(x[["column"]])  , NA, x[["column"]] );
        structure.x[i,"name"]     <- ifelse( isMissing(x[["name"]])    , NA, x[["name"]] );
        structure.x[i,"empty"]    <- ifelse( isMissing(x[["empty"]])   , NA, x[["empty"]] );
        structure.x[i,"class"]    <- ifelse( isMissing(x[["class"]])   , NA, x[["class"]] );
        structure.x[i,"feature"]  <- ifelse( isMissing(x[["feature"]]) , NA, x[["feature"]] );
        structure.x[i,"cleaner"]  <- ifelse( isMissing(x[["cleaner"]]) , NA, x[["cleaner"]] );
        structure.x[i,"format"]   <- ifelse( isMissing(x[["format"]])  , NA, x[["format"]] );
        structure.x[i,"p_class"]  <- ifelse( isMissing(x[["p_class"]]) , NA, x[["p_class"]] );
        structure.x[i,"provider"] <- ifelse( isMissing(x[["provider"]]), NA, x[["provider"]] )
        structure.x[i,"table"]    <- ifelse( isMissing(x[["table"]])   , NA, x[["table"]] )
        structure.x[i,"p_data"]   <- ifelse( isMissing(x[["p_data"]])  , NA, x[["p_data"]] )
      }

      #get structure.y
      out.structure.y <- list()
      for( j in 1:length(response$out$meta) ) {
        structure.y <- data.frame( id       = integer(),
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

        for( i in 1:length(response$out$meta[[j]]) ) {
          x <- response$out$meta[[j]][[i]]
          structure.y[i,"id"]       <- ifelse( isMissing(x[["id"]])      , NA, x[["id"]] );
          structure.y[i,"column"]   <- ifelse( isMissing(x[["column"]])  , NA, x[["column"]] );
          structure.y[i,"name"]     <- ifelse( isMissing(x[["name"]])    , NA, x[["name"]] );
          structure.y[i,"empty"]    <- ifelse( isMissing(x[["empty"]])   , NA, x[["empty"]] );
          structure.y[i,"class"]    <- ifelse( isMissing(x[["class"]])   , NA, x[["class"]] );
          structure.y[i,"feature"]  <- ifelse( isMissing(x[["feature"]]) , NA, x[["feature"]] );
          structure.y[i,"cleaner"]  <- ifelse( isMissing(x[["cleaner"]]) , NA, x[["cleaner"]] );
          structure.y[i,"format"]   <- ifelse( isMissing(x[["format"]])  , NA, x[["format"]] );
          structure.y[i,"p_class"]  <- ifelse( isMissing(x[["p_class"]]) , NA, x[["p_class"]] );
          structure.y[i,"provider"] <- ifelse( isMissing(x[["provider"]]), NA, x[["provider"]] )
          structure.y[i,"table"]    <- ifelse( isMissing(x[["table"]])   , NA, x[["table"]] )
          structure.y[i,"p_data"]   <- ifelse( isMissing(x[["p_data"]])  , NA, x[["p_data"]] )
        }
        out.structure.y[[j]] <- structure.y
      }

      #get keys
      if(!isMissing(response$out$keys)) {
        keys <- lapply(response$out$keys, FUN = function(x) {
                  lapply(x, FUN = function(y) { unlist(y) } ) } )
      }

      #get default labels
      if(!isMissing(response$out$labels)) {
        default <- list()
        for(i in 1:length(response$out$labels) ) {
          response$out$labels[[i]] <- lapply(response$out$labels[[i]], FUN = function(x) {
                                    lapply(x, FUN = function(y) {
                                      ifelse(isMissing(y[[1]]), NA, y[[1]])
                                    })
                                  })

          default[[i]] <- data.frame( matrix( unlist( response$out$labels[[i]] ), nrow=length(response$out$labels[[i]]), byrow=T), stringsAsFactors = F, check.names = F)
          colnames(default[[i]]) <- c("code_default", "label_default")
          rownames(default[[i]]) <- names(response$out$labels[[i]])
        }
      }

      #output
      out <- list( 'data'        = data,
                   'structure.x' = structure.x,
                   'structure.y' = list(column.id.x = list( unlist( response$out$column ) ), structure.y = out.structure.y),
                   'keys'        = keys,
                   'default'     = list(column.id.x = list( unlist( response$out$column ) ), default = default) )

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
