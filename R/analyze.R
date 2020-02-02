#' communicates with rejustify/analyze API endpoint
#'
#' @description The function submits the dataset to the analyze API endpoint and
#' returns the proposed structure of the data. At the current stage
#' dataset must be rectangular, either vertical or horizontal.
#'
#' API recognizes the multi-dimension and multi-line headers. The first \code{inits} rows/columns are
#' collapsed using \code{sep} character. Make sure that the separator doesn't appear in the header values.
#' It is possible to separate dimensions in single-line headers (see examples below).
#'
#' The classification algorithms are applied to the values in the rows/columns if they are not empty, and
#' to the headers if columns are empty. For efficiency reasons only a sample of values in each column is analyzed.
#' To improve the classification accuracy, you can ask the API to draw a larger sample by \code{fast=FALSE}.
#' For empty columns the API returns the proposed resources that appear to fit well into the empty space given the header
#' information and overall structure of \code{df}.
#'
#' The basic properties are characterized by classes. Currently, the API distinguishes between 6 classes: \code{general},
#' \code{geography}, \code{unit}, \code{time}, \code{sector}, \code{number}. They describe the basic characteristics of the
#' values, and are further used to propose the best transformations and matching methods for data reconciliation. Classes
#' are further supported by features, which determine these characteristics in greater detail, such as class \code{geography}
#' may be further supported by feature \code{country}.
#'
#' Cleaner contains the basic set of transformations applied to each value in a dimension to retrieve machine-readable
#' representation. For instance, values \code{y1999}, \code{y2000}, ..., clearly correspond to years, however, they will
#' be processed much faster if stripped from the initial \code{y} character, such as \code{^y}. Cleaner allows basic regular expressions.
#'
#' Finally, format corresponds to the format of the values, and it is particularly useful for time-series operations. Format allows
#' the standard date formats (see \code{?as.Date}).
#'
#' The classification algorithm can be substantially improved by allowing it to learn from the history of how
#' it was used in the past and how it performed. Parameter \code{learn} controls this feature, however, by default it
#' is disabled. The information stored by rejustify is tailored to each user individually and it can substantially
#' increase its usability. For instance, the proposed \code{provider} for empty row/column with header 'gross domestic product'
#' is \code{IMF}. Selecting another provider, for instance \code{AMECO}, will teach the algorithm that for this combination
#' of headers and rows/columns \code{AMECO} is the preferred \code{provider}, such that the next time API is called there will be
#' higher chance of \code{AMECO} to be picked by default.
#'
#' If \code{learn=TRUE}, the information stored by rejustify include (i) the information changed by the user with respect
#' to assigned \code{class}, \code{feature}, \code{cleaner} and \code{format}, (ii) resources determined by \code{provider},
#' \code{table} and headers of \code{df}, (iii) hand-picked matching values for \code{value-selection}. The information will
#' be stored only upon a change of values within groups (i-iii).
#'
#' @param df The data set to be analyzed. Must be matrix-convertible. If data frame,
#' the dimension names will be taken as the row/column names. If matrix, the row/column
#' names will be ignored, and the header will be set from matrix values in line with \code{inits}
#' and \code{sep} specification.
#' @param shape It informs the API whether the data set should be read in by
#' columns (vertical) or by rows (horizontal). The default is vertical.
#' @param inits It informs the API how many initial rows (or columns in
#' horizontal data), correspond to the header description. The default
#' is inits=1.
#' @param sep The header can also be described by single field values,
#' separated by a given character separator, for instance 'GDP, Austria, 1999'.
#' The option informs the API which separator should be used to split the
#' initial header string into corresponding dimensions. The default is ','.
#' @param fast Informs the API on how big a sample of original data should be.
#' The larger the sample, the more precise but overall slower the algorithm.
#' Under the \code{fast = TRUE} the API samples 15% of data, under the
#' \code{fast = FALSE} option it is 50\%. Default is TRUE.
#' @param learn It is TRUE if the user accepts rejustify to track her/his activity
#' to enhance the performance of the AI algorithms. The default is FALSE.
#' @param token API token. By default read from global variables.
#' @param email E-mail address for the account. By default read from global variables.
#' @param url API url. By default read from global variables.
#'
#' @return structure of the \code{df} data set
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
#' analyze(df)
#'
#' #data set with one-line multi-dimension header (semi-colon separated)
#' df <- data.frame(country = c("Poland", "Poland", "Poland"),
#'                  `gross domestic product;2009` = c(NA, NA, NA),
#'                  `gross domestic product;2010` = c(NA, NA, NA),
#'                   check.names = FALSE, stringsAsFactors = FALSE)
#' analyze(df, sep = ";")
#'
#' #data set with multi-line header
#' df <- cbind(c(NA, "country", "Poland", "Poland", "Poland"),
#'             c("gross domestic product", "2009", NA, NA, NA),
#'             c("gross domestic product", "2010", NA, NA, NA))
#' analyze(df, inits = 2)
#'
#' @importFrom httr content
#' @importFrom jsonlite toJSON
#' @export

analyze = function( df,
                    shape = "vertical",
                    inits = 1,
                    fast  = TRUE,
                    sep   = ",",
                    learn = FALSE,
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
  if( typeof(learn) != 'logical'  ) {
    stop(
      paste0(
        "Unrecognized learn value. It should be TRUE/FALSE."
      ),
      call. = FALSE
    )
  }

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
    names <- colnames(df)
  } else {
    names <- rownames(df)
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
        "The shape of the data set is currently not supported."
      ),
      call. = FALSE
    )
  })

  #set column/row headers
  if( is.null( names )  & ss == 1) {
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
  if(ss==1) {
    if(shape == "vertical") {
      df <- rbind( names, df)
      rownames(df) <- NULL
    } else {
      df <- cbind( names, df)
      colnames(df) <- NULL
    }
  }

  ###########
  #API call
  ###########

  #prepare the payload query
  payload  <- toJSON( list(data = df,
                           userToken   = token,
                           dataShape   = shape,
                           inits       = inits,
                           fast        = fast,
                           email       = email,
                           sep         = sep,
                           dbAllowed   = learn), na = 'null', auto_unbox = T)

  #call API
  url       <- paste(url, "/analyze", sep="")
  response  <- callCurl("POST", url, payload)

  #handle response
  if(response$status_code == 200) {
    structure <- content(response)
    index     <- which( unlist( lapply( structure$message, FUN = function(line) { ifelse(line$code==200, 0, 1) } ) ) == 1 )

    #display API warnings/errors
    if(length(index != 0)) {
      for(i in index) {
        warning(paste(structure$message[i][[1]]$text[[1]], "Code:", structure$message[i][[1]]$code[[1]]))
      }
    }

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
