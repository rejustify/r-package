#' changes the elements of basic blocks used by rejustify API
#'
#' @description The purpose of the function is to provide a possibly seamless
#' way of adjusting blocks used in communication with rejustify API, in particular with the
#' \code{fill} endpoint. The blocks include: data structure (\code{structure}), default values
#' (\code{default}) and matching keys (\code{kets}). Items may only be deleted for specific matching
#' dimensions proposed by \code{keys}, for the two other blocks it is possible only to change the relevant
#' values.
#'
#' Upon changes in \code{structure}, the corresponding \code{p_class} or \code{p_data} will be set to -1.
#' This is the way to inform API that the original \code{structure} has changed and, if \code{learn}
#' option is enabled, the new values will be used to train the algorithms in the back end. If \code{learn}
#' is disabled, information will not be stored by the API but the changes will be recognized in the current API call.
#'
#' @param block A data structure to be changed. Currently supported structures include \code{structure},
#' \code{default} and \code{keys}.
#' @param column The data column (or raw in case of horizontal datasets) to be adjusted. Vector values are supported.
#' @param id The identifier of the specific element to be changed. Currently it should be only used in \code{structure}
#' with multi-line headers (see \code{analyze} for details).
#' @param items Specific items to be changed with the new values to be assigned. If the values are set to \code{NA}, \code{NULL}
#' or \code{""}, the specific item will be removed from the block (only for \code{keys}). Items may be multi-valued.
#'
#' @return adjusted structure of the \code{df} data set
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
#' #adjust structures
#' st <- adjust(st, id = 2, items = list('feature' = 'country'))
#' st <- adjust(st, column = 3, items = list('provider' = 'IMF', 'table' = 'WEO'))
#'
#' #endpoint fill
#' df1 <- fill(df, st)
#'
#' #adjust default values
#' default <- adjust(df1$default, column = 3, items = list('Time Dimension' = '2013') )
#'
#' #adjust keys
#' keys <- adjust(df1$keys, column = 3, items = list('id.x' = c(3,1,2) , 'id.y' = c(1,2,3) ) )
#' keys <- adjust(df1$keys, column = 3, items = list('id.x' = 3 , 'id.y' = NA ) )
#'
#' @export

adjust = function(block, column = NULL, id = NULL, items = NULL) {

  index <- NULL
  type  <- "undefined"

  #define block type
  if( all( names(block) %in% c("id", "column", "name", "empty", "class", "feature", "cleaner", "format", "p_class", "provider", "table", "p_data") ) ) {
    type  <- "structure"
  }

  #define block type
  if( all( names(block) %in% c("column.id.x", "default") ) ) {
    type  <- "default"
  }

  #define block type
  if( (!is.null(names(block[[1]])) & all( names(block[[1]]) %in% c("id.x", "name.x", "id.y", "name.y", "class", "method", "column.id.x", "column.name.x") ) ) | is.null( names(block) ) ) {
    type  <- "keys"
  }

  #adjust structure
  if(type == "structure") {

    if( !is.null(items) & !is.null(id) ) {
      index <- block$id %in% id
    }
    if( !is.null(items) & !is.null(column) ) {
      if( is.numeric(column) ) { #if column id
        index <- block$column %in% column
      } else{
        index <- block$name %in% column
      }
    }

    tryCatch({
      block[ index, names(items) ] <- items
      if( sum( names(items) %in% c('provider', 'table') ) > 0 ) {
        block[ index, 'p_data' ] <- -1
      }
      if( sum( names(items) %in% c('class', 'feature', 'cleaner', 'format') ) > 0 ) {
        block[ index, 'p_class' ] <- -1
      }
    }, error = function(e) {
      stop(
        paste0(
          "Coulnd't change the values."
        ),
        call. = FALSE
      )
    })
  }

  #adjust default labels
  if(type == "default") {

    if( !is.null(items) & !is.null(column) ) {
      if( is.numeric(column) ) { #if column id
        index <- which( unlist(block$column.id.x) %in% column )
      } else {
        index <- which( unlist(block$column.name.x) %in% column )
      }
    }

    tryCatch({
        for(i in index) {

          if( is.null( rownames(block$default[[i]])) ) {
            rnames <- seq(1, nrow(block$default[[i]]))
          } else {
            rnames <- rownames(block$default[[i]])
          }

          block$default[[i]][ rnames %in% names(items), 'code_default']  <- unlist( items )
          block$default[[i]][ rnames %in% names(items), 'label_default'] <- NA    #blank label (will be filled by API)
        }
    }, error = function(e) {
      stop(
        paste0(
          "Coulnd't change the values."
        ),
        call. = FALSE
      )
    })

  }

  #adjust keys
  if(type == "keys") {

    id_xy     <- FALSE
    method_xy <- FALSE
    class_xy  <- FALSE

    #consistency checks
    if( !is.null( items$id.x ) & !is.null( items$id.y ) ) {
      if(length( items$id.x ) == length(items$id.y )) {
        id_xy <- TRUE
      } else {
        stop(
          paste0(
            "Item ids have different lengths."
          ) )
      }
    }
    if( !is.null( items$method ) ) {
      if(length( items$method ) == length(items$id.y )) {
        method_xy <- TRUE
      } else {
        stop(
          paste0(
            "Methods have inconsistent length."
          ) )
      }
    }
    if( !is.null( items$class ) ) {
      if(length( items$class ) == length(items$id.y )) {
        class_xy <- TRUE
      } else {
        stop(
          paste0(
            "Classes have inconsistent length."
          ) )
      }
    }

    tryCatch({
      block <- lapply(block, FUN = function(x) {
                  if( x$column.id.x == column ) {
                    if(id_xy) { #change matching ids
                     for(i in 1:length(items$id.x) ) {
                      if( sum(items$id.x[[i]] == x$id.x) > 0 ) { #if id.x is already defined, change it
                        if(isMissing(items$id.y[[i]])) {
                          x$id.y   <- x$id.y[-which( x$id.x == items$id.x[[i]] )];
                          x$name.y <- x$name.y[-which( x$id.x == items$id.x[[i]] )];
                          x$method <- x$method[-which( x$id.x == items$id.x[[i]] )];
                          x$class  <- x$class[-which( x$id.x == items$id.x[[i]] )];
                          x$name.x <- x$name.x[-which( x$id.x == items$id.x[[i]] )];
                          x$id.x   <- x$id.x[-which( x$id.x == items$id.x[[i]] )];
                        } else {
                        x$id.y[which( x$id.x == items$id.x[[i]] )]   <- items$id.y[[i]];
                        x$name.y[which( x$id.x == items$id.x[[i]] )] <- NA;
                        if(method_xy) { x$method[which( x$id.x == items$id.x[[i]] )] <- items$method[[i]] }
                        else { x$method[which( x$id.x == items$id.x[[i]] )] <- 'synonym-proximity-matching' } }
                        if(class_xy)  { x$class[which( x$id.x == items$id.x[[i]] )]  <- items$class[[i]] }
                        else { x$class[which( x$id.x == items$id.x[[i]] )] <- 'general' } }
                      else { #if id.x is not defined, add it
                        if(!isMissing(items$id.y[[i]])) {
                        x$id.x   <- c(x$id.x, items$id.x[[i]]);
                        x$id.y   <- c(x$id.y, items$id.y[[i]])
                        x$name.x <- c(x$name.x, NA);
                        x$name.y <- c(x$name.y, NA);
                        if(method_xy) { x$method <- c(x$method, items$method[[i]]) }
                        else { x$method <- c(x$method, 'synonym-proximity-matching') }
                        if(class_xy)  { x$class  <- c(x$class, items$class[[i]]) }
                        else { x$class  <- c(x$class, 'general') } } } }
                    }
                    return(x)
                  } else {
                    return(x)
                  }
                })
    }, error = function(e) {
      stop(
        paste0(
          "Coulnd't change the values."
        ),
        call. = FALSE
      )
    })

  }

  return(block)
}
