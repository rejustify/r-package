adjust = function(block, id = NULL, column = NULL, items = NULL) {

  index <- NULL
  type  <- "undefined"

  #define block type
  if( all( names(block) %in% c("id", "column", "name", "empty", "class", "feature", "cleaner", "format", "p_class", "provider", "table", "p_data") ) ) {
    type  <- "structure"
  }

  #adjust
  if(type == "structure") {

    #define elements to adjust
    if( !is.null(items) & !is.null(id) & is.null(column) ) {
      index <- block$id %in% column
    }
    if( !is.null(items) & !is.null(column) ) {
      if( is.numeric(column) ) { #if column id
        index <- block$column %in% column
      } else{
        index <- block$name %in% column
      }
    }
    #adjust
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

  return(block)
}
