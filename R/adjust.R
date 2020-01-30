adjust = function(block, id = NULL, column = NULL, items = NULL) {

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
  if( all( names(block) %in% c("id.x", "name.x", "id.y", "name.y", "class", "variation.x", "method", "column.id.x", "column.name.x") ) | is.null( names(block) ) ) {
    type  <- "keys"
  }

  #adjust structure
  if(type == "structure") {

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

          block$default[[i]][ rnames %in% names(items), 'code_default']  <- items
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

    ids_xy      <- FALSE
    names_xy    <- FALSE
    classes_xy  <- FALSE
    var_xy      <- FALSE

    #consistency checks
    if(names(items) %in% c('id.x', 'id.y') ) {
      ids_xy <- TRUE
      items[names(items)] <- lapply( items[names(items)], paste, collapse = "," )
    }
    if(names(items) %in% c('name.x', 'name.y') ) {
      names_xy <- TRUE
      items[names(items)] <- lapply( items[names(items)], paste, collapse = "," )
    }
    if(names(items) %in% c('class') ) {
      classes_xy <- TRUE
      items[names(items)] <- lapply( items[names(items)], paste, collapse = "," )
    }

    if(!ids_xy & !names_xy) {
      stop(
        paste0(
          "The items you want to change do not have ids and names."
        ),
        call. = FALSE
      )
    }

    if(!classes_xy) {
      stop(
        paste0(
          "The items you want to change do not have defined classes."
        ),
        call. = FALSE
      )
    }

    #assignment of values
    if( !is.null(items) & !is.null(column) ) {
      if( is.numeric(column) ) { #if column id
        index <- block$column.id.x %in% column
      } else{
        index <- block$column.name.x %in% column
      }
    }

    tryCatch({
      block[ index, names(items) ] <- items
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
