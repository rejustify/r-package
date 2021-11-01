#' visualizes the matching/filling procedures
#'
#' @description The function aims at simplyfying.
#'
#' @param object Object returned by the \code{fill} function.
#' @param column The empty column for which we want to visualize the matching/filling procedures. By default the
#' first empty column is taken.
#' @param details Displays \code{id}, \code{column}, \code{class} and \code{feature} elements in the table. They are
#' not displayed by default.
#'
#' @return list containing the pairs of value code and label
#'
#' @examples
#' \dontrun{
#'  #rdf is an object returned by fill function
#'  vis(rdf)
#'  vis(rdf, details = TRUE)
#' }
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics legend par plot mtext text legend rect
#' @export

vis = function( object , column = NULL, details = FALSE ) {

  ###########
  # consistency checks
  ###########

  if( length(object) < 4 ) {
    stop(
      paste0(
        "Incorrect object element."
      ),
      call. = FALSE
    )
  }

  if( is.null(object$structure.x) | is.null(object$structure.y) | is.null(object$keys) | is.null(object$default) ) {
    stop(
      paste0(
        "Incorrect object element."
      ),
      call. = FALSE
    )
  }

  if( typeof(details) != 'logical'  ) {
    stop(
      paste0(
        "Unrecognized details value. It should be TRUE/FALSE."
      ),
      call. = FALSE
    )
  }

  #set column if not set
  if(is.null(column)) column <- object$keys[[1]]$column.id.x

  #check if exists
  if(!(column %in% unlist( lapply( object$keys, FUN = function(x) { return(x$column.id.x) } ) ) ) ) {
    stop(
      paste0(
        "Incorrect column element. Check if the matching/filling is carried out for this column."
      ),
      call. = FALSE
    )
  }
  column_name <- unlist( lapply( object$keys, FUN = function(x) { if(x$column.id == column) { return(x$column.name.x) } } ) )

  #default values
  ii      <- which( unlist( object$default$column.id.x ) == column )
  default <- object$default$default[[ii]]

  #determine number of elements
  n <- max(nrow(object$structure.x), nrow(object$structure.y$structure.y[[ii]]))

  #matching colors
  colors <- rainbow(length(object$keys[[ii]]$id.x), start = 0.7, end = 0.1)

  #dimensions
  h <- 50
  w <- 150
  topspace    <- 20
  bottomspace <- 20 + 20 * ceiling(length(colors)/3)

  #setup the drawing region
  op <- par(bg = "white", mar = c( 1, 1, 1, 1 ) )
  plot(c(0, 2*w + 30), c(0, n*h + topspace + bottomspace), xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
  #plot(c(0, 2*w + 30), c(0, n*h + topspace + bottomspace), pch = '', ylab = '', xlab = '')

  #labels
  provider <- object$structure.x$provider[[column]]
  table    <- object$structure.x$table[[column]]

  mtext( paste('Column',column_name) )
  text((2*w + 30)/2, n*h+bottomspace+topspace, labels = paste0("Provider ", provider, " | Table ", table ) )

  text(10+w/2, n*h+bottomspace, labels = 'Structure X')
  text(10+w+10+w/2, n*h+bottomspace, labels = 'Structure Y')

  #legend
  legend('bottom', legend=object$keys[[ii]]$method, fill=colors,  bty = 'n', xjust = 0.5, ncol = 3, cex = 0.85)

  #classes
  mm      <- max(nchar(object$structure.x$class))
  class   <- lapply(object$structure.x$class, FUN = function(x) { paste0(x, paste0( rep(" ", 2*(mm - nchar(x))), collapse = "") )  } )
  object$structure.x$feature[is.na(object$structure.x$feature)] <- ""
  mm      <- max(nchar(object$structure.x$feature))
  feature <-  lapply(object$structure.x$feature, FUN = function(x) { paste0(paste0( rep(" ", 2*(mm - nchar(x))), collapse = ""), x )  } )

  #draw x box
  space <- topspace
  for(i in 1:nrow(object$structure.x)) {

    #contour
    index <- which(object$keys[[ii]]$id.x == object$structure.x$id[[i]])
    if(length(index) > 0) {
      color <- colors[[index]]
    } else {
      color <- "#FFFFFF"
    }
    rect(10, (n-i+1)*h - space + bottomspace, 10+w, (n-i)*h - space + bottomspace, col = color)

    #id and column number
    if(details) text(10+15, (n-i+1)*h - space + bottomspace - 5, labels = paste0("ID:",object$structure.x$id[[i]]))
    if(details) text(10+w-10, (n-i+1)*h - space + bottomspace - 5, labels = paste0("C:",object$structure.x$column[[i]]))

    #text
    dim_name <- object$structure.x$name[[i]]
    if(nchar(dim_name) > 15) dim_name <- paste0(substr(dim_name, 1, 15),"...")
    text(10+w/2, (n-i+0.5)*h - space + bottomspace, labels = dim_name)

    #class & feature
    if(details) text(10 + 35, (n-i)*h - space + bottomspace + 5, labels = class[[i]])
    if(details) text(10 + w - 25, (n-i)*h - space + bottomspace + 5, labels = feature[[i]])

    space <- space + 0
  }

  #classes
  mm      <- max(nchar(object$structure.y$structure.y[[ii]]$class))
  class   <- lapply(object$structure.y$structure.y[[ii]]$class, FUN = function(x) { paste0(x, paste0( rep(" ", 2*(mm - nchar(x))), collapse = "") )  } )
  object$structure.y$structure.y[[ii]]$feature[is.na(object$structure.y$structure.y[[ii]]$feature)] <- ""
  mm      <- max(nchar(object$structure.y$structure.y[[ii]]$feature))
  feature <-  lapply(object$structure.y$structure.y[[ii]]$feature, FUN = function(x) { paste0(paste0( rep(" ", 2*(mm - nchar(x))), collapse = ""), x )  } )

  #draw y box
  space <- topspace
  for(i in 1:nrow(object$structure.y$structure.y[[ii]])) {

    #contour
    index <- which(object$keys[[ii]]$id.y == object$structure.y$structure.y[[ii]]$id[[i]])
    if(length(index) > 0) {
      color <- colors[[index]]
      method        <- object$keys[[ii]]$method[[index]]
    } else {
      color         <- "#FFFFFF"
      method        <- "none"
    }
    if(method == "value-selection" | method == "none") {
      default_code  <- default$code_default[[i]]
      default_label <- default$label_default[[i]]
    } else {
      default_code  <- NA
      default_label <- NA
    }

    #border
    bb <- 1
    if(object$structure.y$structure.y[[ii]]$name[[i]] == "Primary Measure") bb <- 2

    rect(10+w+10, (n-i+1)*h - space + bottomspace, 10+w+10+w, (n-i)*h - space + bottomspace, col = color, lty = bb)

    #id and column number
    if(details) text(10+w+10+15, (n-i+1)*h - space + bottomspace - 5, labels = paste0("ID:",object$structure.y$structure.y[[ii]]$id[[i]]))
    if(details) text(10+w+10+w-10, (n-i+1)*h - space + bottomspace - 5, labels = paste0("C:",object$structure.y$structure.y[[ii]]$column[[i]]))

    #text
    dim_name =  object$structure.y$structure.y[[ii]]$name[[i]]
    if(nchar(dim_name) > 15) dim_name <- paste0(substr(dim_name, 1, 15),"...")
    text(10+w+10+w/2, (n-i+0.5)*h - space + bottomspace, labels = dim_name)

    #class & feature
    if(details) text(10+w+10+35, (n-i)*h - space + bottomspace + 5, labels = class[[i]])
    if(details) text(10+w+10+w-25, (n-i)*h - space + bottomspace + 5, labels = feature[[i]])

    #label if not matched or matching by value-selection
    if(method == "value-selection" | (method == "none" & !is.na(default_code))) default_code <- paste0("=",default_code)

    text(10+w+10+w/2, (n-i+0.5)*h - space + bottomspace-10, labels = default_code)

    space = space + 0
  }

  #text output
  cat( paste0("Displaying matching procedures for column ", column, ": ", column_name, "\n") )
  cat( paste0("Default matching values: \n") )
  for(i in 1:nrow(object$structure.y$structure.y[[ii]])) {
    index <- which(object$keys[[ii]]$id.y == object$structure.y$structure.y[[ii]]$id[[i]])
    if(length(index) > 0) {
      method        <- object$keys[[ii]]$method[[index]]
    } else {
      method        <- "none"
    }
    if(method == "value-selection" | method == "none") {
      default_code  <- default$code_default[[i]]
      default_label <- default$label_default[[i]]
    } else {
      default_code  <- NA
      default_label <- NA
    }
    #label if not matched or matching by value-selection
    if(method == "value-selection" | (method == "none" & !is.na(default_code))) {
      cat( paste0(default_code, " = ", default_label, "\n")  )
    }
  }

  #to browse all labels
  cat( paste0("To browse all labels: ",  "labels('",provider,"','",table,"')", "\n")  )

}
