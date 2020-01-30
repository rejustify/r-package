#' \code{fill} submits the dataset and structure to the fill API endpoint
#' to retrieve the desired extra data points. At the current stage
#' dataset must be rectangular, and structure should be in the shape proposed
#' analyze function.
#'
#' @examples
#' fill(df, structure, modify)
#'
fill = function( df        = NULL,
                 structure = NULL,
                 keys      = NULL,
                 default   = NULL,
                 shape = "vertical",
                 inits = 1,
                 sep   = ",",
                 db    = FALSE,
                 accu  = 0.75,
                 form  = 'full',
                 token = getOption("rejustify.token"),
                 email = getOption("rejustify.email"),
                 url   = getOption("rejustify.mainUrl") ) {

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
  payload <- toJSON( list(structure   = structure,
                           data        = df,
                           keys        = keys,
                           meta        = default,
                           userToken   = token,
                           email       = email,
                           dataForm    = form,
                           dbAllowed   = db,
                           minAccuracy = accu,
                           sep         = sep,
                           direction   = shape,
                           inits       = inits), na = 'null')

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
        colnames(data) <- names
      } else {
        data <- t(data)
        colnames(data) <- names
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
        keys <- data.frame( matrix( unlist( response$out$keys ), nrow=length(response$out$keys), byrow=T), stringsAsFactors = F, check.names = F)
        colnames(keys) <- c("id.x",
                            "name.x",
                            "id.y",
                            "name.y",
                            "class",
                            "variation.x",
                            "method",
                            "column.id.x",
                            "column.name.x")
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
