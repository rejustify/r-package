#' \code{register} sets the token and email as global variables
#' to be accessed by rejustify package. The email must correspond to
#' the token that was assigned to it.
#'
#' @param token API token.
#' @param email E-mail address for the account.
#' @return errors only
#' @examples
#' register(token = "your_token", email = "your_email")
#'
#' @export
register = function(token = NULL, email = NULL) {
  if( is.null(token) | is.null(email) ) {
    stop(
      paste0(
        "Please provide valid token and e-mail values."
      ),
      call. = FALSE
    )
  } else {
    tryCatch({
      options(rejustify.token = token)
      options(rejustify.email = email)
    }, error = function(e) {
      stop(
        paste0(
          "Coulnd't register. Check your R environment."
        ),
        call. = FALSE
      )
    } )
  }
}
