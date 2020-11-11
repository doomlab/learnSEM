#' Server Functions for learnr Tutorials
#'
#' These functions help check that you have put
#' together the tutorial correctly for the
#' student answers to print out at the end of the
#' tutorial
#'
#' @param .envir Automatically grabs the environment
#' variable for your shiny session.
#'
#' @return Error messages if you incorrectly use
#' the functions.
#'
#' @keywords shiny, learnr, student answers
#' @export

is_server_context <- function(.envir) {
  # We are in the server context if there are the follow:
  # * input - input reactive values
  # * output - shiny output
  # * session - shiny session
  #
  # Check context by examining the class of each of these.
  # If any is missing then it will be a NULL which will fail.

  inherits(.envir$input,   "reactivevalues") &
    inherits(.envir$output,  "shinyoutput")    &
    inherits(.envir$session, "ShinySession")
}

check_not_server_context = function(.envir) {
  if (is_server_context(.envir)) {
    calling_func <- deparse(sys.calls()[[sys.nframe()-1]])

    err = paste0(
      "Function `", calling_func,"`",
      " must *not* be called from an Rmd chunk where `context = \"server\"`"
    )

    # The following seems to be necessary - since this is in the server context
    # it will not run at compile time
    shiny::stopApp()

    stop(err, call. = FALSE)
  }
}

check_server_context <- function(.envir) {
  if (!is_server_context(.envir)) {
    calling_func <- deparse(sys.calls()[[sys.nframe()-1]])

    err = paste0(
      "Function `", calling_func,"`",
      " must be called from an Rmd chunk where `context = \"server\"`"
    )

    stop(err, call. = FALSE)
  }
}

#' @rdname server_context
#' @export
