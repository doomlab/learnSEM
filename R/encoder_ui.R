#' Encoding User Interface for learnr Tutorials
#'
#' This function is the shiny user interface for
#' creating the submission output. You can
#' define instructions to go before or after the
#' submission window!
#'
#' @param ui_before Shiny code to go before your
#' submission box.
#' @param ui_after Shiny code to go after your
#' submission box.
#' @return Shiny interface for creating submissions
#' for the learnr tutorials.
#'
#' @keywords shiny, learnr, student answers
#' @import shiny
#' @export
#' @examples
#'
#' #```{r encode, echo=FALSE}
#' #encoder_ui()
#' #```

encoder_ui <- function(ui_before = NULL, ui_after = NULL) {
  check_not_server_context(parent.frame())

  shiny::tags$div(
    ui_before,
    shiny::fixedRow(
      shiny::column(
        width = 3,
        shiny::actionButton("submission_generate", "Generate Submission")
      ),
      shiny::column(width = 7),
      shiny::column(
        width = 2
      )
    ),
    shiny::tags$br(),
    htmlOutput("submission_output"),
    shiny::tags$br(),
    ui_after
  )
}

#' @rdname encoder_ui
#' @export
