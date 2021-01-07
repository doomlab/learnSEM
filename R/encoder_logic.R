#' Encoding Logic for learnr Tutorials
#'
#' This function grabs the student answers from a learnr
#' tutorial and returns them as an HTML output for
#' printing to the tutorial screen.
#'
#' @return HTML output for the student tutorial
#'
#' @keywords shiny, learnr, student answers
#' @import learnr
#' @import shiny
#' @import utils
#' @export
#' @examples
#'
#' # Be sure to put this into a server-context chunk.
#' #```{r context="server"}
#' #encoder_logic()
#' #```

encoder_logic <- function() {
  p <- parent.frame()
  check_server_context(p)

  # Evaluate in parent frame to get input, output, and session
  local({
    encoded_txt <- shiny::eventReactive(
      input$submission_generate,
      {
        #extract the objects
        objs <- learnr:::get_all_state_objects(session)
        objs <- learnr:::submissions_from_state_objects(objs)

        str(objs)

        #create a report
        report <- ""

        #loop and add to the report
        for (level1 in 1:length(objs)){

          #put in the ID
          report <- paste(report, "<br>", "<b><h3>Question ID: ", objs[[level1]]$id,
                          "</h3></b>")

          objs[[level1]]$data$code <- gsub("\n", "<br>", objs[[level1]]$data$code)
          #if code exercise
          if (objs[[level1]]$type == "exercise_submission"){

            report <- paste(report, "<br>", "<b>Code Typed: </b>",
                            objs[[level1]]$data$code)

            report <- paste(report, "<br>", "<b>Output: </b>",
                            objs[[level1]]$data$output)

          } else { #else question submission

            report <- paste(report, "<br>", "<b>Question: </b>",
                            objs[[level1]]$data$question)

            report <- paste(report, "<br>", "<b>Answer: </b>",
                            objs[[level1]]$data$answer)

          }

        }

        #return the report
        report

      }
    )

    output$submission_output <- shiny::renderUI(HTML(encoded_txt()))

  }, envir = p)
}

#' @rdname encoder_logic
#' @export
