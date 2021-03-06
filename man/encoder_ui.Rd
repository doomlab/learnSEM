% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encoder_ui.R
\name{encoder_ui}
\alias{encoder_ui}
\title{Encoding User Interface for learnr Tutorials}
\usage{
encoder_ui(ui_before = NULL, ui_after = NULL)
}
\arguments{
\item{ui_before}{Shiny code to go before your
submission box.}

\item{ui_after}{Shiny code to go after your
submission box.}
}
\value{
Shiny interface for creating submissions
for the learnr tutorials.
}
\description{
This function is the shiny user interface for
creating the submission output. You can
define instructions to go before or after the
submission window!
}
\examples{

#```{r encode, echo=FALSE}
#encoder_ui()
#```
}
\keyword{answers}
\keyword{learnr,}
\keyword{shiny,}
\keyword{student}
