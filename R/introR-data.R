#' Introduction to R Dataset
#'
#' A dataset containing research results from an experiment
#' that examined how pleasant people felt about words, and the
#' information about how the word is typed. This dataset
#' examines the QWERTY effect.
#' @docType data
#'
#' @usage data(introR)
#'
#' @format A data frame with 33949 rows and 14 variables:
#'
#' \describe{
#'   \item{expno}{the experiment number we assigned to that
#'   group of participants}
#'   \item{rating}{the pleasantness rating of that word}
#'   \item{originalcode}{the word the particpiant saw}
#'   \item{id}{the participant ID number}
#'   \item{speed}{the typing speed of the participant}
#'   \item{error}{the number of typing errors by the participant}
#'   \item{whichhand}{which has the participant indicated as
#'   their dominate hand}
#'   \item{LR_switch}{the number of times typing the word would
#'   switch from left to right hands}
#'   \item{finger_switch}{the number of times you would switch
#'   fingers typing the word}
#'   \item{rha}{right hand advantage: Right - Left handed letters}
#'   \item{word_length}{the number of characters in the word}
#'   \item{letter_freq}{the average of the frequency of each of
#'   the letters in the word}
#'   \item{real_fake}{if the word was a real English word or not}
#'   \item{speed_c}{z-scored speed values}
#' }
#'
#' @keywords datasets
"introR"

