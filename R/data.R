#' Fake clinical dataset
#'
#' A dataset consisting of made-up clinical data.
#' Note that some observations are missing (i.e. NAs).
#'
#' @format A small data frame in which the rows represent (imaginary) patients and
#' the columns represent variables of possible clinical relevance.
#'
#' \describe{
#'   \item{id}{Integer: Patient ID number}
#'   \item{Group}{Factor: Treatment Group, A or B}
#'   \item{Severity}{Factor representing severity of condition: Mild, Moderate, or Severe}
#'   \item{Sex}{Factor: M or F}
#'   \item{Male}{Integer: Sex coded as 1=M, 0=F}
#'   \item{Age}{Integer: Age in years, continuous}
#'   \item{Score}{Integer: Score on a test}
#'   \item{Category}{Factor: single, double, or triple}
#'   \item{Pre}{Numeric: initial measurement}
#'   \item{Post}{Numeric: measurement taken after something happened}
#'   \item{Post2}{Numeric: measurement taken at the very end of the study}
#'   \item{Time}{Numeric: time to event, or time of censoring}
#'   \item{Event}{Integer: Did the event occur? 1=yes, 0=no (i.e. censoring)}
#'   \item{Ind1}{Integer: Indicator variable for a certain characteristic, 1=present, 0=absent}
#'   \item{Ind2}{Integer: Indicator variable for a certain characteristic, 1=present, 0=absent}
#'   \item{Ind3}{Integer: Indicator variable for a certain characteristic, 1=present, 0=absent}
#'   \item{Ind4}{Integer: Indicator variable for a certain characteristic, 1=present, 0=absent}
#'   \item{Viral}{Logical: Does this patient have a viral illness?}
#'
#' }
"FakeData"

#' Fake Randomized Controlled Trial (RCT) data
#'
#' A dataset consisting of made-up RCT data.
#'
#' @format A small data frame in which the rows represent (imaginary) patients and
#' the columns represent variables of possible clinical relevance.
#'
#' \describe{
#'   \item{id}{String: Patient ID number}
#'   \item{eligible}{Factor: Eligible or Ineligible}
#'   \item{randomized}{Factor: Randomized or Not randomized}
#'   \item{group}{Factor: A or B}
#'   \item{followup}{Factor: Followed up or Not followed up}
#'   \item{analyzed}{Factor: Analyzed or Not analyzed}
#'
#' }
"FakeRCT"
