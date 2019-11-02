#' @title Convert a crosstabulation into a data frame of cases.
#'
#' @author Nick Barrowman, based on the \code{countsToCases} function at \url{http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/#countstocases-function}
#'
#' @description
#' Convert a table of crosstabulated counts into a data frame of cases.
#'
#' @param x  a matrix or table of frequencies representing a crosstabulation.
 
#' @return
#'   Returns a data frame of cases.
#'
#' @examples
#' # The Titanic data set is in the datasets package.
#' # Convert it from a 4 x 2 x 2 x 2 crosstabulation 
#' # to a 4-column data frame of 2201 individuals
#' titanic <- crosstabToCases(Titanic)
#'
#' @export
#'

crosstabToCases <- function(x) {

  if (!is.table(x)) {
    if (is.matrix(x)) {
      x <- table(x)
    } else {
      stop("not a matrix")
    }
  }
  
  u <- data.frame(x)

  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(u)), u$Freq)

  # Drop count column
  u$Freq <- NULL

  # Get the rows from x
  rows <- u[idx, ]
  rownames(rows) <- NULL
  rows
}
