#'
#' @title Build a data frame to display with vtree
#'
#' @description
#' Build a data frame by specifying variable names and patterns of values together with frequencies.
#'
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#'
#' @param varnames A vector of variable names.
#' @param ...      Lists of patterns and the frequency of each pattern.
#'                 When a pattern is shorted than the list of variable names
#'                 (for example, 3 variable names but only 2 values in the pattern),
#'                 \code{NA}'s are substituted for the missing variable names.
#' 
#' @details
#' Suppose \code{varnames=c("animal","size","hair")},
#' then one pattern would be \code{list("dog","small","short",4)},
#' which specifies 4 dogs that are small and short-haired.
#' Another pattern could be \code{list("cat","large","long",101)},
#' specifying 101 large cats.
#' 
#' @return A data frame.
#' 
#' @examples
#' # Number of countries in Africa, whether population is over 30 million or not,
#' # and whether landlocked or not.
#' # https://www.worldometers.info/geography/how-many-countries-in-africa/
#' #
#' df <- build.data.frame(
#'  c("continent","population","landlocked"),
#'  list("Africa","Over 30 million","landlocked",2),
#'  list("Africa","Over 30 million","not landlocked",12),
#'  list("Africa","Under 30 million","landlocked",14),
#'  list("Africa","Under 30 million","not landlocked",26))
#' 
#' @export
#' 
build.data.frame <- function(varnames,...) {
  input <- list(...)
  nvar <- length(varnames)
  count <- NULL
  for (i in 1:length(input)) {
    m <- length(input[[i]])
    if ((m-1)>nvar) stop("Number of input in a list cannot exceed number of variable names")
    count <- c(count,input[[i]][[m]])
  }
  listit <- vector("list",length=length(input[[1]])-1)
  names(listit) <- varnames
  for (i in 1:length(input)) {
    for (j in 1:(length(input[[i]])-1)) {
      listit[[j]] <- c(listit[[j]],rep(input[[i]][[j]],count[i]))
    }
    if ( (length(input[[i]])-1) < nvar) {
      for (j in (length(input[[i]]):nvar)) {
        listit[[j]] <- c(listit[[j]],rep(NA,count[i]))
      }
    }
  }
  as.data.frame(listit)
}
