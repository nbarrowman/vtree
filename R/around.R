around <- function (x, digits = 2, tooLong = 10) {
  if (is.character(x)) {
    x
  } else
  if (is.integer(x) || is.factor(x)) {
    as.character(x)
  } else
  if (is.data.frame(x)) {
      for (i in 1:ncol(x)) {
          x[[i]] <- around(x[[i]], digits = digits)
      }
      x
  } else
  if (!is.numeric(x)) {
    if (all(is.na(x))) {
      rep("NA",length(x))
    } else {
      x
    }
  } else {
    if (digits == 0) {
      result <- formatC(x, digits = digits, drop0trailing = TRUE,
        format = "f", flag = "#")
      result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
        tooLong], digits = digits, drop0trailing = TRUE,
        format = "g", flag = "#")
    }
    else {
      result <- formatC(x, digits = digits, drop0trailing = FALSE,
        format = "f", flag = "#")
      result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
        tooLong], digits = digits, drop0trailing = FALSE,
        format = "g", flag = "#")
    }
    result[result == "-0"] <- "0"
    result[result == "-0.0"] <- "0.0"
    result[result == "-0.00"] <- "0.00"
    result[result == "-0.000"] <- "0.000"
    result
  }
}
