around <- function (x, digits = 2, thousands = "",tooLong = 12) {
  if (is.character(x)) {
    x
  } else
  if (is.integer(x) || is.factor(x)) {
    as.character(x)
  } else
  if (is.data.frame(x)) {
      for (i in seq_len(ncol(x))) {
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
        big.mark = thousands, format = "f", flag = "#")
      result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
        tooLong], digits = digits, drop0trailing = TRUE,
        big.mark = thousands, format = "g", flag = "#")
    }
    else {
      result <- formatC(x, digits = digits, drop0trailing = FALSE,
        big.mark = thousands, format = "f", flag = "#")
      result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
        tooLong], digits = digits, drop0trailing = FALSE,
        big.mark = thousands, format = "g", flag = "#")
    }
    #browser()
    result[result == "-0"] <- "0"
    result[result == "-0.0"] <- "0.0"
    result[result == "-0.00"] <- "0.00"
    result[result == "-0.000"] <- "0.000"
    result
  }
}
