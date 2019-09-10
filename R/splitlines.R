splitlines <- function (x, width = 10, sp = "\n", at = c(" ", "-", "+", "_", "=", "/"), same = FALSE) {

# NOTE: I removed forward slash from the default at argument,
# because it caused a problem with HTML where / is important.
# e.g. <BR/>

  if (any(is.na(x))) stop("Missing value in vector of strings.")

  n <- nchar(x)
  nsp <- nchar(sp)
  result <- rep("", length(x))
  splits <- rep(0, length(x))
  if (is.null(x) || (length(x)==0)) return(NULL)
  for (i in 1:length(x)) {
      count <- 0
      start <- 1
      for (j in 1:(n[i])) {
        count <- count + 1
        char <- substring(x[i], j, j)
        if (char %in% sp) {
          count <- 0
        } else {
          if (char %in% at) {
            if (count > width) {
              if (char == " ") {
                end <- j - 1
              }
              else {
                end <- j
              }
              result[i] <- paste0(result[i], substring(x[i],
                start, end), sp)
              start <- j + 1
              count <- 0
              splits[i] <- splits[i] + 1
            }
          }
        }
      }
      if (start <= n[i])
          result[i] <- paste0(result[i], substring(x[i], start,
              n[i]))
  }
  if (same) {
      maxsplits <- max(splits)
      for (i in 1:length(x)) {
          while (splits[i] < maxsplits) {
              result[i] <- paste0(result[i], sp)
              splits[i] <- splits[i] + 1
          }
      }
  }
  result
}
