tableWithoutSort <- function(x,exclude = NA) {
  tab <- table(x,exclude=exclude)
  u <- unique(x)
  if (any(is.na(u))) {
    u <- u[!is.na(u)]
    ustr <- as.character(u)
    count <- tab[ustr]
    count <- c(count,tab[is.na(names(tab))])
  } else {
    ustr <- as.character(u)
    #count <- tab[ustr]  
    count <- tab[match(ustr,names(tab))]  
  }
  names(dimnames(count)) <- NULL
  count
}

