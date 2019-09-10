nodeMeanSD <- function(u,varname,value,args) {
  if (is.null(args$digits)) args$digits <- 1   # default value

  paste0("\n",args$var,": mean (SD)\n",
    around(mean(u[[args$var]],na.rm=TRUE),args$digits),
    " (",around(sd(u[[args$var]],na.rm=TRUE),args$digits),")",
    " mv=",sum(is.na(u[[args$var]])))
}

