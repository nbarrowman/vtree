combineVars <- function(prefix,text_part,matching_vars,checked,unchecked,z) {
  if (prefix=="any:" || prefix=="anyx:" || 
      prefix=="rany:" || prefix=="ranyx:" ||
      prefix=="anyr:" || prefix=="anyrx:") {
    
    output <- rep(FALSE,nrow(z))
    for (j in 1:length(matching_vars)) {
      convertedToLogical <- 
        ifelse(z[[matching_vars[j]]] %in% checked,TRUE,
          ifelse(z[[matching_vars[j]]] %in% unchecked,FALSE,NA))
      if (prefix=="anyx:" || prefix=="ranyx:" || prefix=="anyrx:") {
        convertedToLogical[is.na(convertedToLogical)] <- FALSE
      }
      output <- output | convertedToLogical
    }
    NewVarName <- paste0("Any: ",text_part)
  }
  if (prefix=="none:" || prefix=="nonex:") {
    output <- rep(TRUE,nrow(z))
    for (j in 1:length(matching_vars)) {
      convertedToLogical <- 
        ifelse(z[[matching_vars[j]]] %in% unchecked,TRUE,
          ifelse(z[[matching_vars[j]]] %in% checked,FALSE,NA))
      if (prefix=="nonex:") {
        convertedToLogical[is.na(convertedToLogical)] <- TRUE
      }
      output <- output & convertedToLogical
    }
    NewVarName <- paste0("None: ",text_part)
  }  
  if (prefix=="all:" || prefix=="allx:") {
    output <- rep(TRUE,nrow(z))
    for (j in seq_len(length(matching_vars))) {
      convertedToLogical <- 
        ifelse(z[[matching_vars[j]]] %in% checked,TRUE,
          ifelse(z[[matching_vars[j]]] %in% unchecked,FALSE,NA))
      if (prefix=="allx:") {
        convertedToLogical[is.na(convertedToLogical)] <- TRUE
      }
      output <- output & convertedToLogical
    }
    NewVarName <- paste0("All: ",text_part)
  }
  if (prefix=="notall:" || prefix=="notallx:") {
    output <- rep(FALSE,nrow(z))
    for (j in 1:length(matching_vars)) {
      convertedToLogical <- 
        ifelse(z[[matching_vars[j]]] %in% unchecked,TRUE,
          ifelse(z[[matching_vars[j]]] %in% checked,FALSE,NA))
      if (prefix=="notallx:") {
        convertedToLogical[is.na(convertedToLogical)] <- FALSE
      }
      output <- output | convertedToLogical
    }
    NewVarName <- paste0("Not all: ",text_part)
  }    
  return(list(output=output,NewVarName=NewVarName))
}