parseSummary <- function(z,vars,summary,verbose,choicechecklist,checked,unchecked) {

  # regexVarName and regexComplex could in fact be passed in
  regexVarName <- "([a-zA-Z0-9~@#()_|,.]+)"
  regexComplex <- "^((i|r|any|anyx|all|allx|notall|notallx|none|nonex)+:)*([^([:space:]|:)@\\*#]*)([@\\*#]?)(.*)$"
  
  
  regex <- "^(\\S+)\\s(.+)$"
  
  
  codevar <- gsub(regex, "\\1", summary)
  summaryvar <- heading <- codevar
  
  summaryformat <- gsub(regex, "\\2", summary)
  summaryformat[grep(regex,summary,invert=TRUE)] <- ""

  extra_variables <- NULL
  
  # Process >= tag in variable names in summary argument
  regex <- paste0("^",regexVarName,"(>=)",regexVarName)
  findgte <- grep(regex,codevar)
  if (length(findgte)>0) {
    for (i in seq_len(length(codevar))) {    
      if (i %in% findgte) {
        gtevar <- sub(regex,"\\1",codevar[i])
        if (is.null(z[[gtevar]]))
          stop(paste("Unknown variable in summary:",gtevar))                             
        gteval <- sub(regex,"\\3",codevar[i])
        newvarname <- paste0(gtevar,"_gte_",gteval)
        # Check to see if any of the values of the specified variable contain spaces
        # If they do, replace underscores in the specified value with spaces.
        if (any(length(grep(" ",names(table(z[[gtevar]]))))>0)) {
          gteval <- gsub("_"," ",gteval)
        }
        m <- z[[gtevar]]>=as.numeric(gteval)
        z[[newvarname]] <- m
        summaryvar <- newvarname
        # codevar[i] <- gtvar			
      }
    }
  }
  
  # Process != tag in variable names in summary argument
  # (Note that this comes before the = tag so that it doesn't match first.)
  regex <- paste0("^",regexVarName,"(\\!=)",regexVarName)
  findnotequal <- grep(regex,codevar)
  if (length(findnotequal)>0) {
    for (i in seq_len(length(codevar))) {    
      if (i %in% findnotequal) {
        thevar <- sub(regex,"\\1",codevar[i])
        if (is.null(z[[thevar]]))
          stop(paste("Unknown variable in summary:",thevar))                      
        theval <- sub(regex,"\\3",codevar[i])
        # Check to see if any of the values of the specified variable contain spaces
        # If they do, replace underscores in the specified value with spaces.
        if (any(length(grep(" ",names(table(z[[thevar]]))))>0)) {
          theval <- gsub("_"," ",equalval)
        }
        m <- z[[thevar]]!=theval
        z[[thevar]] <- m
        summaryvar <- thevar
        # codevar[i] <- thevar
      }
    }
  }
  
  
  # Process = tag in variable names in summary argument
  regex <- paste0("^",regexVarName,"(=)",regexVarName)
  findequal <- grep(regex,codevar)
  if (length(findequal)>0) {
    for (i in seq_len(length(codevar))) {    
      if ((i %in% findequal) && !(i %in% findnotequal)) {
        equalvar <- sub(regex,"\\1",codevar[i])
        if (is.null(z[[equalvar]]))
          stop(paste("Unknown variable in summary:",equalvar))                      
        equalval <- sub(regex,"\\3",codevar[i])
        newvarname <- paste0(equalvar,equalval)
        # Check to see if any of the values of the specified variable contain spaces.
        # If they do, replace underscores in the specified value with spaces.
        if (any(length(grep(" ",names(table(z[[equalvar]]))))>0)) {
          equalval <- gsub("_"," ",equalval)
        }
        m <- z[[equalvar]]==equalval
        z[[newvarname]] <- m
        summaryvar <- newvarname
        # codevar[i] <- equalvar			
      }
    }
  } 
  
  # Process > tag in variable names in summary argument
  regex <- paste0("^",regexVarName,"(>)",regexVarName)
  findgt <- grep(regex,codevar)
  if (length(findgt)>0) {
    for (i in seq_len(length(codevar))) {    
      if (i %in% findgt) {
        gtvar <- sub(regex,"\\1",codevar[i])
        if (is.null(z[[gtvar]]))
          stop(paste("Unknown variable in summary:",gtvar))                             
        gtval <- sub(regex,"\\3",codevar[i])
        newvarname <- paste0(gtvar,"_gt_",gtval)
        # Check to see if any of the values of the specified variable contain spaces
        # If they do, replace underscores in the specified value with spaces.
        if (any(length(grep(" ",names(table(z[[gtvar]]))))>0)) {
          gtval <- gsub("_"," ",gtval)
        }
        m <- z[[gtvar]]>as.numeric(gtval)
        z[[newvarname]] <- m
        summaryvar <- newvarname
        # codevar[i] <- gtvar			
      }
    }
  }
  
  # Process < tag in variable names in summary argument
  regex <- paste0("^",regexVarName,"(<)",regexVarName)
  findlt <- grep(regex,codevar)
  if (length(findlt)>0) {
    for (i in seq_len(length(codevar))) {    
      if (i %in% findlt) {
        ltvar <- sub(regex,"\\1",codevar[i])
        if (is.null(z[[ltvar]]))
          stop(paste("Unknown variable in summary:",ltvar))                             
        ltval <- sub(regex,"\\3",codevar[i])
        newvarname <- paste0(ltvar,"_lt_",ltval)
        # Check to see if any of the values of the specified variable contain spaces
        # If they do, replace underscores in the specified value with spaces.
        if (any(length(grep(" ",names(table(z[[ltvar]]))))>0)) {
          ltval <- gsub("_"," ",ltval)
        }
        m <- z[[ltvar]]<as.numeric(ltval)
        z[[newvarname]] <- m
        summaryvar <- newvarname
        # codevar[i] <- ltvar			
      }
    }
  } 
  
  # Process is.na: tag in variable names to handle individual missing value checks
  regex <- paste0("^is\\.na:",regexVarName,"$")
  findna <- grep(regex,codevar)
  if (length(findna)>0) {
    for (i in seq_len(length(vars))) {
      if (i %in% findna) {
        navar <- sub(regex,"\\1",codevar[i])
        if (is.null(z[[navar]]))
          stop(paste("Unknown variable:",navar))
        NewVar <- paste0("is.na:",navar)
        m <- is.na(z[[navar]])
        z[[NewVar]] <- factor(m, levels = c(FALSE, TRUE),c("not N/A","N/A"))
        summaryvar <- NewVar
      }
    }
  }      
  
  # Process stem: tag in variable names in summary argument
  regex <- paste0("^stem:",regexVarName,"$")
  findstem <- grep(regex,codevar)
  if (length(findstem)>0) {
    for (i in seq_len(length(codevar))) {    
      if (i %in% findstem) {
        thevar <- sub(regex,"\\1",codevar[i])
        expanded_stem <- names(z)[grep(paste0("^",thevar,"___[0-9]+$"),names(z))]
        if (verbose) message(paste0(codevar[i]," expands to: ",paste(expanded_stem,collapse=", ")))
        if (length(expanded_stem)==0) {
          stop(paste0("summary: Could not find variables with names matching the specified stem: ",thevar))
        }
        rexp0 <- "\\(choice=.+\\)"
        rexp1 <- "(.+) \\(choice=(.+)\\)"
        rexp2 <- "(.+): (.+)"
        expandedvars <- c()
        if (choicechecklist) {
          for (j in 1:length(expanded_stem)) {
            lab <- attributes(z[[expanded_stem[j]]])$label
            if (length(grep(rexp0,lab))>0) {
              REDCap_var_label <- sub(rexp1,"\\1",lab)
              choice <- sub(rexp1,"\\2",lab)
            } else
              if (length(grep(rexp2,lab))>0) {
                choice <- sub(rexp2,"\\2",lab)
              } else {
                stop("Could not find value of checklist item")
              }
            if (verbose) message(paste0(expanded_stem[j]," is ",choice))
            z[[choice]] <- z[[expanded_stem[j]]]
            expandedvars <- c(expandedvars,choice)
          }            
        } else {
          expandedvars <- c(expandedvars,expanded_stem)
        }
        summaryvar <- expandedvars
        heading <- expandedvars            
        extra_variables <- c(extra_variables,expanded_stem)
        summaryformat <- rep(summaryformat,length(expanded_stem))
      }
    }
  }
  
  # Process stemc: tag in variable names in summary argument
  regex <- paste0("^stemc:",regexVarName,"$")
  findstem <- grep(regex,codevar)
  if (length(findstem)>0) {
    for (i in seq_len(length(codevar))) {    
      if (i %in% findstem) {
        y <- rep("",nrow(z))
        none <- rep(TRUE,nrow(z))
        thevar <- sub(regex,"\\1",codevar[i])
        expanded_stem <- names(z)[grep(paste0("^",thevar,"___[0-9]+$"),names(z))]
        if (verbose) message(paste0(codevar[i]," expands to: ",paste(expanded_stem,collapse=", ")))
        if (length(expanded_stem)==0) {
          stop(paste0("summary: Could not find variables with names matching the specified stem: ",thevar))
        }
        rexp0 <- "\\(choice=.+\\)"
        rexp1 <- "(.+) \\(choice=(.+)\\)"
        rexp2 <- "(.+): (.+)"
        expandedvars <- c()
        if (choicechecklist) {
          for (j in 1:length(expanded_stem)) {
            lab <- attributes(z[[expanded_stem[j]]])$label
            if (length(grep(rexp0,lab))>0) {
              REDCap_var_label <- sub(rexp1,"\\1",lab)
              choice <- sub(rexp1,"\\2",lab)
            } else
              if (length(grep(rexp2,lab))>0) {
                choice <- sub(rexp2,"\\2",lab)
              } else {
                stop("Could not find value of checklist item")
              }
            y <- ifelse(z[[expanded_stem[j]]]==1,
              ifelse(y=="",choice,paste0(y,"+",choice)),y)
            if (verbose) message(paste0(expanded_stem[j]," is ",choice))
            z[[choice]] <- z[[expanded_stem[j]]]
            expandedvars <- c(expandedvars,choice)
          }            
        } else {
          expandedvars <- c(expandedvars,expanded_stem)
        }
        y[y %in% ""] <- "*None"
        newvar <- paste0("stem:",thevar)
        z[[newvar]] <- y
        summaryvar <- newvar
        heading <- ""            
        extra_variables <- c(extra_variables,newvar)
        summaryformat <- rep(summaryformat,length(expanded_stem))
      }
    }
  }      
  
  #
  # >> Process complex summary specification ----
  # including REDCap variables, intersections, and wildcards
  #
  # Uses the same regular expression as for variable specifications,
  # namely the string regexComplex
  
  match_regex <- grep(regexComplex,codevar)
  if (length(match_regex)>0) {
    expandedvars <- c()
    for (i in seq_len(length(codevar))) {    
      if (i %in% match_regex) {
        y <- rep("",nrow(z))
        none <- rep(TRUE,nrow(z))
        prefix <- sub(regexComplex,"\\1",codevar[i])
        text_part <- sub(regexComplex,"\\3",codevar[i])
        wildcard <- sub(regexComplex,"\\4",codevar[i])
        tail <- sub(regexComplex,"\\5",codevar[i])
        if (prefix=="" && wildcard=="") {
          expandedvars <- c(expandedvars,vars[i]) 
        } else            
          if (prefix=="any:"    || prefix=="anyx:"    || 
              prefix=="none:"   || prefix=="nonex:"   ||
              prefix=="all:"    || prefix=="allx:"    ||
              prefix=="notall:" || prefix=="notallx:" ) {
            if (wildcard=="*") {
              matching_vars <- names(z)[grep(paste0("^",text_part,".*",tail,"$"),names(z))]
            } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+",tail,"$"),names(z))]
              } else {
                stop("Invalid wildcard in summary specification")
              }
            expandedvars <- c()
            if (length(matching_vars)==0) {
              stop("Could not find variables matching summary specification")
            }      
            if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
            out <- combineVars(prefix,text_part,matching_vars,checked,unchecked,z)
            output <- out$output
            NewVarName <- out$NewVarName   
            
            z[[NewVarName]] <- output
            expandedvars <- c(expandedvars,NewVarName)          
            
            newvarheading <- NewVarName
            summaryvar <- NewVarName
            heading <- NewVarName          
            extra_variables <- c(extra_variables,NewVarName)
            summaryformat <- rep(summaryformat,length(matching_vars))
            
          } else
            if (prefix=="r:" || prefix=="ir:" || prefix=="ri:" ||
                prefix=="anyr:" || prefix=="rany:" ||
                prefix=="allr:" || prefix=="rall:" || 
                prefix=="noner:" || prefix=="rnone:" ||
                prefix=="notallr:" || prefix=="rnotall:") {
              if (wildcard=="@") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"___[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard in summary specification")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables names matching summary specification")
              }
              if (verbose) message(paste0(codevar[i]," expands to: ",paste(matching_vars,collapse=", ")))
              #message(paste0("prefix-->",prefix,"<-- matching_vars=",paste(matching_vars,collapse=", ")))
              rexp0 <- "\\(choice=.+\\)"
              rexp1 <- "(.+) \\(choice=(.+)\\)"
              rexp2 <- "(.+): (.+)"
              expandedvars <- c()
              
              if (prefix=="r:") {
                if (choicechecklist) {
                  for (j in seq_len(length(matching_vars))) {
                    lab <- attributes(z[[matching_vars[j]]])$label
                    if (length(grep(rexp0,lab))>0) {
                      REDCap_var_label <- sub(rexp1,"\\1",lab)
                      choice <- sub(rexp1,"\\2",lab)
                    } else
                      if (length(grep(rexp2,lab))>0) {
                        choice <- sub(rexp2,"\\2",lab)
                      } else {
                        stop("Could not find value of checklist item")
                      }
                    if (verbose) message(paste0(matching_vars[j]," is ",choice))
                    z[[choice]] <- z[[matching_vars[j]]]
                    expandedvars <- c(expandedvars,choice)
                  }            
                } else {
                  expandedvars <- c(expandedvars,matching_vars)
                }
                summaryvar <- expandedvars
                heading <- expandedvars            
                extra_variables <- c(extra_variables,matching_vars)
                summaryformat <- rep(summaryformat,length(matching_vars))
              } else
                if (prefix=="anyr:" || prefix=="rany:") {
                  lab1 <- attributes(z[[matching_vars[1]]])$label
                  if (length(grep(rexp0,lab1))>0) {
                    REDCap_var_label <- sub(rexp1,"\\1",lab1)
                  } else {
                    REDCap_var_label <- sub(rexp2,"\\1",lab1)
                  }
                  if (choicechecklist) {
                    out <- combineVars(prefix,REDCap_var_label,matching_vars,checked,unchecked,z)
                  }
                  output <- out$output
                  REDCap_var_label_any <- out$NewVarName
                  z[[REDCap_var_label_any]] <- output
                  expandedvars <- c(expandedvars,REDCap_var_label_any)
                  summaryvar <- expandedvars
                  heading <- expandedvars            
                  extra_variables <- c(extra_variables,matching_vars)
                  summaryformat <- rep(summaryformat,length(matching_vars))
                } else
                  if (prefix=="noner:" || prefix=="rnone:") {
                    lab1 <- attributes(z[[matching_vars[1]]])$label
                    if (length(grep(rexp0,lab1))>0) {
                      REDCap_var_label <- sub(rexp1,"\\1",lab1)
                    } else {
                      REDCap_var_label <- sub(rexp2,"\\1",lab1)
                    }
                    if (choicechecklist) {
                      for (j in 1:length(matching_vars)) {
                        convertedToLogical <- 
                          ifelse(!(z[[matching_vars[j]]] %in% checked),TRUE,
                            ifelse(!(z[[matching_vars[j]]] %in% unchecked),FALSE,NA))
                        if (j==1) {
                          output <- convertedToLogical
                        } else {
                          output <- output & convertedToLogical
                        }
                      }
                    } 
                    REDCap_var_label_none <- paste0("None: ",REDCap_var_label)
                    z[[REDCap_var_label_none]] <- output
                    expandedvars <- c(expandedvars,REDCap_var_label_none)
                    summaryvar <- expandedvars
                    heading <- expandedvars            
                    extra_variables <- c(extra_variables,matching_vars)
                    summaryformat <- rep(summaryformat,length(matching_vars))
                  } else                
                    if (prefix=="allr:" || prefix=="rall:") {
                      lab1 <- attributes(z[[matching_vars[1]]])$label
                      if (length(grep(rexp0,lab1))>0) {
                        REDCap_var_label <- sub(rexp1,"\\1",lab1)
                      } else {
                        REDCap_var_label <- sub(rexp2,"\\1",lab1)
                      }
                      if (choicechecklist) {
                        for (j in 1:length(matching_vars)) {
                          convertedToLogical <- 
                            ifelse(z[[matching_vars[j]]] %in% checked,TRUE,
                              ifelse(z[[matching_vars[j]]] %in% unchecked,FALSE,NA))
                          if (j==1) {
                            output <- convertedToLogical
                          } else {
                            output <- output & convertedToLogical
                          }
                        }
                      } 
                      REDCap_var_label_all <- paste0("All: ",REDCap_var_label)
                      z[[REDCap_var_label_all]] <- output
                      expandedvars <- c(expandedvars,REDCap_var_label_all)
                      summaryvar <- expandedvars
                      heading <- expandedvars            
                      extra_variables <- c(extra_variables,matching_vars)
                      summaryformat <- rep(summaryformat,length(matching_vars))
                    } else
                      if (prefix=="notallr:" || prefix=="rnotall:") {
                        lab1 <- attributes(z[[matching_vars[1]]])$label
                        if (length(grep(rexp0,lab1))>0) {
                          REDCap_var_label <- sub(rexp1,"\\1",lab1)
                        } else {
                          REDCap_var_label <- sub(rexp2,"\\1",lab1)
                        }
                        if (choicechecklist) {
                          for (j in 1:length(matching_vars)) {
                            convertedToLogical <- 
                              ifelse(!(z[[matching_vars[j]]] %in% checked),TRUE,
                                ifelse(!(z[[matching_vars[j]]] %in% unchecked),FALSE,NA))
                            if (j==1) {
                              output <- convertedToLogical
                            } else {
                              output <- output | convertedToLogical
                            }
                          }
                        } 
                        REDCap_var_label_notall <- paste0("Not all: ",REDCap_var_label)
                        z[[REDCap_var_label_notall]] <- output
                        expandedvars <- c(expandedvars,REDCap_var_label_notall)
                        summaryvar <- expandedvars
                        heading <- expandedvars            
                        extra_variables <- c(extra_variables,matching_vars)
                        summaryformat <- rep(summaryformat,length(matching_vars))
                      } else                                                
                        if (prefix=="ri:" | prefix=="ir:") {
                          if (choicechecklist) {
                            for (j in seq_len(length(matching_vars))) {
                              lab <- attributes(z[[matching_vars[j]]])$label
                              if (length(grep(rexp0,lab))>0) {
                                REDCap_var_label <- sub(rexp1,"\\1",lab)
                                choice <- sub(rexp1,"\\2",lab)
                              } else
                                if (length(grep(rexp2,lab))>0) {
                                  choice <- sub(rexp2,"\\2",lab)
                                } else {
                                  stop("Could not find value of checklist item")
                                }
                              y <- ifelse(z[[matching_vars[j]]]==1,
                                ifelse(y=="",choice,paste0(y,"+",choice)),y)
                              if (verbose) message(paste0(matching_vars[j]," is ",choice))
                              z[[choice]] <- z[[matching_vars[j]]]
                              expandedvars <- c(matching_vars,choice)
                            }            
                          } else {
                            expandedvars <- c(expandedvars,matching_vars)
                          }
                          y[y %in% ""] <- "*None"
                          newvar <- paste0("stem:",text_part)
                          z[[newvar]] <- y
                          summaryvar <- newvar
                          heading <- ""            
                          extra_variables <- c(extra_variables,newvar)
                          summaryformat <- rep(summaryformat,length(matching_vars))                
                        }  
            } else {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*",tail,"$"),names(z))]
              } else
                if (wildcard=="#") {
                  matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+",tail,"$"),names(z))]
                } else {
                  stop("Invalid wildcard in summary specification")
                }
              if (length(matching_vars)==0) {
                stop("summary: Could not find variables with matching names")
              }
              if (verbose) message(paste0(codevar[i]," expands to: ",paste(matching_vars,collapse=", ")))
              #message(paste0("prefix-->",prefix,"<-- matching_vars=",paste(matching_vars,collapse=", ")))
              if (prefix=="i:") {
                expandedvars <- c()
                if (choicechecklist) {
                  for (j in seq_len(length(matching_vars))) {
                    y <- ifelse(z[[matching_vars[j]]]==1,
                      ifelse(y=="",matching_vars[j],paste0(y,"+",matching_vars[j])),y)
                  }
                } 
                y[y %in% ""] <- "*None"
                newvar <- paste0("combinations_of_",paste(matching_vars,collapse="_"))
                newvarheading <- paste0("combinations of ",paste(matching_vars,collapse=", "))
                z[[newvar]] <- y
                summaryvar <- newvar
                heading <- newvarheading           
                extra_variables <- c(extra_variables,newvar)
                summaryformat <- rep(summaryformat,length(matching_vars))
              } else 
                if (prefix=="") {
                  summaryvar <- matching_vars
                  heading <- matching_vars          
                  summaryformat <- rep(summaryformat,length(matching_vars))
                } else {
                  stop("Unknown prefix")
                }
            }
      }
    }
  }     
  
  # If an element of codevar is not the name of a variable in z,
  # perhaps it's an expression that can be evaluated in z
  if ((length(summaryvar)==1) && (summaryvar!="_")) {
    if (length(grep("^([oair]+[oair]*:)*(\\S*)([\\*#@])$",summaryvar))==0) {
      if (length(grep("^stem:",summaryvar))==0) {   # except for stems
        if (length(grep("^stemc:",summaryvar))==0) {   # except for stems
          if (length(grep("\\*$",summaryvar))==0) {   # except for ending in *
            if (length(grep("#$",summaryvar))==0) {   # except for ending in #
              if (!(summaryvar %in% names(z))) {
                derivedvar <- with(z,eval(parse(text=summaryvar,keep.source=FALSE))) 
                z[[summaryvar]] <- derivedvar
              }
            }
          }
        }
      }
    }
  }
  
  #browser()
  
  list(z=z,summaryvar=summaryvar,format=summaryformat,heading=heading,codevar=codevar)
}
