#' @importFrom stats median quantile sd

summaryNodeFunction <- function (u, varname, value, args) {
  
  # Conditional substitution
  # gsub evaluates the replacement expression even if x doesn't match pattern.
  # This function only evaluates the replacement expression if x matches pattern.
  condsub <- function(pattern,replacement,x) {
    if (length(grep(pattern,x))>0) {
      gsub(pattern,replacement,x)
    } else {
      x
    }
  }
  
  fullsummary <- function(w,digits,varname) {
    nMissing <- sum(is.na(w))
    if (length(w)<=3) {
      return(paste0(varname,"\n",paste(around(w,digits=digits),collapse=", ")))
    }
    if (nMissing==length(w)) {
      return(paste0(varname,"\n","missing ",nMissing))
    }
    med <- around(as.numeric(stats::median(w,na.rm=TRUE)), digits = digits)
    lo <- around(as.numeric(min(w,na.rm=TRUE)), digits = digits)
    hi <- around(as.numeric(max(w,na.rm=TRUE)), digits = digits)
    q25 <- around(quantile(w,0.25,na.rm=TRUE), digits = digits)
    q75 <- around(quantile(w,0.75,na.rm=TRUE), digits = digits)
    mn <- around(mean(w,na.rm=TRUE), digits = digits)
    s <- around(stats::sd(w,na.rm=TRUE), digits = digits)
    paste0(
      varname,"\n",
      "missing ",nMissing,"\n",
      "mean ",mn," SD ",s,"\n",
      "med ",med," IQR ",q25,", ",q75,"\n",
      "range ",lo,", ",hi)
  }
  
  medianfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    m <- around(stats::median(w,na.rm=TRUE), digits = cdigits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }
  
  minfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    m <- around(min(w,na.rm=TRUE), digits = cdigits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }
  
  maxfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    m <- around(max(w,na.rm=TRUE), digits = cdigits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }  

  IQRfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    i <- paste0(
      around(qntl(w,0.25,na.rm=TRUE), digits = cdigits),", ",
      around(qntl(w,0.75,na.rm=TRUE), digits = cdigits))
    if (nMissing>0) {
      paste0(i," mv=",nMissing)
    } else {
      i
    }
  }

  rangefunc <- function(w,cdigits,na.rm=FALSE) {
    #print(w)
    if (na.rm) w <- w[!is.na(w)]
    nMissing <- sum(is.na(w))
    if (length(w[!is.na(w)])==0) {
      if (nMissing==0) {
        "No values"
      } else {
        paste0("mv=",nMissing)
      }
    } else {
      r <- paste0(
        around(min(w,na.rm=TRUE), digits = cdigits),", ",
        around(max(w,na.rm=TRUE), digits = cdigits))
      if (nMissing>0) {
        paste0(r," mv=",nMissing)
      } else {
        r
      }
    }
  }
  
  SDfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    s <- around(stats::sd(w,na.rm=TRUE), digits = cdigits)
    if (nMissing>0) {
      paste0(s," mv=",nMissing)
    } else {
      s
    }
  }  

  sumfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    s <- around(sum(w,na.rm=TRUE), digits = cdigits)
    if (nMissing>0) {
      paste0(s," mv=",nMissing)
    } else {
      s
    }
  }
  meanfunc <- function(w,cdigits) {
    nMissing <- sum(is.na(w))
    m <- around(mean(w,na.rm=TRUE), digits = cdigits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }
    
  justpct <- function(w,digits=2,vp=TRUE,empty="") {
    if (vp) {
      num <- sum(w==1,na.rm=TRUE)
      den <- length(w) - sum(is.na(w))
    } else {
      num <- sum(w==1,na.rm=TRUE)
      den <- length(w)
    }
    pctString <- paste0(around(100*num/den,digits),"%")
    if (den==0) {
      pctString <- empty
    }
    if (any(is.na(w)))
      pctString <- paste0(pctString," mv=",sum(is.na(w)))
    pctString
  }
  
  nAndpct <- function(w,digits=2,vp=TRUE,empty="") {
    if (vp) {
      num <- sum(w==1,na.rm=TRUE)
      den <- length(w) - sum(is.na(w))
    } else {
      num <- sum(w==1,na.rm=TRUE)
      den <- length(w)
    }
    npctString <- paste0(num," (",
      around(100*num/den,digits),"%)")
    if (den==0) {
      npctString <- empty
    }
    if (any(is.na(w)))
      npctString <- paste0(npctString," mv=",sum(is.na(w)))
    npctString
  }
  
  
  freqfunc <- function(w,digits=2,vp=TRUE,empty="",
    pcs = "%",  showN = FALSE, shown = TRUE, showp = TRUE, 
    nmiss = FALSE, nmiss0 = FALSE, includemiss = TRUE, showzero = FALSE, 
    percentfirst = FALSE, sep = ", ",sort=FALSE) {
    x <- w
 
    nmissString <- ""
    missingNum <- sum(is.na(x))
    if (nmiss) {
        nmissString <- paste0("mv=", missingNum)
        if (!vp) 
            nmissString <- paste0("[", nmissString, "]")
        nmissString <- paste0(" ^", nmissString, "^")
        if (!nmiss0 & missingNum == 0) 
            nmissString <- ""
    }
    if (vp) {
        x <- x[!is.na(x)]
    }
    if (is.logical(x)) {
        x <- factor(x, c("FALSE", "TRUE"))
    }
    if (length(x) == 0 & (!is.factor(x))) 
        return(empty)
    tab <- table(x, exclude = NULL)
    if (sort) {
      tab <- rev(sort(tab))
    }
    if (any(is.na(names(tab)))) 
        names(tab)[is.na(names(tab))] <- "NA"
    result <- ""
    if (shown) {
        pr <- paste(result)
        if (!showzero) 
            pr[pr == "0"] <- ""
        result <- paste0(pr, tab)
        if (showN) 
            result <- paste0(result, "/", length(x))
        if (showp) 
            result <- paste0(result, " (", sep = "")
    }
    if (showp) {
        result <- paste0(result, around(100 * as.numeric(tab)/sum(tab), 
            digits = digits), pcs)
        if (shown) 
            result <- paste0(result, ")")
    }
    if (percentfirst & shown & showp) {
        result <- paste(around(100 * as.numeric(tab)/sum(tab), 
            digits = digits), pcs, sep = "")
        result <- paste0(result, " (", tab)
        if (showN) 
            result <- paste0(result, "/", length(x))
        result <- paste0(result, ")")
    }
    if (!showzero) result[!is.na(tab) & tab == 0] <- ""
    
    result <- paste0(result, nmissString)
    names(result) <- names(tab)
    result <- result[names(result) != "NA"]
    
    if (includemiss) {
      if (missingNum>0 | showzero) {
        result["NA"] <- missingNum
      }
    }
    paste0(paste0(names(result),": ",result),collapse=sep)
  }
  

  qntl <- function(x,...) {
    if (any(is.na(x))) {
      NA
    } else {
      stats::quantile(x,...)
    }
  }

  sepN <- args$sepN
  
  if (is.null(args$digits))
    args$digits <- 1
  if (is.null(args$cdigits))
    args$cdigits <- 2
  if (is.null(args$na.rm))
    args$na.rm <- TRUE

  if (is.null(args$root)) {
    args$root <- FALSE
  }

  if (is.null(args$leaf)) {
    args$leaf <- FALSE
  }

  nargs <- length(args$var)
  RESULT <- rep("",nargs)
  for (i in 1:nargs) {
  
    var <- args$var[i]
    
    if (args$format[i]=="") {
      ShowFullSummary <- TRUE
    } else {
      ShowFullSummary <- FALSE
    }
    
    if (length(grep("%combo%",args$format[i]))>0) {
      ShowCombinations <- TRUE
    } else {
      ShowCombinations <- FALSE
    }
    
    if (length(grep("%sort%",args$format[i]))>0) {
      SortIt <- TRUE
    } else {
      SortIt <- FALSE
    }    
    
    # check if it's a stem
    if (length(grep("^stem:",var))>0) {
      thevar <- sub("^stem:(\\S+)","\\1",var)
      expanded_stem <- names(u)[grep(paste0("^",thevar,"___[0-9]+$"),names(u))]
      if (ShowCombinations) {
        y <- rep("",nrow(u))
      } else {
        y <- NULL
      }
      none <- rep(TRUE,nrow(u))
      for (j in 1:length(expanded_stem)) {
        rexp1 <- ".+\\(choice=(.+)\\)"
        rexp2 <- ".+: (.+)"
        lab <- attributes(u[[expanded_stem[j]]])$label
        if (length(grep(rexp1,lab))>0) {
          choice <- sub(rexp1,"\\1",lab)
        } else
        if (length(grep(rexp2,lab))>0) {
          choice <- sub(rexp2,"\\1",lab)
        } else {
          stop("Could not find value of checklist item")
        }
        if (ShowCombinations) {
          y <- ifelse(u[[expanded_stem[j]]]==1,
            ifelse(y=="",choice,paste0(y,"+",choice)),y)
        } else {
          none <- none & u[[expanded_stem[j]]]==0
          y <- c(y,rep(choice,sum(u[[expanded_stem[j]]])))
        }
      }
      if (ShowCombinations) {
        y[y==""] <- "*None"
      } else {
        y <- c(y,rep("*None",sum(none)))
      }
    } else {
      y <- u[[var]]
    }
    
    show <- TRUE
    if (!is.null(args$sf)) {
      show <- args$sf[[i]](u)
    }

    if (show) {
      format <- args$format[i]
      digits <- args$digits
      cdigits <- args$cdigits
      na.rm <- args$na.rm

      missingNum <- sum(is.na(y))
      nonmissingNum <- sum(!is.na(y))
      if (na.rm) {
        x <- y[!is.na(y)]
        if (is.null(x)) x <- NA
      } else {
        x <- y
      }

      result <- format

      ShowNodeText <- TRUE

      # check the %var=V% and %node=N% codes
      if (length(grep("%var=([^%]+)%",result))>0) {
        varspec <- sub("(.*)%var=([^%]+)%(.*)","\\2",result)
        if (varspec==varname) {
          if (length(grep("%node=([^%]+)%",result))>0) {
            nodespec <- sub("(.*)%node=([^%]+)%(.*)","\\2",result)
            if (!is.na(value) & (nodespec==value)) {
              ShowNodeText <- TRUE
            } else {
              ShowNodeText <- FALSE
            }            
          } else {
            ShowNodeText <- TRUE
          }
        } else {
          ShowNodeText <- FALSE
        } 
      } else {
        if (length(grep("%node=([^%]+)%",result))>0) {
          nodespec <- sub("(.*)%node=([^%]+)%(.*)","\\2",result)
          if (!is.na(value) & (nodespec==value)) {
            ShowNodeText <- TRUE
          } else {
            ShowNodeText <- FALSE
          }            
        }
      }
      
      y_event <- NULL
      if (length(grep("%pct=([^%]+)%",result))>0) {
        pct_arg <- sub(
          "(.*)%pct=([^%]+)%(.*)","\\2",result)
        y_event <- y==pct_arg
      }
      if (length(grep("%npct=([^%]+)%",result))>0) {
        npct_arg <- sub(
          "(.*)%npct=([^%]+)%(.*)","\\2",result)
        y_event <- y==npct_arg
      }      
      
      if (!args$leaf) {
        if (length(grep("%leafonly%",result))>0) {
          ShowNodeText <- FALSE
        }
      }

      if (args$root) {
        if (length(grep("%noroot%",result))>0) {
          ShowNodeText <- FALSE
        }
      }

      TruncNodeText <- FALSE
      if (length(grep("%trunc=([^%]+)%",result))>0) {
        truncval <- as.numeric(sub("(.*)%trunc=([^%]+)%(.*)","\\2",result))
        TruncNodeText <- TRUE
      }

      # Format %list% output
      tabval <- tableWithoutSort(around(sort(y,na.last=TRUE),digits=cdigits),exclude=NULL)
      countval <- paste0(" (n=",tabval,")")
      countval[tabval==1] <- ""
      listOutput <- paste0(paste0(names(tabval),countval),collapse=", ")
      listLinesOutput <- paste0(paste0(names(tabval),countval),collapse=sepN)

      if (ShowNodeText) {
        if (length(x)==0 || !is.numeric(x)) {
          minx <- maxx <- NA
        } else {
          minx <- min(x)
          maxx <- max(x)
        }
        
        if (ShowFullSummary) {
          result <- paste0("\n",fullsummary(y,digits=cdigits,varname=var))
        }
        
        result <- gsub("%var=[^%]+%","",result)
        result <- gsub("%node=[^%]+%","",result)
        result <- gsub("%trunc=(.+)%","",result)
        result <- gsub("%noroot%","",result)
        result <- gsub("%combo%","",result)
        result <- gsub("%sort%","",result)
        result <- gsub("%leafonly%","",result)
        result <- gsub("%v%",args$var[i],result)
        result <- gsub("%list%",listOutput,result)
        result <- gsub("%listlines%",listLinesOutput,result)
        result <- gsub("%list_%",listLinesOutput,result)
        result <- gsub("%freqpct%",freqfunc(y,digits=digits,sort=SortIt),result)
        result <- gsub("%freq%",freqfunc(y,digits=digits,showp=FALSE,sort=SortIt),result)
        result <- gsub("%freqpctlines%",freqfunc(y,digits=digits,sep="\n",sort=SortIt),result)
        result <- gsub("%freqpct_%",freqfunc(y,digits=digits,sep="\n",sort=SortIt),result)
        result <- gsub("%freqlines%",freqfunc(y,digits=digits,showp=FALSE,sep="\n",sort=SortIt),result)
        result <- gsub("%freq_%",freqfunc(y,digits=digits,showp=FALSE,sep="\n",sort=SortIt),result)
        result <- gsub("%mv%",paste0(missingNum),result)
        result <- gsub("%nonmv%",paste0(nonmissingNum),result)
        if (is.numeric(x) | is.logical(x)) {
          # Note that y is used in the call to nAndpct
          # so that missing values can be handled as desired
          result <- condsub("%npct%",nAndpct(y,digits=digits),result)
          result <- condsub("%pct%",justpct(y,digits=digits),result)
          result <- condsub("%mean%", meanfunc(y,cdigits=cdigits),result)
          result <- condsub("%meanx%", around(mean(x), digits = cdigits),result)
          result <- condsub("%sum%", sumfunc(y,cdigits=cdigits),result)
          result <- condsub("%sumx%", around(sum(x), digits = cdigits),result)
          result <- condsub("%median%", medianfunc(y,cdigits=cdigits),result)
          result <- condsub("%medianx%", around(stats::median(x), digits = cdigits),
              result)          
          result <- condsub("%SD%", SDfunc(y,cdigits=cdigits), result)
          result <- condsub("%SDx%", around(stats::sd(x), digits = cdigits), result)
          result <- condsub("%min%", minfunc(y, cdigits = cdigits), result)
          result <- condsub("%minx%", around(min(x), digits = cdigits), result)
          result <- condsub("%max%", maxfunc(y, cdigits = cdigits), result)
          result <- condsub("%maxx%", around(max(x), digits = cdigits), result)
          result <- condsub("%range%", rangefunc(y,cdigits=cdigits), result)
          result <- condsub("%rangex%", rangefunc(y,cdigits=cdigits,na.rm=TRUE), result)
          result <- condsub("%IQR%", IQRfunc(y,cdigits=cdigits), result)
          result <- condsub("%IQRx%",
            paste0(
              around(qntl(x,0.25), digits = cdigits),", ",
              around(qntl(x,0.75), digits = cdigits)),
            result)
          repeat {
              if (length(grep("%(p)([0-9]+)%", result)) == 0)
                  break
              quant <- sub("(.*)%(p)([0-9]+)%(.*)", "\\3", result)
              if (quant != "") {
                  qq <- around(qntl(x, as.numeric(quant)/100),
                      digits = digits)
                  result <- sub(paste0("%p", quant,"%"), qq, result)
              }
          }
        }
      } else {
        result <- ""
      }
      if (TruncNodeText) {
        if (nchar(result)>truncval) {
          RESULT[i] <- paste0(substr(result,1,truncval),"...")
        } else {
          RESULT[i] <- result
        }
      } else {
        RESULT[i] <- result
      }
    }
  }
  RESULT
}
