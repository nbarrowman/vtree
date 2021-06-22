#' @importFrom stats median quantile sd

summaryNodeFunction <- function (u, varname, value, args) {
  
  # Conditional substitution: whereas gsub evaluates the replacement expression
  # even if x doesn't match pattern, this function only evaluates the 
  # replacement expression if x matches pattern.
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
  
  medianfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%median% : expected a numeric variable.")
    }      
    nMissing <- sum(is.na(w))
    m <- around(stats::median(w,na.rm=TRUE), digits = digits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }
  
  minfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%min% : expected a numeric variable.")
    }      
    nMissing <- sum(is.na(w))
    m <- around(min(w,na.rm=TRUE), digits = digits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }
  
  maxfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%max% : expected a numeric variable.")
    }      
    nMissing <- sum(is.na(w))
    m <- around(max(w,na.rm=TRUE), digits = digits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }  

  IQRfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%IQR% : expected a numeric variable.")
    }      
    nMissing <- sum(is.na(w))
    i <- paste0(
      around(qntl(w,0.25,na.rm=TRUE), digits = digits),", ",
      around(qntl(w,0.75,na.rm=TRUE), digits = digits))
    if (nMissing>0) {
      paste0(i," mv=",nMissing)
    } else {
      i
    }
  }

  rangefunc <- function(w,digits,na.rm=FALSE) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%range% : expected a numeric variable.")
    }      
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
        around(min(w,na.rm=TRUE), digits = digits),", ",
        around(max(w,na.rm=TRUE), digits = digits))
      if (nMissing>0) {
        paste0(r," mv=",nMissing)
      } else {
        r
      }
    }
  }
  
  SDfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%SD% : expected a numeric variable.")
    }      
    nMissing <- sum(is.na(w))
    s <- around(stats::sd(w,na.rm=TRUE), digits = digits)
    if (nMissing>0) {
      paste0(s," mv=",nMissing)
    } else {
      s
    }
  }  

  sumfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%sum% : expected a numeric variable.")
    }      
    nMissing <- sum(is.na(w))
    s <- around(sum(w,na.rm=TRUE), digits = digits)
    if (nMissing>0) {
      paste0(s," mv=",nMissing)
    } else {
      s
    }
  }
  
  meanfunc <- function(w,digits) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%mean% : expected a numeric variable.")
    }    
    nMissing <- sum(is.na(w))
    m <- around(mean(w,na.rm=TRUE), digits = digits)
    if (nMissing>0) {
      paste0(m," mv=",nMissing)
    } else {
      m
    }
  }
    
  justpct <- function(w,digits=2,vp=TRUE,empty="") {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%pct% : expected a logical or 0-1 variable.")
    }    
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
  
  nAndpct <- function(w,digits=2,vp=TRUE,empty="",varname="") {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%npct% : expected a logical or 0-1 variable.")
    }
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
    if (!is.na(varname) & varname!="") npctString <- paste0(varname,": ",npctString)
    npctString
  }
  
  
  freqfunc <- function(w,digits=2,vp=TRUE,empty="",
    pcs = "%",  showN = FALSE, shown = TRUE, showp = TRUE, 
    nmiss = FALSE, nmiss0 = FALSE, includemiss = TRUE, showzero = FALSE, 
    percentfirst = FALSE, sep = ", ",sort=FALSE,varname="",na.rm = FALSE) {
    
    x <- w
    if (na.rm) { x <- x[!is.na(x)] }
 
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
    }
    if (showp) {
        pct <- paste0(around(100 * as.numeric(tab)/sum(tab), digits = digits),pcs)
        if (shown) {
          pct <- paste0(" (",pct,")")
        }
        pct[pct==" (NaN%)"] <- ""
        result <- paste0(result,pct)
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
      if (missingNum>0) { # | showzero) {
        result["NA"] <- missingNum
      }
    }
    
    RESULT <- paste0(paste0(names(result),": ",result),collapse=sep)
    
    if (varname!="") RESULT <- paste0(varname,"\n",RESULT)
    
    RESULT
  }
  

  qntl <- function(w,...) {
    if (!(is.numeric(w) | is.logical(w))) {
      stop("%q% : expected a numeric variable.")
    }      
    if (any(is.na(x))) {
      NA
    } else {
      stats::quantile(x,...)
    }
  }
  
  
  #--- Body of code starts here -----------------------------------------------
  

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
    
    original_var <- args$original_var[i]
    
    ShowSimpleSummary <- TRUE
    Formatstring <- FALSE
    SortIt <- TRUE
    if (args$format[i]=="") {
      SortIt <- TRUE
      FormatString <- FALSE
      ShowSimpleSummary <- TRUE
    }
    # else {
    #  SortIt <- FALSE
    #  FormatString <- TRUE
    #  ShowSimpleSummary <- FALSE
    # }
    
    if (length(grep("%v%"            ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%list%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%listlines%"    ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%list_%"        ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqpct%"      ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqpctx%"    ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freq%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqx%"        ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqpctlines%" ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqpct_%"     ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqpctx_%"    ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqlines%"    ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freq_%"        ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%freqx_%"       ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%mv%"           ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%nonmv%"        ,args$format[i])>0)) ShowSimpleSummary <- FALSE    
    if (length(grep("%npct%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%pct%"          ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%mean%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%meanx%"        ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%sum%"          ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%sumx%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%median%"       ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%medianx%"      ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%SD%"           ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%SDx%"          ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%min%"          ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%minx%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%max%"          ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%maxx%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%range%"        ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%rangex%"       ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%IQR%"          ,args$format[i])>0)) ShowSimpleSummary <- FALSE
    if (length(grep("%IQRx%"         ,args$format[i])>0)) ShowSimpleSummary <- FALSE  
               
    if (length(grep("%combo%",args$format[i]))>0) {
      ShowCombinations <- TRUE
    } else {
      ShowCombinations <- FALSE
    }
    
    if (length(grep("%sort%",args$format[i]))>0) {
      SortIt <- TRUE
    } else
    if (length(grep("%nosort%",args$format[i]))>0) {
      SortIt <- FALSE
    }     
    
    # check if it's a stem
    StemSpecified <- StarSpecified <- HashmarkSpecified <- FALSE
    if (length(grep("\\*$",var))>0) {
      StarSpecified <- TRUE
      ShowSimpleSummary <- FALSE
      thevar <- sub("(\\S+)\\*$","\\1",var)
      expanded_stem <- names(u)[grep(paste0("^",thevar,".*$"),names(u))]
      none <- rep(TRUE,nrow(u))
      if (ShowCombinations) {
        y <- rep("",nrow(u))
        for (j in 1:length(expanded_stem)) {
          y <-
              ifelse(is.na(u[[expanded_stem[j]]]),
                paste0("NA(",expanded_stem[j],")"),
                ifelse(u[[expanded_stem[j]]]==1,
                  ifelse(y=="",expanded_stem[j],paste0(y,"+",expanded_stem[j])),y))
        }
      } else {
        y <- NULL
        for (j in 1:length(expanded_stem)) {
          none <- none & u[[expanded_stem[j]]]==0
          y <- c(y,rep(expanded_stem[j],sum(u[[expanded_stem[j]]],na.rm=TRUE)))
          y <- c(y,rep(paste0("NA(",expanded_stem[j],")"),sum(is.na(u[[expanded_stem[j]]]))))
        }
      }
      if (ShowCombinations) {
        y[y %in% ""] <- "*None"
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
      tabval <- tableWithoutSort(around(sort(y,na.last=TRUE),digits=digits),exclude=NULL)
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
        
        if (ShowSimpleSummary) {
          if (is.numeric(y) && length(unique(y))>3) {
            result <- paste0("\n",fullsummary(y,digits=cdigits,varname=var))
          } else
          if (is.logical(y) || (is.numeric(y) && (all(unique(y) %in% c(NA,0,1))))) {
            result <- paste0("\n",nAndpct(y,digits=digits,varname=original_var))
          } else {
            result <- paste0("\n",freqfunc(y,digits=digits,sep="\n",sort=SortIt,varname=original_var,showzero=TRUE))
          }
        } else
        if (StemSpecified && !FormatString) {
          result <- paste0("\n",freqfunc(y,digits=digits,sort=SortIt,sep="\n",showp=FALSE))
        } else {
          result <- gsub("%var=[^%]+%","",result)
          result <- gsub("%node=[^%]+%","",result)
          result <- gsub("%trunc=(.+)%","",result)
          result <- gsub("%noroot%","",result)
          result <- gsub("%combo%","",result)
          result <- gsub("%sort%","",result)
          result <- gsub("%nosort%","",result)
          result <- gsub("%leafonly%","",result)
          result <- gsub("%v%",args$var[i],result)
          result <- gsub("%list%",listOutput,result)
          result <- gsub("%listlines%",listLinesOutput,result)
          result <- gsub("%list_%",listLinesOutput,result)
          result <- gsub("%freqpct%",freqfunc(y,digits=digits,sort=SortIt),result)
          result <- gsub("%freqpctx%",freqfunc(y,digits=digits,sort=SortIt,na.rm=TRUE),result)
          result <- gsub("%freq%",freqfunc(y,digits=digits,showp=FALSE,sort=SortIt),result)
          result <- gsub("%freqx%",freqfunc(y,digits=digits,showp=FALSE,sort=SortIt,na.rm=TRUE),result)
          result <- gsub("%freqpctlines%",freqfunc(y,digits=digits,sep="\n",sort=SortIt),result)
          result <- gsub("%freqpct_%",freqfunc(y,digits=digits,sep="\n",sort=SortIt),result)
          result <- gsub("%freqpctx_%",freqfunc(y,digits=digits,sep="\n",sort=SortIt,na.rm=TRUE),result)
          result <- gsub("%freqlines%",freqfunc(y,digits=digits,showp=FALSE,sep="\n",sort=SortIt),result)
          result <- gsub("%freq_%",freqfunc(y,digits=digits,showp=FALSE,sep="\n",sort=SortIt),result)
          result <- gsub("%freqx_%",freqfunc(y,digits=digits,showp=FALSE,sep="\n",sort=SortIt,na.rm=TRUE),result)
          result <- gsub("%mv%",paste0(missingNum),result)
          result <- gsub("%nonmv%",paste0(nonmissingNum),result)
          
          result <- condsub("%npct%",nAndpct(y,digits=digits),result)
          result <- condsub("%pct%",justpct(y,digits=digits),result)
          result <- condsub("%mean%", meanfunc(y,digits=cdigits),result)
          result <- condsub("%meanx%", around(mean(x), digits = cdigits),result)
          result <- condsub("%sum%", sumfunc(y,digits=cdigits),result)
          result <- condsub("%sumx%", around(sum(x), digits = cdigits),result)
          result <- condsub("%median%", medianfunc(y,digits=cdigits),result)
          result <- condsub("%medianx%", around(stats::median(x), digits = cdigits),
            result)          
          result <- condsub("%SD%", SDfunc(y,digits=cdigits), result)
          result <- condsub("%SDx%", around(stats::sd(x), digits = cdigits), result)
          result <- condsub("%min%", minfunc(y, digits = cdigits), result)
          result <- condsub("%minx%", around(min(x), digits = cdigits), result)
          result <- condsub("%max%", maxfunc(y, digits = cdigits), result)
          result <- condsub("%maxx%", around(max(x), digits = cdigits), result)
          result <- condsub("%range%", rangefunc(y,digits=cdigits), result)
          result <- condsub("%rangex%", rangefunc(y,digits=cdigits,na.rm=TRUE), result)
          result <- condsub("%IQR%", IQRfunc(y,digits=cdigits), result)
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
