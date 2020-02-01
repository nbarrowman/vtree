#' @importFrom stats median quantile sd

summaryNodeFunction <- function (u, varname, value, args) {

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
    pcs = "%",  showN = FALSE, sep = " ", shown = TRUE, showp = TRUE, 
    nmiss = FALSE, nmiss0 = FALSE, includemiss = TRUE, showzero = FALSE, 
    percentfirst = FALSE, comma = FALSE) {
    x <- w
    if (comma) {
        pStart <- paste0(",", sep)
        pStop <- ""
    }
    else {
        pStart <- paste0(sep, "(")
        pStop <- ")"
    }
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
    if (any(is.na(names(tab)))) 
        names(tab)[is.na(names(tab))] <- "NA"
    result <- ""
    if (shown) {
        pr <- paste(result)
        if (!showzero) 
            pr[pr == "0"] <- ""
        result <- paste(pr, tab, sep = "")
        if (showN) 
            result <- paste0(result, "/", length(x))
        if (showp) 
            result <- paste0(result, pStart, sep = "")
    }
    if (showp) {
        result <- paste(result, around(100 * as.numeric(tab)/sum(tab), 
            digits = digits), pcs, sep = "")
        if (shown) 
            result <- paste(result, pStop, sep = "")
    }
    if (percentfirst & shown & showp) {
        result <- paste(around(100 * as.numeric(tab)/sum(tab), 
            digits = digits), pcs, sep = "")
        result <- paste0(result, pStart, tab)
        if (showN) 
            result <- paste0(result, "/", length(x))
        result <- paste0(result, pStop)
    }
    if (!showzero) result[!is.na(tab) & tab == 0] <- ""
    
    result <- paste(result, nmissString, sep = "")
    names(result) <- names(tab)
    result <- result[names(result) != "NA"]
    
    if (includemiss) {
      if (missingNum>0 | showzero) {
        result["NA"] <- missingNum
      }
    }
    paste0(paste0(names(result),": ",result),collapse="\n")
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

    y <- u[[var]]

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

        result <- gsub("%var=[^%]+%","",result)
        result <- gsub("%node=[^%]+%","",result)
        result <- gsub("%trunc=(.+)%","",result)
        result <- gsub("%noroot%","",result)
        result <- gsub("%leafonly%","",result)
        result <- gsub("%v%",args$var[i],result)
        result <- gsub("%list%",listOutput,result)
        result <- gsub("%listlines%",listLinesOutput,result)
        result <- gsub("%freqpct%",freqfunc(y,digits=digits),result)
        result <- gsub("%freq%",freqfunc(y,digits=digits,showp=FALSE),result)
        result <- gsub("%mv%",paste0(missingNum),result)
        result <- gsub("%nonmv%",paste0(nonmissingNum),result)
        if (is.numeric(x) | is.logical(x)) {
          # Note that y is used in the call to nAndpct
          # so that missing values can be handled as desired
          result <- gsub("%npct%",nAndpct(y,digits=digits),result)
          result <- gsub("%pct%",justpct(y,digits=digits),result)
          result <- gsub("%mean%", around(mean(x), digits = cdigits),result)
          result <- gsub("%sum%", around(sum(x), digits = cdigits),result)
          result <- gsub("%median%", around(stats::median(x), digits = cdigits),
              result)
          result <- gsub("%SD%", around(stats::sd(x), digits = cdigits), result)
          result <- gsub("%min%", around(minx, digits = cdigits), result)
          result <- gsub("%max%", around(maxx, digits = cdigits), result)
          result <- gsub("%IQR%",
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
