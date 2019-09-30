#' @title VennTable
#'
#' @author Nick Barrowman
#'
#' @description
#' Given a pattern table produced by \code{vtree} for indicator (i.e 0/1) variables,
#' \code{VennTable} returns an augmented table.
#' The augmented table includes an extra row with the total for each indicator variable
#' and an extra row with the corresponding percentage
#' (which will not in general add to 100\%).
#' Also, optionally, does some additional formatting for pandoc markdown.
#'  
#' @param x         Required: Pattern table produced by \code{vtree} for indicator (i.e 0/1) variables
#' @param markdown  Format nicely for markdown (see Details).
#' @param NAcode    Code to use to represent NAs in markdown formatting
#' @param checked   Vector of character strings that represent checked values;
#'                  by default: c("1","TRUE","Yes","yes","N/A")
#' @param unchecked Vector of character strings that represent unchecked values;
#'                  by default: c("0","FALSE","No","no","not N/A")
#' 
#' @details
#' The column totals ignore missing values.
#' 
#' When \code{markdown=TRUE}, the row and column headings for percentages are 
#' labeled "\%", indicator values equal to 1 are replaced by checkmark codes,
#' indicator values equal to 0 are replaced by spaces, and missing indicator
#' values are replaced by dashes. Empty headings are replaced by spaces.
#' Finally the table is transposed.
#' 
#' @examples
#' # Generate a pattern table for the indicator variables Ind1 and Ind2
#' ptab <- vtree(FakeData,"Ind1 Ind2",ptable=TRUE)
#' # Augment the table
#' ptab2 <- VennTable(ptab)
#' # Print the result without quotation marks (which are distracting)
#' print(ptab2,quote=FALSE)
#' # Generate a table with pandoc markdown formatting
#' ptab3 <- VennTable(ptab,markdown=TRUE)
#' 
#' @return
#' Returns a character matrix with extra rows containing indicator sums.
#'
#' @export
#'
VennTable <- function(x,markdown=FALSE,NAcode="-",
  unchecked=c("0","FALSE","No","no","not N/A"),checked=c("1","TRUE","Yes","yes","N/A")) {
  
  # {m}ark{d}own {t}able function
  mdt <- function(x) {
    cn <- colnames(x)
    cn[cn==""] <- "&nbsp;"
    rn <- rownames(x)
    rn[rn==""] <- "&nbsp;" 
    x <- cbind(rn,x)
    header <- paste0(cn,collapse="|")
    dashes <- paste0(rep("-",length(cn)+1),collapse="|")
    x[x==""] <- "&nbsp;"
    body <- paste0(apply(x,1,paste0,collapse="|"),collapse="\n")
    paste0("&nbsp;|",header,"\n",dashes,"\n",body,collapse="\n")
  }
  
  mat <- as.matrix(x[,-c(1,2)])
  # Convert logical values to 0's and 1's
  mat[mat %in% unchecked] <- "0"  
  mat[mat %in% checked] <- "1"
  mode(mat) <- "numeric"
  mat <- mat*x$n  # Note that this relies on column recycling
  count <- apply(mat,2,sum,na.rm=TRUE)
  xmat <- as.matrix(x)
  if (markdown) {
    xmat[,-(1:2)][is.na(xmat[,-(1:2)])] <- NAcode
    xmat[,-(1:2)][xmat[,-(1:2)] %in% unchecked] <- ""
    xmat[,-(1:2)][xmat[,-(1:2)] %in% checked] <- "&#10004;"
  }
  xmat <- rbind(xmat,c("","",count))
  xmat <- rbind(xmat,c("","",round(100*count/sum(x[,1]))))
  rownames(xmat) <- c(rep("",nrow(x)),"N","pct")
  
  if (markdown) {
    colnames(xmat)[colnames(xmat)=="pct"] <- "%"
    rownames(xmat)[rownames(xmat)=="pct"] <- "%"
    rownames(xmat)[rownames(xmat)==""] <- "&nbsp;"
    xmat <- t(xmat) 
    mdt(xmat)
  } else {
    xmat
  }
}

