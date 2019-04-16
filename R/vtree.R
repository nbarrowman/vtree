#'
#' vtree: Draw a variable tree
#'
#' @description
#' vtree is a tool for drawing variable trees.
#' Variable trees display information about nested subsets of a data frame,
#' in which the subsetting is defined by the values of categorical variables.
#'
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#'
#' @param z                Required: Data frame, or a single vector.
#' @param vars             Required (unless \code{z} is a vector):
#'                         Either a character string of whitespace-separated variable names
#'                         or a vector of variable names.
#' @param splitspaces      When \code{vars} is a character string,
#'                         split it by spaces to get variable names?
#'                         It is only rarely necessary to use this parameter.
#'                         This should only be \code{FALSE} when a single variable name
#'                         that contains spaces is specified.
#' @param horiz            Should the tree be drawn horizontally?
#'                         (i.e. parent node on the left, with the tree growing to the right)
#' @param labelnode        List of vectors used to change how values of variables are displayed.
#'                         The name of each element of the
#'                         list is one of the variable names in \code{vars}.
#'                         Each element of the list is a vector of character strings,
#'                         representing the values of the variable.
#'                         The names of the vector represent the labels to be used in place of the values.
#' @param tlabelnode       A list of vectors, each of which specifies a particular node,
#'                         as well as a label for that node (a "targeted" label).
#'                         The names of each vector specify variable names,
#'                         except for an element named \code{label}, which specifies the label to use.
#' @param labelvar         A named vector of labels for variables.
#' @param varminwidth      A named vector of minimum initial widths for nodes of each variable.
#'                         (Sets the Graphviz \code{width} attribute.)
#' @param varminheight     A named vector of minimum initial heights for nodes of each variable.
#'                         (Sets the Graphviz \code{height} attribute.)
#' @param varlabelloc      A named vector of vertical label locations
#'                         ("t", "c", or "b" for top, center, or bottom, respectively)
#'                         for nodes of each variable.
#'                         (Sets the Graphviz \code{labelloc} attribute.)
#' @param title            Optional title for the root node of the tree.
#' @param shownodelabels   Show node labels?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} values for named variables is interpreted as
#'                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} values for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param showvarnames     Show the name of the variable next to each level of the tree?
#' @param showlevels       (Deprecated) Same as showvarnames.
#' @param varnamepointsize Font size (in points) to use when displaying variable names.
#' @param prune            List of vectors that specifies nodes to prune.
#'                         The name of each element of the
#'                         list must be one of the variable names in \code{vars}.
#'                         Each element is a vector of character strings that
#'                         identifies the values of the variable (i.e. the nodes) to prune.
#' @param prunebelow       Like \code{prune} but instead of pruning the specified nodes,
#'                         their descendants are pruned.
#' @param keep             Like \code{prune} but specifies which nodes to \emph{keep}.
#'                         The other nodes will be pruned.
#' @param follow           Like \code{keep} but specifies which nodes to "follow",
#'                         i.e. which nodes' \emph{descendants} to keep.
#' @param prunelone        A vector of values specifying "lone nodes" (of \emph{any} variable) to prune.
#'                         A lone node is a node that has no siblings.
#' @param pruneNA          Prune all missing values?
#'                         This should be used carefully because "valid" percentages
#'                         are hard to interpret when NAs are pruned.
#' @param sameline         Display node labels on the same line as the count and percentage?
#' @param gradient         Use gradients of fill color across the values of each variable?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} values for named variables is interpreted as
#'                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} values for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param revgradient      Should the gradient be reversed (i.e. dark to light instead of light to dark)?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} values for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} values for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param colorvarlabels   Color the variable labels?
#' @param check.is.na      Replace each variable named in \code{vars} with a logical vector indicating
#'                         whether or not each of its values is missing?
#' @param summary          A character string used to specify summary statistics to display in the nodes.
#'                         The first word in the character string is the name of the variable to be summarized.
#'                         The rest of the character string is the text that will be displayed,
#'                         along with special codes specifying the information to display
#'                         (see \strong{Summary codes} below).
#'                         A vector of character strings can also be specified,
#'                         if more than one variable is to be summarized.
#' @param runsummary       A list of functions, with the same length as \code{summary}.
#'                         Each function must take a data frame as its sole argument,
#'                         and return a logical value.
#'                         Each string in \code{summary} will only be interpreted if
#'                         the corresponding logical value is \code{TRUE}.
#'                         the corresponding string in \code{summary} will be evaluated.
#' @param retain           Vector of names of additional variables in the data frame that need to be
#'                         available to execute the functions in \code{runsummary}.
#' @param fillnodes        Fill the nodes with color?
#' @param fillcolor        A named vector of colors for filling the nodes of each variable.
#'                         If an unnamed, scalar color is specified,
#'                         all nodes will have this color.
#' @param NAfillcolor      Fill-color for missing-value nodes.
#'                         If \code{NULL}, fill colors of missing value nodes will be consistent
#'                         with the fill colors in the rest of the tree.
#' @param rootfillcolor    Fill-color for the root node.
#' @param text             A list of vectors containing extra text to add to
#'                         nodes corresponding to specified values of a specified variable.
#'                         The name of each element of the list
#'                         must be one of the variable names in \code{vars}.
#'                         Each element is a vector of character strings.
#'                         The names of the vector identify the nodes to which the text should be added.
#'                         (See \strong{Formatting codes} below for information
#'                         on how to format text.)
#' @param ttext            A list of vectors, each of which specifies a particular node,
#'                         as well as text to add to that node ("targeted" text).
#'                         The names of each vector specify variable names,
#'                         except for an element named \code{text}, which specifies the text to add.
#' @param HTMLtext         Is the text formatted in HTML?
#' @param splitwidth       The minimum number of characters before an automatic
#'                         linebreak is inserted.
#' @param lsplitwidth      In legends, the minimum number of characters before an automatic
#'                         linebreak is inserted.
#' @param nodesep          Graphviz attribute: Node separation amount.
#' @param ranksep          Graphviz attribute: Rank separation amount.
#' @param margin           Graphviz attribute: node margin.
#' @param vp               Use "valid percentages"?
#'                         Valid percentages are computed by first excluding any missing values,
#'                         i.e. restricting attention to the set of "valid" observations.
#'                         The denominator is thus the number of non-missing observations.
#'                         When \code{vp=TRUE}, nodes for missing values show the number of missing values
#'                         but do not show a percentage;
#'                         all the other nodes how valid percentages.
#'                         When \code{vp=FALSE}, all nodes (including nodes for missing values)
#'                         show percentages of the total number of observations.
#' @param nodefunc         A node function (see \strong{Node functions} below).
#' @param nodeargs         A list containing named arguments for the node function
#'                         specified by \code{nodefunc}.
#' @param rounded          Use rounded boxes for nodes?
#' @param getscript        Instead of displaying the variable tree,
#'                         return the DOT script as a character string?
#' @param showempty        Show nodes that do not contain any observations?
#' @param digits           Number of decimal digits to show in percentages.
#' @param cdigits          Number of decimal digits to show in continuous values displayed via the summary parameter.
#' @param color            A vector of color names for the \emph{outline} of the nodes at each level.
#' @param colornodes       Color the node outlines?
#' @param width            Width (in pixels) to be passed to \code{DiagrammeR::grViz}.
#' @param height           Height (in pixels) to be passed to \code{DiagrammeR::grViz}.
#' @param squeeze          The degree (between 0 and 1) to which the tree will be "squeezed".
#'                         This controls two Graphviz parameters: \code{margin} and \code{nodesep}.
#' @param plain            Use "plain" settings?
#'                         These settings are as follows: for each variable all nodes are the same color,
#'                         namely a shade of blue (with each successive variable using a darker shade);
#'                         all variable labels are black; and the \code{squeeze} parameter is set to 0.6.
#' @param showpct          Show percentage in each node?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} for named variables is interpreted as
#'                         \code{FALSE} for those variables and TRUE for all others.
#' @param showcount        Show count in each node?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param showlegend       Show legend (including marginal frequencies) for each variable?
#' @param showlpct         Show percentages (for the marginal frequencies) in the legend?
#' @param graphattr        Character string: Additional attributes for the Graphviz graph.
#' @param nodeattr         Character string: Additional attributes for Graphviz nodes.
#' @param edgeattr         Character string: Additional attributes for Graphviz edges.
#' @param seq              Display the variable tree using "sequences"?
#'                         Each unique sequence (i.e. pattern) of values will be shown separately.
#'                         The sequences are sorted from least frequent to most frequent.
#' @param pattern          Same as \code{seq}, but lines without arrows are drawn,
#'                         and instead of a \code{sequence} variable, a \code{pattern} variable is shown.
#' @param showroot         Show the root node?
#'                         When \code{seq=TRUE}, it may be useful to set \code{showroot=FALSE}.
#' @param Venn             Display multi-way set membership information?
#'                         This provides an alternative to a Venn diagram.
#'                         This sets \code{showpct=FALSE} and \code{shownodelabels=FALSE}.
#'                         Assumption: all of the specified variables are logicals or 0/1 numeric variables.
#' @param palette          A vector of palette numbers (which can range between 1 and 9).
#'                         The names of the vector indicate the corresponding variable.
#'                         See \strong{Palettes} below for more information.
#' @param singlecolor      When a variable has a single value,
#'                         this parameter is used to specify whether nodes should have a
#'                         (1) light shade, (2) a medium shade, or (3) a dark shade.
#'                         specify \code{singlecolor=1} to assign a light shade.
#' @param choicechecklist  When REDCap checklists are specified using the \code{stem:} syntax,
#'                         automatically extract the names of choices and use them as variable names? 
#' @param parent           Parent node number (Internal use only.)
#' @param last             Last node number (Internal use only.)
#' @param root             Is this the root node of the tree? (Internal use only.)
#'
#' @section Summary codes:
#' \itemize{
#'  \item{\code{\%mean\%} }{mean}
#'  \item{\code{\%SD\%} }{standard deviation}
#'  \item{\code{\%min\%} }{minimum}
#'  \item{\code{\%max\%} }{maximum}
#'  \item{\code{\%pX\%} }{Xth percentile, e.g. p50 means the 50th percentile}
#'  \item{\code{\%median\%} }{median, i.e. p50}
#'  \item{\code{\%IQR\%} }{interquartile range, i.e. p25, p75}
#'  \item{\code{\%npct\%} }{number and percentage of \code{TRUE} values}
#'  \item{\code{\%list\%} }{list of the individual values}
#'  \item{\code{\%mv\%} }{the number of missing values}
#'  \item{\code{\%v\%} }{the name of the variable}
#'  \item{\code{\%noroot\%} }{flag: Do not show summary in the root node.}
#'  \item{\code{\%leafonly\%} }{flag: Only show summary in leaf nodes.}
#'  \item{\code{\%var=}V\code{\%} }{flag: Only show summary in nodes of variable V.}
#'  \item{\code{\%node=}N\code{\%} }{flag: Only show summary in nodes with value N.}
#'  \item{\code{\%trunc=}n\code{\%} }{flag: Truncate the summary to the first n characters.}
#' }
#'
#' @section Node functions:
#' Node functions provide a mechanism for running a function within each subset
#' representing a node of the tree. The \code{summary} parameter uses node functions.
#' A node functions is a function takes as arguments a data frame subset,
#' the name of the subsetting variable, the value of the subsetting variable, and
#' a list of named arguments.
#'
#' @section Formatting codes:
#' Formatting codes for the \code{text} argument.
#' Also used by \code{labelnode} and \code{labelvar}.
#' \itemize{
#'  \item{\code{\\n} }{line break}
#'  \item{\code{*...*} }{italics}
#'  \item{\code{**...**} }{bold}
#'  \item{\code{^...^} }{superscript (using 10 point font)}
#'  \item{\code{~...~} }{subscript (using 10 point font)}
#'  \item{\code{\%\%red ...\%\%} }{display text in red (or whichever color is specified)}
#' }
#'
#' @section Palettes:
#' Sequential palettes from Color Brewer:
#' \enumerate{
#'  \item{Reds}
#'  \item{Blues}
#'  \item{Greens}
#'  \item{Oranges}
#'  \item{Purples}
#'  \item{YlGn}
#'  \item{PuBu}
#'  \item{PuRd}
#'  \item{YlOrBr}
#' }
#'
#' @return
#' If \code{getscript=TRUE}, returns a character string of DOT script that describes the variable tree.
#' If \code{getscript=FALSE}, returns an object of class \code{htmlwidget}
#' that will intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents, and within Shiny output bindings.
#'
#' @examples
#' # A single-level hierarchy
#' vtree(FakeData,"Severity")
#'
#' # A two-level hierarchy
#' vtree(FakeData,"Severity Sex")
#'
#' # A two-level hierarchy with pruning of some values of Severity
#' vtree(FakeData,"Severity Sex",prune=list("Severity"=c("Moderate","NA")))
#'
#' # Rename some nodes
#' vtree(FakeData,"Severity Sex",labelnode=list(Sex=(c("Male"="M","Female"="F"))))
#'
#' # Rename a variable
#' vtree(FakeData,"Severity Sex",labelvar=c(Severity="How bad?"))
#'
#' # Show legend. Put labels on the same line as counts and percentages
#' vtree(FakeData,"Severity Sex Viral",sameline=TRUE,showlegend=TRUE)
#'
#' # Using the summary parameter to list ID numbers (truncated to 40 characters) in specified nodes
#' vtree(FakeData,"Severity Sex",summary="id \nid = %list% %var=Severity% %trunc=40%")
#'
#' # Adding text to specified nodes of a tree
#' vtree(FakeData,"Severity Sex",ttext=list(
#'   c(Severity="Severe",Sex="M",text="Males with Severe disease"),
#'   c(Severity="NA",text="Unknown severity")))
#'
#' @export

vtree <- function (z, vars, splitspaces=TRUE,
  prune=list(), prunebelow = list(), keep=list(), follow=list(), prunelone=NULL,pruneNA=FALSE,
  labelnode = list(),tlabelnode=NULL,labelvar = NULL,
  varminwidth=NULL,varminheight=NULL,varlabelloc=NULL,
  fillcolor = NULL, fillnodes = TRUE,
  NAfillcolor="white",rootfillcolor="#EFF3FF",
  palette=NULL,
  gradient=TRUE, revgradient=FALSE,
  singlecolor=2,
  colorvarlabels=TRUE,
  title = "",
  sameline=FALSE,
  Venn = FALSE, check.is.na = FALSE,
  seq=FALSE, pattern=FALSE, showroot=TRUE,
  text = list(),ttext=list(),
  plain = FALSE, squeeze = 1,
  shownodelabels=TRUE,
  showvarnames = TRUE, showlevels = TRUE,
  showpct=TRUE, showlpct=TRUE,
  showcount=TRUE, showlegend=FALSE,
  varnamepointsize = 18,
  HTMLtext = FALSE,
  digits = 0,cdigits=1,
  splitwidth = 20, lsplitwidth=15,
  getscript = FALSE,
  nodesep = 0.5, ranksep = 0.5, margin=0.2, vp = TRUE,
  horiz = TRUE, summary = "", runsummary = NULL, retain=NULL,
  width=NULL,height=NULL,
  graphattr="",nodeattr="",edgeattr="",
  color = c("blue", "forestgreen", "red", "orange", "pink"), colornodes = FALSE,
  showempty = FALSE, rounded = TRUE,
  nodefunc = NULL, nodeargs = NULL, 
  choicechecklist = TRUE,
  parent = 1, last = 1, root = TRUE)
{

  makeHTML <- function(x) {
    if (is.list(x)) {
      lapply(x, convertToHTML)
    }
    else {
      convertToHTML(x)
    }
  }
  makeHTMLnames <- function(x) {
    if (is.list(x)) {
      x <- lapply(x,
        function(u) {
          names(u) <- convertToHTML(names(u))
          u
        })
    }
    else {
      names(x) <- convertToHTML(names(x))
    }
    x
  }


  ### ----------- Begin code for root only ------------

  if (root) {

    unknowncolor <- "pink"

    argname <- sapply(as.list(substitute({z})[-1]), deparse)

    if (!missing(vars) && length(vars)==1 && splitspaces) {
      vars <- strsplit(vars,"\\s+")[[1]]
      # In case the first element is empty
      # (due to whitespace at the beginning of the string)
      if (vars[1]=="") vars <- vars[-1]
    }

    # Special case where z is provided as a vector instead of a data frame
    if (!is.data.frame(z)) {
        z <- data.frame(z)
        if (!missing(vars))
            argname <- vars
        colnames(z)[1] <- argname
        vars <- argname
    }
    
    # Process tri: tag in variable names 
   findtri <- grep("tri:",vars)
    if (length(findtri)>0) {
      for (i in 1:length(vars)) {    
        if (i %in% findtri) {
          trivar <- sub("^tri:([^ ]+)$","\\1",vars[i])
          med <- median(z[[trivar]],na.rm=TRUE)
          iqrange <- 
            quantile(z[[trivar]],0.75,na.rm=TRUE)-
            quantile(z[[trivar]],0.25,na.rm=TRUE)
          upper <- med+1.5*iqrange
          lower <- med-1.5*iqrange
          m <- ifelse(z[[trivar]]<lower,"lo",
                ifelse(z[[trivar]]>=lower & z[[trivar]]<upper,"mid",
                  ifelse(z[[trivar]]>=upper,"hi","impossible")))
          trivar_name <- paste0("tri:",trivar)
          z[[trivar_name]] <- factor(m)
          vars[i] <- trivar_name
        }
      }
    }    
    
    
    
    
    
    
    # Process = tag in variable names 
    findequal <- grep("=",vars)
    if (length(findequal)>0) {
      for (i in 1:length(vars)) {    
        if (i %in% findequal) {
          equalvar <- sub("([^ ]+)(=)([^ ]+)","\\1",vars[i])
          equalval <- sub("([^ ]+)(=)([^ ]+)","\\3",vars[i])
          # Check to see if any of the values of the specified variable contain spaces
          # If they do, replace underscores in the specified value with spaces.
          if (any(length(grep(" ",names(table(z[[equalvar]]))))>0)) {
            equalval <- gsub("_"," ",equalval)
          }
          m <- z[[equalvar]]==equalval
          z[[equalvar]] <- factor(m, levels = c(FALSE, TRUE),
            c(paste0("Not ",equalval),paste0(equalval)))
          vars[i] <- equalvar
        }
      }
    }
    
    # Process > tag in varible names
    findgt <- grep(">",vars)
    if (length(findgt)>0) {
      for (i in 1:length(vars)) {    
        if (i %in% findgt) {
          gtvar <- sub("([^ ]+)(>)([^ ]+)","\\1",vars[i])
          gtval <- sub("([^ ]+)(>)([^ ]+)","\\3",vars[i])
          # Check to see if any of the values of the specified variable contain spaces
          # If they do, replace underscores in the specified value with spaces.
          if (any(length(grep(" ",names(table(z[[gtvar]]))))>0)) {
            gtval <- gsub("_"," ",gtval)
          }
          m <- z[[gtvar]]>as.numeric(gtval)
          z[[gtvar]] <- factor(m, levels = c(FALSE, TRUE),
            c(paste0("<=",gtval),paste0(">",gtval)))
          vars[i] <- gtvar
        }
      }
    }    
    
    # Process < tag in varible names
    findlt <- grep("<",vars)
    if (length(findlt)>0) {
      for (i in 1:length(vars)) {    
        if (i %in% findlt) {
          ltvar <- sub("([^ ]+)(<)([^ ]+)","\\1",vars[i])
          ltval <- sub("([^ ]+)(<)([^ ]+)","\\3",vars[i])
          # Check to see if any of the values of the specified variable contain spaces
          # If they do, replace underscores in the specified value with spaces.
          if (any(length(grep(" ",names(table(z[[ltvar]]))))>0)) {
            ltval <- gsub("_"," ",ltval)
          }
          m <- z[[ltvar]]<as.numeric(ltval)
          z[[ltvar]] <- factor(m, levels = c(FALSE, TRUE),
            c(paste0(">=",ltval),paste0("<",ltval)))
          vars[i] <- ltvar
        }
      }
    }        
    
    # Process is.na: tag in variable names to handle individual missing value checks
    findna <- grep("^is\\.na:",vars)
    if (length(findna)>0) {
      for (i in 1:length(vars)) {
        if (i %in% findna) {
          navar <- sub("^is\\.na:([^ ]+)$","\\1",vars[i])
#          newvar <- paste0("MISSING_", navar)
          m <- is.na(z[[navar]])
          z[[navar]] <- factor(m, levels = c(FALSE, TRUE),c("available","N/A"))
          # Note that available comes before N/A in alphabetical sorting.
          # Similarly FALSE comes before TRUE.
          # And 0 (representing FALSE) comes before 1 (representing TRUE) numerically.
          # This is convenient, especially when when using the seq parameter.
          vars[i] <- navar
        }
      }
    }
    
    # Process stem: tag in variable names to handle REDCap checklists automatically
    findstem <- grep("^stem:",vars)
    if (length(findstem)>0) {
      expandedvars <- c()
      for (i in 1:length(vars)) {
        if (i %in% findstem) {
          stem <- sub("^stem:([^ ]+)$","\\1",vars[i])
          expanded_stem <- names(z)[grep(paste0("^",stem,"___[0-9]+$"),names(z))]
          if (length(expanded_stem)==0) {
            stop(paste0("Could not find variables with names matching the specified stem: ",stem))
          }
          if (choicechecklist) {
            for (j in 1:length(expanded_stem)) {
              choice <- sub(".+\\(choice=(.+)\\)","\\1",attributes(z[[expanded_stem[j]]])$label)
              z[[choice]] <- z[[expanded_stem[j]]]
              expandedvars <- c(expandedvars,choice)
            }
          } else {
            expandedvars <- c(expandedvars,expanded_stem)
          }
        } else {
          expandedvars <- c(expandedvars,vars[i])
        }
      }
      vars <- expandedvars
    }

    if (!missing(showlevels)) showvarnames <- showlevels

    allvars <- vars

    # Set up summaries if requested
    if (!all(summary=="")) {
      codevar <- gsub("^([^ ]+) (.+)$", "\\1", summary)
      if (!all(codevar %in% names(z))) {
        nomatch <- codevar[!(codevar %in% names(z))]
        stop("Variable(s) specified in summary argument not in data frame: ",paste(nomatch,collapse=", "))
      }
      if (!is.null(runsummary)) {
        if (length(runsummary) != length(summary)) {
          stop("runsummary argument is not the same length as summary argument.")
        }
      }
      codecode <- gsub("^([^ ]+) (.+)$", "\\2", summary)
      nodefunc <- summaryNodeFunction
      nodeargs <- list(var = codevar, format = codecode, sf = runsummary, digits = digits, cdigits = cdigits)
      allvars <- c(allvars,codevar)
    }

    # Add any extra variables needed
    allvars <- c(allvars,retain)

    numvars <- length(vars)

    col <- list(
      rbind(
        "#DE2D26",
        "#3182BD",
        "#31A354",
        "#E6550D",
        "#756BB1",
        "#31A354",
        "#2B8CBE",
        "#DD1C77",
        "#D95F0E",
        "#1C9099",
        "#8856A7",
        "#F03B20"
      ),
      rbind(
        c("#FEE0D2","#DE2D26"),
        c("#DEEBF7","#3182BD"),
        c("#E5F5E0","#31A354"),
        c("#FEE6CE","#E6550D"),
        c("#EFEDF5","#756BB1"),
        c("#F7FCB9","#31A354"),
        c("#ECE7F2","#2B8CBE"),
        c("#E7E1EF","#DD1C77"),
        c("#FFF7BC","#D95F0E"),
        c("#ECE2F0","#1C9099"),
        c("#E0ECF4","#8856A7"),
        c("#FFEDA0","#F03B20")
      ),
      rbind(
        c("#FEE0D2","#FC9272","#DE2D26"),
        c("#DEEBF7","#9ECAE1","#3182BD"),
        c("#E5F5E0","#A1D99B","#31A354"),
        c("#FEE6CE","#FDAE6B","#E6550D"),
        c("#EFEDF5","#BCBDDC","#756BB1"),
        c("#F7FCB9","#ADDD8E","#31A354"),
        c("#ECE7F2","#A6BDDB","#2B8CBE"),
        c("#E7E1EF","#C994C7","#DD1C77"),
        c("#FFF7BC","#FEC44F","#D95F0E"),
        c("#ECE2F0","#A6BDDB","#1C9099"),
        c("#E0ECF4","#9EBCDA","#8856A7"),
        c("#FFEDA0","#FEB24C","#F03B20")
      ),
      rbind(
        c("#FEE5D9","#FCAE91","#FB6A4A","#CB181D"),
        c("#EFF3FF","#BDD7E7","#6BAED6","#2171B5"),
        c("#EDF8E9","#BAE4B3","#74C476","#238B45"),
        c("#FEEDDE","#FDBE85","#FD8D3C","#D94701"),
        c("#F2F0F7","#CBC9E2","#9E9AC8","#6A51A3"),
        c("#FFFFCC","#C2E699","#78C679","#238443"),
        c("#F1EEF6","#BDC9E1","#74A9CF","#0570B0"),
        c("#F1EEF6","#D7B5D8","#DF65B0","#CE1256"),
        c("#FFFFD4","#FED98E","#FE9929","#CC4C02"),
        c("#F6EFF7","#BDC9E1","#67A9CF","#02818A"),
        c("#EDF8FB","#B3CDE3","#8C96C6","#88419D"),
        c("#FFFFB2","#FECC5C","#FD8D3C","#E31A1C")
      ),
      rbind(
        c("#FEE5D9","#FCAE91","#FB6A4A","#DE2D26","#A50F15"),
        c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C"),
        c("#EDF8E9","#BAE4B3","#74C476","#31A354","#006D2C"),
        c("#FEEDDE","#FDBE85","#FD8D3C","#E6550D","#A63603"),
        c("#F2F0F7","#CBC9E2","#9E9AC8","#756BB1","#54278F"),
        c("#FFFFCC","#C2E699","#78C679","#31A354","#006837"),
        c("#F1EEF6","#BDC9E1","#74A9CF","#2B8CBE","#045A8D"),
        c("#F1EEF6","#D7B5D8","#DF65B0","#DD1C77","#980043"),
        c("#FFFFD4","#FED98E","#FE9929","#D95F0E","#993404"),
        c("#F6EFF7","#BDC9E1","#67A9CF","#1C9099","#016C59"),
        c("#EDF8FB","#B3CDE3","#8C96C6","#8856A7","#810F7C"),
        c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026")
      ),
      rbind(
        c("#FEE5D9","#FCBBA1","#FC9272","#FB6A4A","#DE2D26","#A50F15"),
        c("#EFF3FF","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C"),
        c("#EDF8E9","#C7E9C0","#A1D99B","#74C476","#31A354","#006D2C"),
        c("#FEEDDE","#FDD0A2","#FDAE6B","#FD8D3C","#E6550D","#A63603"),
        c("#F2F0F7","#DADAEB","#BCBDDC","#9E9AC8","#756BB1","#54278F"),
        c("#FFFFCC","#D9F0A3","#ADDD8E","#78C679","#31A354","#006837"),
        c("#F1EEF6","#D0D1E6","#A6BDDB","#74A9CF","#2B8CBE","#045A8D"),
        c("#F1EEF6","#D4B9DA","#C994C7","#DF65B0","#DD1C77","#980043"),
        c("#FFFFD4","#FEE391","#FEC44F","#FE9929","#D95F0E","#993404"),
        c("#F6EFF7","#D0D1E6","#A6BDDB","#67A9CF","#1C9099","#016C59"),
        c("#EDF8FB","#BFD3E6","#9EBCDA","#8C96C6","#8856A7","#810F7C"),
        c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#F03B20","#BD0026")
      ),
      rbind(
        c("#FEE5D9","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#99000D"),
        c("#EFF3FF","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594"),
        c("#EDF8E9","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#005A32"),
        c("#FEEDDE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801","#8C2D04"),
        c("#F2F0F7","#DADAEB","#BCBDDC","#9E9AC8","#807DBA","#6A51A3","#4A1486"),
        c("#FFFFCC","#D9F0A3","#ADDD8E","#78C679","#41AB5D","#238443","#005A32"),
        c("#F1EEF6","#D0D1E6","#A6BDDB","#74A9CF","#3690C0","#0570B0","#034E7B"),
        c("#F1EEF6","#D4B9DA","#C994C7","#DF65B0","#E7298A","#CE1256","#91003F"),
        c("#FFFFD4","#FEE391","#FEC44F","#FE9929","#EC7014","#CC4C02","#8C2D04"),
        c("#F6EFF7","#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A","#016450"),
        c("#EDF8FB","#BFD3E6","#9EBCDA","#8C96C6","#8C6BB1","#88419D","#6E016B"),
        c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026")
      ),
      rbind(
        c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#99000D"),
        c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594"),
        c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#005A32"),
        c("#FFF5EB","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801","#8C2D04"),
        c("#FCFBFD","#EFEDF5","#DADAEB","#BCBDDC","#9E9AC8","#807DBA","#6A51A3","#4A1486"),
        c("#FFFFE5","#F7FCB9","#D9F0A3","#ADDD8E","#78C679","#41AB5D","#238443","#005A32"),
        c("#FFF7FB","#ECE7F2","#D0D1E6","#A6BDDB","#74A9CF","#3690C0","#0570B0","#034E7B"),
        c("#F7F4F9","#E7E1EF","#D4B9DA","#C994C7","#DF65B0","#E7298A","#CE1256","#91003F"),
        c("#FFFFE5","#FFF7BC","#FEE391","#FEC44F","#FE9929","#EC7014","#CC4C02","#8C2D04"),
        c("#FFF7FB","#ECE2F0","#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A","#016450"),
        c("#F7FCFD","#E0ECF4","#BFD3E6","#9EBCDA","#8C96C6","#8C6BB1","#88419D","#6E016B"),
        c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026")
      ),
      rbind(
        c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D"),
        c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#08519C","#08306B"),
        c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B"),
        c("#FFF5EB","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801","#A63603","#7F2704"),
        c("#FCFBFD","#EFEDF5","#DADAEB","#BCBDDC","#9E9AC8","#807DBA","#6A51A3","#54278F","#3F007D"),
        c("#FFFFE5","#F7FCB9","#D9F0A3","#ADDD8E","#78C679","#41AB5D","#238443","#006837","#004529"),
        c("#FFF7FB","#ECE7F2","#D0D1E6","#A6BDDB","#74A9CF","#3690C0","#0570B0","#045A8D","#023858"),
        c("#F7F4F9","#E7E1EF","#D4B9DA","#C994C7","#DF65B0","#E7298A","#CE1256","#980043","#67001F"),
        c("#FFFFE5","#FFF7BC","#FEE391","#FEC44F","#FE9929","#EC7014","#CC4C02","#993404","#662506"),
        c("#FFF7FB","#ECE2F0","#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A","#016C59","#014636"),
        c("#F7FCFD","#E0ECF4","#BFD3E6","#9EBCDA","#8C96C6","#8C6BB1","#88419D","#810F7C","#4D004B"),
        c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026")
    ))
    
    # Duplicate the color gradients 3 times to allow for huge trees.
    for (i in 1:length(col)) {
      col[[i]] <- rbind(col[[i]],col[[i]],col[[i]])
    }

    # When a variable has a single value,
    # should nodes be colored light (1) medium (2) or dark (3)?
    if (singlecolor==1) { col[[1]] <- col[[3]][,1,drop=FALSE] }
    if (singlecolor==2) { col[[1]] <- col[[3]][,2,drop=FALSE] }
    if (singlecolor==3) { col[[1]] <- col[[3]][,3,drop=FALSE] }

    # Check that all of named variables are in the data frame
    findallvars <- allvars %in% names(z)
    if (any(!findallvars)) {
        stop("The following variables were not found in the data frame: ",
            paste(vars[!findallvars], collapse = ", "))
    }

    # Subset the whole data frame!
    z <- z[allvars]

    if (Venn) {
      if (missing(shownodelabels)) shownodelabels <- FALSE
      if (missing(showpct)) showpct <- FALSE
      if (missing(showlegend)) showlegend <- FALSE
      if (missing(showlpct)) showlpct <- FALSE
    }

    if (check.is.na) {
      if (missing(shownodelabels)) shownodelabels <- FALSE
    }

    if (length(labelvar) > 0) {
        namesvarheaders <- names(labelvar)
        labelvar <- splitlines(labelvar, splitwidth, sp = "\n", at = c(" ", ".", "-", "+", "_", "=", "/"))
        names(labelvar) <- namesvarheaders
    }

    if (!missing(labelnode) && !is.list(labelnode)) stop("labelnode must be a list.")

    if (length(labelnode) > 0) {
      for (i in 1:length(labelnode)) {
        names(labelnode[[i]]) <- splitlines(names(labelnode[[i]]),splitwidth,sp ="\n", at=" ")
      }
    }

    if (check.is.na) {
      OLDVARS <- vars
      NEWVARS <- c()
      for (v in vars) {
        newvar <- paste0("MISSING_", v)
        m <- is.na(z[[v]])
        z[[newvar]] <- factor(m, levels = c(FALSE, TRUE),c("available","N/A"))
        # Note that available comes before N/A in alphabetical sorting.
        # Similarly FALSE comes before TRUE.
        # And 0 (representing FALSE) comes before 1 (representing TRUE) numerically.
        # This is convenient, especially when when using the seq parameter.
        NEWVARS <- c(NEWVARS, newvar)
      }
      vars <- NEWVARS
    }

    if (seq) {
      if (missing(showroot)) showroot <- FALSE
      sequence <- NULL
      for (i in 1:length(vars)) {
        sequence <- paste(sequence,z[[vars[i]]])
      }
      # The order of sequence levels has to be reversed
      # if the root node is not shown. Which is a bit odd.
      if (showroot) {
        sequence_levels <- names(sort(table(sequence)))
      } else {
        sequence_levels <- names(rev(sort(table(sequence))))
      }
      sequence <- factor(sequence,levels=sequence_levels)
      z$sequence <- sequence
      vars <- c("sequence",vars)
      if (check.is.na) {
        OLDVARS <- c("sequence",OLDVARS)
      }
      numvars <- length(vars)
      if (missing(showcount)) showcount <- c(sequence=TRUE)
      if (missing(showpct)) showpct <- c(sequence=TRUE)
      if (missing(shownodelabels)) shownodelabels <- c(sequence=FALSE)
    }
    
    if (pattern) {
      if (missing(showroot)) showroot <- FALSE
      edgeattr <- paste(edgeattr,"arrowhead=none")
      PATTERN <- NULL
      for (i in 1:length(vars)) {
        PATTERN <- paste(PATTERN,z[[vars[i]]])
      }
      # The order of pattern levels has to be reversed
      # if the root node is not shown. Which is a bit odd.
      if (showroot) {
        PATTERN_levels <- names(sort(table(PATTERN)))
      } else {
        PATTERN_levels <- names(rev(sort(table(PATTERN))))
      }
      PATTERN <- factor(PATTERN,levels=PATTERN_levels)
      z$pattern <- PATTERN
      vars <- c("pattern",vars)
      if (check.is.na) {
        OLDVARS <- c("pattern",OLDVARS)
      }
      numvars <- length(vars)
      if (missing(showcount)) showcount <- c(pattern=TRUE)
      if (missing(showpct)) showpct <- c(pattern=TRUE)
      if (missing(shownodelabels)) shownodelabels <- c(pattern=FALSE)
    }    

    if (is.null(names(gradient))) {
      gradient <- rep(gradient[1],numvars)
      names(gradient) <- vars
    } else {
      if (all(gradient)) {
        gg <- rep(FALSE,numvars)
      } else
      if (all(!gradient)) {
        gg <- rep(TRUE,numvars)
      } else
      if (length(gradient)!=numvars) {
        stop("gradient: ambiguous specification.")
      } else {
        gg <- rep(NA,numvars)
      }
      if (any(names(gradient) %in% vars)) {
        m <- match(names(gradient),vars)
        gg[m[!is.na(m)]] <- gradient[!is.na(m)]
      }
      names(gg) <- vars
      gradient <- gg
    }

    if (is.null(names(shownodelabels))) {
      shownodelabels <- rep(shownodelabels[1],numvars)
      names(shownodelabels) <- vars
    } else {
      if (all(shownodelabels)) {
        sn <- rep(FALSE,numvars)
      } else
      if (all(!shownodelabels)) {
        sn <- rep(TRUE,numvars)
      } else
      if (length(shownodelabels)!=numvars) {
        stop("shownodelabels: ambiguous specification.")
      } else {
        sn <- rep(NA,numvars)
      }
      if (any(names(shownodelabels) %in% vars)) {
        m <- match(names(shownodelabels),vars)
        sn[m[!is.na(m)]] <- shownodelabels[!is.na(m)]
      }
      names(sn) <- vars
      shownodelabels <- sn
    }

    if (is.null(names(showcount))) {
      showcount <- rep(showcount[1],numvars)
      names(showcount) <- vars
    } else {
      if (all(showcount)) {
        sc <- rep(FALSE,numvars)
      } else
      if (all(!showcount)) {
        sc <- rep(TRUE,numvars)
      } else
      if (length(showcount)!=numvars) {
        stop("showcount: ambiguous specification.")
      } else {
        sc <- rep(NA,length(vars))
      }
      if (any(names(showcount) %in% vars)) {
        m <- match(names(showcount),vars)
        sc[m[!is.na(m)]] <- showcount[!is.na(m)]
      }
      names(sc) <- vars
      showcount <- sc
    }

    if (is.null(names(showpct))) {
      showpct <- rep(showpct[1],numvars)
      names(showpct) <- vars
    } else {
      if (all(showpct)) {
        sp <- rep(FALSE,numvars)
      } else
      if (all(!showpct)) {
        sp <- rep(TRUE,numvars)
      } else
      if (length(showpct)!=numvars) {
        stop("showpct: ambiguous specification.")
      } else {
        sp <- rep(NA,numvars)
      }
      if (any(names(showpct) %in% vars)) {
        m <- match(names(showpct),vars)
        sp[m[!is.na(m)]] <- showpct[!is.na(m)]
      }
      names(sp) <- vars
      showpct <- sp
    }

    if (is.null(names(revgradient))) {
      revgradient <- rep(revgradient[1],numvars)
      names(revgradient) <- vars
    } else {
      if (all(revgradient)) {
        rg <- rep(FALSE,numvars)
      } else
      if (all(!revgradient)) {
        rg <- rep(TRUE,numvars)
      } else
      if (length(revgradient)!=numvars) {
        stop("revgradient: ambiguous specification.")
      } else {
        rg <- rep(NA,numvars)
      }
      if (any(names(revgradient) %in% vars)) {
        m <- match(names(revgradient),vars)
        rg[m[!is.na(m)]] <- revgradient[!is.na(m)]
      }
      names(rg) <- vars
      revgradient <- rg
    }

    findvars <- names(shownodelabels) %in% vars
    if (any(!findvars)) {
      stop("The following variables named in shownodelabels were not in vars: ",
          paste(names(shownodelabels)[!findvars], collapse = ", "))
    }

    # If varlabelloc is a single unnamed value, then apply it to all variables.
    if (!missing(varlabelloc) && (length(varlabelloc)==1) && (is.null(names(varlabelloc))) ) {
      varlabelloc <- rep(varlabelloc,numvars)
      names(varlabelloc) <- vars
    }

    # If varminwidth is a single unnamed value, then apply it to all variables.
    if (!missing(varminwidth) && (length(varminwidth)==1) && (is.null(names(varminwidth))) ) {
      varminwidth <- rep(varminwidth,numvars)
      names(varminwidth) <- vars
    }

    # If varminheight is a single unnamed value, then apply it to all variables.
    if (!missing(varminheight) && (length(varminheight)==1) && (is.null(names(varminheight))) ) {
      varminheight <- rep(varminheight,numvars)
      names(varminheight) <- vars
    }

    if (plain) {
      fillcolor <- rep(c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5","#084594"), 8)[1:numvars]
      autocolorvar <- FALSE
      if (missing(colorvarlabels)) colorvarlabels <- FALSE
      if (missing(showlegend)) showlegend <- FALSE
      if (missing(squeeze)) squeeze <- 0.6
    }

    if (squeeze<0 || squeeze>1) stop("The squeeze parameter must be between 0 and 1.")
    if (missing(nodesep)) nodesep <- 0.1+(1-squeeze)*(1-0.1)
    if (missing(margin)) margin <- 0.1+(1-squeeze)*(0.3-0.1)

    singleColor <- FALSE
    # Single color specified
    if (!missing(fillcolor) && (length(fillcolor)==1) && (is.null(names(fillcolor)))) {
      singleColor <- TRUE
      fillcolor <- rep(fillcolor,numvars)
      names(fillcolor) <- vars
      if (missing(rootfillcolor)) rootfillcolor <- fillcolor
    }

    if (missing(fillcolor)) {
      varlabelcolors <- rep(unknowncolor,numvars)
    } else {
      varlabelcolors <- fillcolor
      varlabelcolors[fillcolor=="white"] <- "black"  # So that varlabels are visible on a white background
      if (singleColor || plain) varlabelcolors[TRUE] <- "black"
    }

    if (!is.null(palette) && length(palette)==1) {
      palette <- rep(palette,numvars)
      names(palette) <- vars
      if (missing(rootfillcolor)) rootfillcolor <- col[[1]][palette,1]
    }

    if (!plain) {
      FC <- vector("list",numvars)
      names(FC) <- vars
      numPalettes <- nrow(col[[1]])
      for (i in 1:numvars) {
        thisvar <- z[[vars[i]]]
        if (i>numPalettes) {
          row <- i %% numPalettes
        } else {
          row <- i
        }
        if (!is.null(palette)) {
          if (check.is.na) {
            if (any(names(palette)==OLDVARS[i])) {
              row <- palette[names(palette)==OLDVARS[i]]
            }
          } else {
            if (any(names(palette)==vars[i])) {
              row <- palette[names(palette)==vars[i]]
            }
          }
        }
        revgrad <- revgradient[vars[i]]
        if (is.na(revgrad)) revgrad <- FALSE

        if (is.logical(thisvar)) {
          thisvar <- factor(thisvar, c("FALSE", "TRUE"))
        }
        values <- names(table(thisvar,exclude=NULL))
        values[is.na(values)] <- "NA"
        Nallvalues <- length(values)
        Nnonmissing <- length(values[values!="NA"])
        if (is.null(NAfillcolor)) {
          valuecolors <- rep(unknowncolor,length(values))
        } else {
          valuecolors <- rep(NAfillcolor,length(values))
        }
        if (Nnonmissing>0) {
          if (Nnonmissing>length(col) || (seq & (vars[i]=="sequence")) || (pattern & (vars[i]=="pattern")) || (row==0)) {
            valuecolors[values!="NA"] <- col[[1]][row] # "grey90"
            names(valuecolors) <- values
            varlabelcolors[i] <- col[[1]][row] # "grey90"
          } else {
            if (!missing(fillcolor) && (vars[i] %in% names(fillcolor))) {
              if (is.null(NAfillcolor)) {
                valuecolors[TRUE] <- fillcolor[names(fillcolor)==vars[i]]
              } else {
                valuecolors[values!="NA"] <- fillcolor[names(fillcolor)==vars[i]]
              }
              varlabelcolors[i] <- fillcolor[names(fillcolor)==vars[i]]
            } else
            if (gradient[vars[i]]) {
              if (revgrad) {
                if (is.null(NAfillcolor)) {
                  valuecolors[TRUE] <- rev(col[[Nallvalues]][row,])
                } else {
                  valuecolors[values!="NA"] <- rev(col[[Nnonmissing]][row,])
                }
                varlabelcolors[i] <- col[[2]][row,2]
              } else {
                if (is.null(NAfillcolor)) {
                  valuecolors[TRUE] <- col[[Nallvalues]][row,]
                } else {
                  valuecolors[values!="NA"] <- col[[Nnonmissing]][row,]
                }
               varlabelcolors[i] <- col[[2]][row,2]
              }
              names(valuecolors) <- values
            } else {
              if (is.null(NAfillcolor)) {
                valuecolors[TRUE]  <- rep(col[[1]][row],Nallvalues)
              } else {
                valuecolors[values!="NA"]  <- rep(col[[1]][row],Nnonmissing)
              }
              varlabelcolors[i]  <- col[[1]][row]
            }
          }
        }
        names(valuecolors) <- values
        FC[[vars[i]]] <- valuecolors
      }
      fillcolor <- FC
      colorIndex <- rep(1:numPalettes,length=numvars)
      names(varlabelcolors) <- vars
      if (check.is.na) {
        names(varlabelcolors) <- OLDVARS
      }
    }

    # If fillcolor isn't a list, create a list
    if (!is.list(fillcolor)) {
      FC <- vector("list",numvars)
      names(FC) <- vars
      for (i in 1:length(vars)) {
        values <- names(table(z[[vars[i]]],exclude=NULL))
        values[is.na(values)] <- "NA"
        valuecolors <- rep(fillcolor[i],length(values))
        names(valuecolors) <- values
        FC[[vars[i]]] <- valuecolors
      }
      fillcolor <- FC
    }
  }

  ### ----------- End code for root only ------------

  numvars <- length(vars)

  # Node outline colors
  if (!colornodes) color <- rep("black", 100)

  z_names <- names(z)

  findvars <- vars %in% z_names
  if (any(!findvars)) {
      stop("The following variables were not found in the data frame: ",
          paste(vars[!findvars], collapse = ", "))
  }

  # Special case with a single variable being relabled and variable name not specified
  if (!missing(labelvar) && is.null(names(labelvar))) {
    if ((numvars==1) && (length(labelvar)==1)) {
      names(labelvar) <- z_names
    }
  }

  findvars <- names(labelvar) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in labelvar were not found in the data frame: ",
          paste(vars[!findvars], collapse = ", "))
  }

  findvars <- names(prunebelow) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in prunebelow were not found in the data frame: ",
          paste(names(prunebelow)[!findvars], collapse = ", "))
  }

  findvars <- names(prune) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in prune were not found in the data frame: ",
          paste(names(prune)[!findvars], collapse = ", "))
  }

  findvars <- names(follow) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in follow were not found in the data frame: ",
          paste(names(follow)[!findvars], collapse = ", "))
  }

  findvars <- names(keep) %in% z_names
  if (any(!findvars)) {
      stop("The following variables named in keep were not found in the data frame: ",
          paste(names(keep)[!findvars], collapse = ", "))
  }

  if (is.null(z) || is.null(vars)) {
    #cat("Return NULL because z is NULL or vars is NULL\n")
    return(NULL)
  }
  if (nrow(z) == 0 || numvars == 0) {
    #cat("Return NULL because z is empty or vars has zero length\n")
    return(NULL)
  }
  TopText <- ""
  if (!is.null(nodefunc)) {
    if (numvars == 1)
        nodeargs$leaf <- TRUE
    ThisLevelText <- c()
    qqq <- z[[vars[1]]]
    qqq <- as.character(qqq)
    qqq[is.na(qqq)] <- "NA"
    categoryCounts <- table(qqq, exclude = NULL)
    CAT <- names(categoryCounts)
    for (value in CAT) {
      nodetext <- nodefunc(z[qqq == value, ], vars[1], value, args = nodeargs)
      nodetext <- splitlines(nodetext, width = splitwidth, sp = "\n", at=" ")
      ThisLevelText <- c(ThisLevelText, nodetext)
    }
    if (root) {
      topnodeargs <- nodeargs
      topnodeargs$root <- TRUE
      topnodeargs$leaf <- FALSE
      nodetext <- nodefunc(z, "", value = NA, args = topnodeargs)
      nodetext <- splitlines(nodetext, width = splitwidth,sp = "\n", at=" ")
      TopText <- nodetext
    }
    names(ThisLevelText) <- CAT
  }
  else {
    ThisLevelText <- text[[vars[1]]]
  }
  #if (!HTMLtext)
  #    ThisLevelText <- makeHTML(ThisLevelText)
  #if (!HTMLtext)
  #    TopText <- makeHTML(TopText)

  fc <- flowcat(z[[vars[1]]], root = root, title = title, parent = parent,
    var=vars[[1]],
    last = last, labels = labelnode[[vars[1]]], tlabelnode=tlabelnode, labelvar = labelvar[vars[1]],
    varminwidth=varminwidth[vars[1]],varminheight=varminheight[vars[1]],varlabelloc=varlabelloc[vars[1]],
    check.is.na=check.is.na,
    sameline=sameline,
    shownodelabels=shownodelabels[vars[1]],
    showpct=showpct[vars[1]],
    showcount=showcount[vars[1]],
    prunefull=prune[[vars[1]]],
    prunelone=prunelone,
    HTMLtext = HTMLtext, showvarnames = showvarnames,
    keep=keep[[vars[1]]],
    pruneNA=pruneNA,
    text = ThisLevelText, ttext=ttext,TopText = TopText, digits = digits, cdigits = cdigits,
    splitwidth = splitwidth, showempty = showempty, topcolor = color[1],
    color = color[2], topfillcolor = rootfillcolor, fillcolor = fillcolor[[vars[1]]],
    vp = vp, rounded = rounded, showroot=showroot)
  CurrentVar <- vars[1]
  if (CurrentVar %in% names(follow)) {
    followlevels <- follow[[CurrentVar]]
  } else {
    followlevels <- NULL
  }
  if (CurrentVar %in% names(prunebelow)) {
    prunebelowlevels <- prunebelow[[CurrentVar]]
  } else {
    prunebelowlevels <- NULL
  }
  i <- 0
  for (varlevel in fc$levels) {
    TTEXT <- ttext
    j <- 1
    while (j <= length(TTEXT)) {
      if (!any(names(TTEXT[[j]])==CurrentVar)) {
        TTEXT[[j]] <- ""
      } else {
        if (TTEXT[[j]][CurrentVar]==varlevel) {
          TTEXT[[j]] <- TTEXT[[j]][names(TTEXT[[j]])!=CurrentVar]
        } else {
          if (TTEXT[[j]][CurrentVar]!=varlevel) {
            TTEXT[[j]] <- ""
          }
        }
      }
      j <-j + 1
    }

    TLABELNODE <- tlabelnode
    j <- 1
    while (j <= length(TLABELNODE)) {
      if (!any(names(TLABELNODE[[j]])==CurrentVar)) {
        TLABELNODE[[j]] <- ""
      } else {
        if (TLABELNODE[[j]][CurrentVar]==varlevel) {
          TLABELNODE[[j]] <- TLABELNODE[[j]][names(TLABELNODE[[j]])!=CurrentVar]
        } else {
          if (TLABELNODE[[j]][CurrentVar]!=varlevel) {
            TLABELNODE[[j]] <- ""
          }
        }
      }
      j <-j + 1
    }


    i <- i + 1
    if (!(varlevel %in% prunebelowlevels) & (is.null(followlevels) | (varlevel %in% followlevels))) {
      if (varlevel == "NA") {
          select <- is.na(z[[CurrentVar]])
      }
      else {
          select <- which(z[[CurrentVar]] == varlevel)
      }
      if (length(select)>0 & numvars>=1) {
        fcChild <- vtree(z[select, , drop = FALSE],
          vars[-1], parent = fc$nodenum[i], last = max(fc$nodenum),
          labelnode = labelnode,
          tlabelnode = TLABELNODE,
          colorvarlabels=colorvarlabels,
          check.is.na=check.is.na,
          shownodelabels=shownodelabels,
          showpct=showpct,
          showcount=showcount,
          sameline=sameline, showempty = showempty,
          root = FALSE, prune=prune, prunebelow = prunebelow, labelvar = labelvar,
          varminwidth = varminwidth, varminheight = varminheight, varlabelloc=varlabelloc,
          prunelone=prunelone,
          nodefunc = nodefunc, nodeargs = nodeargs, digits = digits,
          showvarnames = showvarnames,
          keep=keep,
          follow=follow,
          pruneNA=pruneNA,
          text = text, ttext=TTEXT,gradient=gradient,
          colornodes = colornodes, color = color[-1], fillnodes = fillnodes,
          fillcolor = fillcolor, splitwidth = splitwidth,
          vp = vp, rounded = rounded)
        fc <- joinflow(fc,fcChild)
      }
    }
  }
  if (length(fc$nodenum) == 0) {
      #cat("Setting fc to NULL\n")
      fc <- NULL
  }

  # If desired, show variable levels and legend

  if (root) {
      if (showvarnames) {
          # Special case for check.is.na
          if (check.is.na) {
            VARS <- OLDVARS
          } else {
            VARS <- vars
          }
          if (!is.null(labelvar)) {
              for (i in 1:numvars) {
                if (!is.na(labelvar[vars[i]])) {
                  VARS[i] <- labelvar[vars[i]]
                }
              }
          }

          if (!HTMLtext) {
            VARS <- splitlines(VARS,width=lsplitwidth,sp='\n',at = c(" ", ".", "-", "+", "_", "=", "/"))
            VARS <- convertToHTML(VARS)
          }

          if (colorvarlabels) {
            colored_VARS <- paste0('<FONT COLOR="',varlabelcolors,'">',"<B>",VARS,'  </B>','</FONT>')
          } else {
            colored_VARS <- VARS
          }
          colored_VARS <- paste0('<FONT POINT-SIZE="',varnamepointsize,'">',colored_VARS,'</FONT>')
          marginalText <- rep("",numvars)
          if (showlegend) {
            for (i in 1:numvars) {
              thisvarname <- vars[i]
              thisvar <- z[[thisvarname]]
              if (is.logical(thisvar)) {
                thisvar <- factor(thisvar, c("FALSE", "TRUE"))
              }
              categoryCounts <- table(thisvar,exclude=NULL)

              if (Venn) {
                names(categoryCounts)[which(names(categoryCounts)=="1" | names(categoryCounts)=="TRUE")] <- "Yes"
                names(categoryCounts)[which(names(categoryCounts)=="0" | names(categoryCounts)=="FALSE")] <- "No"
              }

              names(categoryCounts)[is.na(names(categoryCounts))] <- "NA"

              if (vp & any(is.na(thisvar))) {
                cc <- categoryCounts
                cc <- cc[names(cc)!="NA"]
                if (length(cc)>0) {
                  if (showlpct) {
                    npctString <- paste0(cc," (",
                      around(100*cc/sum(cc),digits),"%)")
                  } else {
                    npctString <- cc
                  }
                } else {
                  npctString <- NULL
                }
                npctString <- c(npctString,categoryCounts["NA"])
              } else {
                if (showlpct) {
                  npctString <- paste0(categoryCounts," (",
                    around(100*categoryCounts/length(thisvar),digits),"%)")
                } else {
                  npctString <- categoryCounts
                }
              }

              colors <- fillcolor[[thisvarname]][1:length(categoryCounts)]
              symbols <- rep("&#x25CF;",length(colors)) # rep("&#x2B24;",length(colors))
              colorkey <- paste0(
                "<FONT POINT-SIZE='30' COLOR='",colors,"'>",
                symbols,
                "</FONT>")
              spaces <- rep("&nbsp;",length(colors))
              if (any(is.na(thisvar))) {
                colorkey[names(categoryCounts)=="NA"] <- paste0(
                  '<FONT POINT-SIZE="20" COLOR="','black','">',
                  '&#x25EF;',
                  '</FONT>')
                spaces[names(categoryCounts)=="NA"] <- '&nbsp;&nbsp;'
              }

              CAT <- names(categoryCounts)

              # Relabel the nodes if labels have been specified
              labels <- labelnode[[thisvarname]]
              for (label in labels) {
                if (label %in% CAT) {
                  m <- match(label,CAT)
                  CAT[m] <- names(labels)[labels==label]
                }
              }

              legendlabel <- paste0(CAT,", ",npctString)

              if (HTMLtext) {
                legendlabel <- splitlines(legendlabel,width=lsplitwidth,sp='<BR/>',at=" ")
              } else {
                legendlabel <- splitlines(legendlabel,width=lsplitwidth,sp='\n',
                  at = c(" ", ".", "-", "+", "_", "=", "/"))
                legendlabel <- convertToHTML(legendlabel)
              }

              if (length(legendlabel)>length(col) || thisvarname=="sequence") {
                marginalText[i] <- ""
              } else {
                marginalText[i] <- paste0('<FONT POINT-SIZE="1"> </FONT><BR ALIGN="LEFT" />',
                  colorkey,spaces,
                  legendlabel,sep="",
                  collapse='<BR ALIGN="LEFT" />')
                marginalText[i] <- paste(marginalText[i],'<BR ALIGN="LEFT" />')
              }
            }
          }

          labels <- paste0(
            'label=<',
            colored_VARS,'<BR/>',marginalText,
            '>')
          if (showroot) {
            nodelevels <- 'Node_L0[style=invisible]\n'
          } else {
            nodelevels <- ''
          }
          nodelevels <- paste0(nodelevels, paste0('Node_L',
              1:numvars,
              '[',
              labels,
              ' shape=none margin=0]',collapse = '\n'))
          nodelinks <- paste0('Node_L', 1:numvars, collapse = '->')
          if (showroot) {
            nodelinks <- paste0('Node_L0->',nodelinks)
          }
          nodelevels <- paste0(nodelevels, paste0('\n\nedge[style=invis];\n',
              nodelinks),'\n')
      }
      else {
          nodelevels <- ''
      }
      showflow(fc, getscript = getscript, nodesep = nodesep,
        ranksep=ranksep, margin=margin, nodelevels = nodelevels, horiz = horiz,
        width=width,height=height,
        graphattr=graphattr,nodeattr=nodeattr,edgeattr=edgeattr)
  }
  else {
      fc
  }
}



showflow <- function(flow,getscript=FALSE,nodesep=0.5,ranksep=0.5,margin=0.2,
nodelevels="",horiz=FALSE,width=NULL,height=NULL,
graphattr="",nodeattr="",edgeattr="") {
#
# {show} a {flow}chart produced by flowcat or by hier.
#
# showscript Only show the script generated rather than displaying the flowchart? Useful for debugging.
# nodesep    The nodesep graph attribute.
# ranksep    The ranksep graph attribute.
#

  nodePart <- "node [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black"
  nodePart <- paste0(nodePart,",margin=",margin)
  nodePart <- paste0(nodePart,ifelse(nodeattr=="","",","),nodeattr)
  nodePart <- paste0(nodePart,"]\n")

  graphPart <- paste0('graph [layout = dot, compound=true, nodesep=',nodesep,', ranksep=',ranksep,', fontsize=12')
  graphPart <- paste0(graphPart,ifelse(graphattr=="","",","),graphattr)
  graphPart <- paste0(graphPart,']\n')

  script <- paste0(
    'digraph vtree {\n',
    graphPart,
    nodePart)

  if (horiz) {
    script <- paste0(script,'rankdir=LR;\n')
  }

  script <- paste0(script,nodelevels)

  edgePart <- '\nedge[style=solid'
  edgePart <- paste0(edgePart,ifelse(edgeattr=="","",","),edgeattr)
  edgePart <- paste0(edgePart,']\n')

  script <- paste0(script,edgePart,
    flow$edges,"\n\n",flow$labelassign,sep="\n")

  script <- paste0(script,"\n}\n")
  if (getscript) { return(script) }
  flowchart <- DiagrammeR::grViz(script,width=width,height=height)
  flowchart
}



flowcat <- function(z,root=TRUE,title="",parent=1,last=1,labels=NULL,tlabelnode=NULL,HTMLtext=FALSE,
var,
check.is.na=FALSE,
labelvar=NULL,
varminwidth=NULL,varminheight=NULL,varlabelloc=NULL,
shownodelabels=TRUE,sameline=FALSE,
prunefull=NULL,
prunelone=NULL,
keep=NULL,
text=NULL,ttext=NULL,TopText="",showempty=FALSE,digits=0,cdigits=2,
showpct=TRUE,
showcount=TRUE,
showvarnames=FALSE,
pruneNA=FALSE,
splitwidth=Inf,topcolor="black",color="blue",topfillcolor="olivedrab3",fillcolor="olivedrab2",
vp=TRUE,rounded=FALSE,showroot=TRUE) {
#
# Write DOT code for a single-level {flow}chart of {cat}egories using the
# DiagrammeR framework.
#
# https://en.wikipedia.org/wiki/DOT_(graph_description_language)
#

  if (HTMLtext) {
    sepN <- "<BR/>"
  } else {
    sepN <- "\n"
  }

  if (is.na(shownodelabels)) shownodelabels <- TRUE

  if (is.logical(z)) {
    z <- factor(z, c("FALSE", "TRUE"))
  }

  categoryCounts <- table(z,exclude=NULL)
  names(categoryCounts)[is.na(names(categoryCounts))] <- "NA"

  # Pre-pend the parent node
  categoryCounts <- c(length(z),categoryCounts)
  names(categoryCounts)[1] <- title

  # Use npct to calculate percentages, but don't use "valid percentages"
  # since the denominator should always be the number in the parent node.
  # npctString <- npct(z,includemiss=TRUE,vp=FALSE,pcs="%")
  # If there are no missing values, don't include the NA category
  # if (sum(is.na(z))==0) npctString <- npct(z,pcs="%")

  if (vp & any(is.na(z))) {
    cc <- categoryCounts[-1]
    cc <- cc[names(cc)!="NA"]
    if (length(cc)>0) {
      npctString <- rep("",length(cc))
      if (showcount) {
        npctString <- cc
        if (showpct) npctString <- paste0(npctString," ")
      }
      if (showpct) {
        npctString <- paste0(npctString,"(",
          around(100*cc/sum(cc),digits),"%)")
      }
    } else {
      npctString <- NULL
    }
    if (showcount) {
      npctString <- c(npctString,categoryCounts["NA"])
    } else {
      npctString <- c(npctString,"")
    }
  } else {
    npctString <- rep("",length(categoryCounts[-1]))
    if (showcount) {
      npctString <- categoryCounts[-1]
      if (showpct) npctString <- paste0(npctString," ")
    }
    if (showpct) {
      npctString <- paste0(npctString,"(",
        around(100*categoryCounts[-1]/length(z),digits),"%)")
    }
  }

  npctString <- c(length(z),npctString)
  names(npctString)[1] <- title

  if (!showempty) {
    s <- categoryCounts>0
    categoryCounts <- categoryCounts[s]
    npctString <- npctString[s]
  }

  if (!is.null(prunefull)) {
    if (is.numeric(prunefull)) {
      categoryCounts <- c(categoryCounts[1],categoryCounts[-1][-prunefull])
      npctString <- c(npctString[1],npctString[-1][-prunefull])
    } else {
      matching <- names(categoryCounts)[-1] %in% prunefull
      categoryCounts <- c(categoryCounts[1],categoryCounts[-1][!matching])
      npctString <- c(npctString[1],npctString[-1][!matching])
    }
  }

  if (!is.null(keep)) {
    if (is.numeric(keep)) {
      categoryCounts <- c(categoryCounts[1],categoryCounts[-1][keep])
      npctString <- c(npctString[1],npctString[-1][keep])
    } else {      
      matching <- match(keep,names(categoryCounts)[-1])
      matching <- matching[!is.na(matching)]
      categoryCounts <- c(categoryCounts[1],categoryCounts[-1][matching])
      npctString <- c(npctString[1],npctString[-1][matching])
    }
  }

  if (pruneNA) {
    m <- names(categoryCounts)[-1]!="NA"
    categoryCounts <- c(categoryCounts[1],categoryCounts[-1][m])
    npctString <- c(npctString[1],npctString[-1][m])
  }

  if (!is.null(prunelone)) {
    if (length(categoryCounts[-1])==1) {
      if (names(categoryCounts)[-1] %in% prunelone) {
        categoryCounts <- categoryCounts[1]
      }
    }
  }

  # Number of new nodes to add to the tree
  n <- length(categoryCounts)-1              # exclude the parent node

  if (n>0) {
    # Number the parent node and the additional nodes to be added
    nodenum <- c(parent,last+(1:n))
  } else {
    nodenum <- parent
  }
  nodenames <- paste0("Node_",nodenum)

  CAT <- names(categoryCounts)

  FILLCOLOR <- fillcolor[match(CAT[-1],names(fillcolor))]

  extraText <- rep("",length(CAT))

  # Match extra text to nodes
  if (TopText!="") extraText[1] <- paste0("<BR/> ",TopText)
  for (label in names(text)) {
    if (label %in% names(categoryCounts)) {
      m <- match(label,names(categoryCounts))
      if (text[names(text)==label]!="") {
        extraText[m] <- paste0("",text[names(text)==label])
      }
    }
  }

  if (length(ttext)>0) {
    for (j in 1:length(ttext)) {
      if (length(ttext[[j]])==2 && any(names(ttext[[j]])==var)) {
        TTEXTposition <- CAT[-1] == ttext[[j]][names(ttext[[j]])==var]
        extraText[-1][TTEXTposition] <- ttext[[j]]["text"]
      }
    }
  }

  if (length(tlabelnode)>0) {
    for (j in 1:length(tlabelnode)) {
      if (length(tlabelnode[[j]])==2 && any(names(tlabelnode[[j]])==var)) {
        tlabelnode_position <- CAT[-1] == tlabelnode[[j]][names(tlabelnode[[j]])==var]
        CAT[-1][tlabelnode_position] <- tlabelnode[[j]]["label"]
      }
    }
  }

  if (HTMLtext) {
    CAT <- splitlines(CAT,width=splitwidth,sp="<BR/>",at=" ")
  } else {
    CAT <- splitlines(CAT,width=splitwidth,sp="\n",at = c(" ", ".", "-", "+", "_", "=", "/"))
  }

  if (check.is.na) {
    for (i in 2:length(CAT)) {
      varname <- gsub("^MISSING_(.+)", "\\1", var)
      # CAT[i] <- paste0(varname," ",CAT[i])
    }
  }

  # Relabel the nodes if labels have been specified
  for (label in labels) {
    if (label %in% names(categoryCounts)) {
      m <- match(label,names(categoryCounts))
      CAT[m] <- names(labels)[labels==label]
    }
  }

  # Relabel the nodes if labelvar has been specified
  if (!showvarnames) {
    if (!is.null(labelvar)) {
      if (!is.na(labelvar)) {
        if (HTMLtext) {
          CAT[-1] <- paste0(labelvar,"<BR/>",CAT[-1])
        } else {
          CAT[-1] <- paste0(labelvar,"\n",CAT[-1])        
        }
      }
    }
  }

  # Write DOT code for the edges
  if (showroot) {
    edgeVector <- paste0(nodenames[1],"->",nodenames[-1])
    edges <- paste(edgeVector,collapse=" ")
  } else {
    edges <- ""
  }

  if (rounded) {
    styleString <- ' style="rounded,filled"'
  } else {
    styleString <- ' style=filled'
  }

  # Glue a space or a line break onto the non-empty elements of CAT
  if (sameline) {
    for (i in 1:length(CAT)) {
      if (showcount || showpct || extraText[i]!="") {
        if (CAT[i]!="") CAT[i] <- paste0(CAT[i],", ")
      }
    }
  } else {
    for (i in 1:length(CAT)) {
      if (CAT[i]!="") CAT[i] <- paste0(CAT[i],sepN)
    }
  }

  if (!shownodelabels) {
    for (i in 2:length(CAT)) CAT[i] <- ""
  }

  if (!HTMLtext) {
    CAT <- convertToHTML(CAT)
    extraText[-1] <- convertToHTML(extraText[-1])
  }
  
  # Write DOT code for assigning labels (using the DiagrammeR framework)
  VARLABELLOC <- ""
  if (!is.null(varlabelloc) && !is.na(varlabelloc)) VARLABELLOC <- paste0("labelloc=",varlabelloc)
  VARMINWIDTH <- ""
  if (!is.null(varminwidth) && !is.na(varminwidth)) VARMINWIDTH <- paste0("width=",varminwidth)
  VARMINHEIGHT <- ""
  if (!is.null(varminheight) && !is.na(varminheight)) VARMINHEIGHT <- paste0("height=",varminheight)
  labelassign <- c()
  if (root) {
    if (showroot) {
      if (title!="") CAT[1] <- paste0(CAT[1],"<BR/>")
      labelassign <- paste(paste0(
        nodenames[1],'[label=<',CAT[1],npctString[1],extraText[1],'> color=',topcolor,styleString,
        ' fillcolor=<',topfillcolor,'>]'),collapse='\n')
    }
    labelassign <- paste0(labelassign,'\n',paste(paste0(
      nodenames[-1],'[label=<',CAT[-1],npctString[-1],extraText[-1],'> color=',color,styleString,
      ' fillcolor=<',FILLCOLOR,'>',VARLABELLOC,' ',VARMINWIDTH,' ',VARMINHEIGHT,']')),collapse='\n')
  } else {
    labelassign <- paste(paste0(
      nodenames[-1],'[label=<',CAT[-1],npctString[-1],extraText[-1],'> color=',color,styleString,
      ' fillcolor=<',FILLCOLOR,'>',VARLABELLOC,' ',VARMINWIDTH,' ',VARMINHEIGHT,']'),collapse='\n')
  }

  return(list(
    levels=names(categoryCounts)[-1],
    nodenum=nodenum[-1],
    edges=edges,labelassign=labelassign,
    lastnode=nodenum[length(nodenum)]))
}




joinflow <- function(...) {
#
# {join} information (from the flowcat function) about two or more {flow}charts
#

  edges <- labelassign <- labelshow <- nodenum <- c()
  flows <- list(...)
  for (i in 1:length(flows)) {
    if (!is.null(flows[[i]])) {
      nodenum <- c(nodenum,flows[[i]]$nodenum)
      if (length(edges)==0) {
        edges <- flows[[i]]$edges
      } else {
        edges <- paste0(edges,"\n",flows[[i]]$edges)
      }
      if (length(labelassign)==0) {
        labelassign <- flows[[i]]$labelassign
      } else {
        labelassign <- paste0(labelassign,"\n",flows[[i]]$labelassign)
      }
    }
  }
  return(list(nodenum=nodenum,edges=edges,labelassign=labelassign,labelshow=labelshow))
}



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



around <- function (x, digits = 2, tooLong = 10) {
    if (is.character(x)) {
      x
    } else
    if (is.integer(x) || is.factor(x)) {
      as.character(x)
    } else
    if (is.data.frame(x)) {
        for (i in 1:ncol(x)) {
            x[[i]] <- around(x[[i]], digits = digits)
        }
        x
    }
    else
    if (!is.numeric(x)) {
        x
    }
    else {
        if (digits == 0) {
            result <- formatC(x, digits = digits, drop0trailing = TRUE,
                format = "f", flag = "#")
            result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
                tooLong], digits = digits, drop0trailing = TRUE,
                format = "g", flag = "#")
        }
        else {
            result <- formatC(x, digits = digits, drop0trailing = FALSE,
                format = "f", flag = "#")
            result[nchar(result) > tooLong] <- formatC(x[nchar(result) >
                tooLong], digits = digits, drop0trailing = FALSE,
                format = "g", flag = "#")
        }
        result[result == "-0"] <- "0"
        result[result == "-0.0"] <- "0.0"
        result[result == "-0.00"] <- "0.00"
        result[result == "-0.000"] <- "0.000"
        result
    }
}



nodeMeanSD <- function(u,varname,value,args) {
  if (is.null(args$digits)) args$digits <- 1   # default value

  paste0("\n",args$var,": mean (SD)\n",
    around(mean(u[[args$var]],na.rm=TRUE),args$digits),
    " (",around(sd(u[[args$var]],na.rm=TRUE),args$digits),")",
    " mv=",sum(is.na(u[[args$var]])))
}



convertToHTML <- function(x) {
  # Convert various text elements to their HTML entities.
  # Note that order matters here!

  x <- gsub("&","&amp;",x)
  x <- gsub("<=","&le;",x)
  x <- gsub(">=","&ge;",x)
  x <- gsub("<","&lt;",x)
  x <- gsub(">","&gt;",x)

  # Also convert character sequences for line breaks.

  x <- gsub("\n\\*l","<BR ALIGN='LEFT'/>",x)

  x <- gsub("\\\\n","<BR/>",x)
  x <- gsub("\n","<BR/>",x)


  # Markdown-style formatting

  x <- gsub("\\*\\*(.+?)\\*\\*","<B>\\1</B>",x)
  x <- gsub("\\*(.+?)\\*","<I>\\1</I>",x)

  # In markdown, _underscores_ can be used to format in italics.
  # But I have disabled this because it caused problems with
  # variable_names_likeThis
  # x <- gsub("_(.+?)_","<I>\\1</I>",x)

  # Special character sequence for color!

  x <- gsub("%%([^ ]+?) (.+?)%%","<FONT COLOR=\"\\1\">\\2</FONT>",x)

  # Markdown-style formatting for superscript and subscript

  x <- gsub("\\^(.+?)\\^","<FONT POINT-SIZE='10'><SUP>\\1</SUP></FONT>",x)
  x <- gsub("~(.+?)~","<FONT POINT-SIZE='10'><SUB>\\1</SUB></FONT>",x)

  x
}




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

  qntl <- function(x,...) {
    if (any(is.na(x))) {
      NA
    } else {
      stats::quantile(x,...)
    }
  }

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

  RESULT <- ""
  for (i in 1:length(args$var)) {

    y <- u[[args$var[i]]]

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
            if (nodespec==value) {
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
          if (nodespec==value) {
            ShowNodeText <- TRUE
          } else {
            ShowNodeText <- FALSE
          }            
        }
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
      tabval <- table(around(y,digits=cdigits),exclude=NULL)
      countval <- paste0(" (n=",tabval,")")
      countval[tabval==1] <- ""
      listOutput <- paste0(paste0(names(tabval),countval),collapse=", ")

      if (ShowNodeText) {
        if (length(x)==0 || !is.numeric(x)) {
          minx <- maxx <- NA
        } else {
          minx <- min(x)
          maxx <- max(x)
        }

        result <- gsub("%var=(.+)%","",result)
        result <- gsub("%trunc=(.+)%","",result)
        result <- gsub("%noroot%","",result)
        result <- gsub("%leafonly%","",result)
        result <- gsub("%v%",args$var[i],result)
        result <- gsub("%list%",listOutput,result)
        result <- gsub("%mv%",paste0(missingNum),result)
        if (is.numeric(x) | is.logical(x)) {
          # Note that y is used in the call to nAndpct
          # so that missing values can be handled as desired
          result <- gsub("%npct%",nAndpct(y,digits=digits),result)
          result <- gsub("%pct%",justpct(y,digits=digits),result)
          result <- gsub("%mean%", around(mean(x), digits = cdigits),
              result)
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
      RESULT <- paste0(RESULT,result)
      if (TruncNodeText) {
        if (nchar(RESULT)>truncval) {
          RESULT <- paste0(substr(RESULT,1,truncval),"...")
        }
      }
    }
  }
  RESULT
}




#' @title grVizToPNG
#'
#' @author Nick Barrowman
#'
#' @description
#'  \code{grVizToPNG} Export a grViz object into a PNG file.
#'
#' @param g      an object produced by the grViz function from the DiagrammmeR package
#' @param width  the width in pixels of the bitmap
#' @param height the height in pixels of the bitmap
#' @param folder path to folder where the PNG file should stored
#'
#' @details
#'   First the grViz object is exported to an SVG file (using \code{DiagrammeRsvg::export_svg}).
#'   Then the SVG file is converted to a bitmap (using \code{rsvg::rsvg}).
#'   Then the bitmap is exported as a PNG file (using \code{png::writePNG}).
#'   Note that the SVG file and the PNG file will be named using the name of the \code{g} parameter
#'
#' @note
#'   In addition to the DiagrammmeR package, the following packages are used: \code{DiagrammeRsvg}, \code{rsvg}
#'
#' @return
#'   Returns the full path of the PNG file.
#'
#' @export
#'


grVizToPNG <- function (g, width=NULL, height=NULL, folder = ".") {
  filename <- paste0(sapply(as.list(substitute({g})[-1]), deparse),".png")
  if (is.null(g)) {
    g <- DiagrammeR::grViz("digraph empty{ Node1[label='Empty'] }")
  }
  # Convert any double backslashes to forward slashes.
  folder <- gsub("\\\\","/",folder)
  fullpath <- file.path(folder,filename)
  message <- utils::capture.output(svg <- DiagrammeRsvg::export_svg(g))
  result <- rsvg::rsvg_png(charToRaw(svg),fullpath, width = width, height=height)
  invisible(fullpath)
}
