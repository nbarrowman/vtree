#' vtree: a tool for calculating and drawing variable trees.
#'
#' @description
#' vtree is a flexible tool for generating variable trees —
#' diagrams that display information about nested subsets of a data frame.
#' Given simple specifications,
#' the \code{vtree} function produces these diagrams and automatically
#' labels them with counts, percentages, and other summaries.
#' 
#' With vtree, you can:
#' \itemize{
#'   \item explore a data set interactively, and 
#'   \item produce customized figures for reports and publications.
#' }
#' 
#' For a comprehensive introduction, type: \code{vignette("vtree")}
#' 
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#' 
#' @seealso 
#' \itemize{
#'   \item GitHub page: \url{https://github.com/nbarrowman/vtree}
#'   \item Report bugs at \url{https://github.com/nbarrowman/vtree/issues}
#' }
#' 
#' @docType package
#' @name vtree-package
NULL 



#'
#' Draw a variable tree
#'
#' @description
#' \code{vtree} is a tool for drawing variable trees.
#' Variable trees display information about nested subsets of a data frame,
#' in which the subsetting is defined by the values of categorical variables.
#'
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#'
#' @param z                Required: Data frame, or a single vector.
#' @param vars             Required (unless \code{z} is a vector):
#'                         Either a character string of whitespace-separated variable names
#'                         or a vector of variable names.
#' @param auto             Automatically choose variables? (\code{vars} should not be specified)
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
#' @param showvarinnode    Show the variable name in each node?
#' @param shownodelabels   Show node labels?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         Otherwise, a named logical vector indicates which variables should have their
#'                         node labels shown.
#'                         If the vector consists of only \code{TRUE} values,
#'                         it is interpreted as \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         Similarly, if the vector consists of only \code{FALSE} values, 
#'                         it is interpreted as \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param showvarnames     Show the name of the variable next to each level of the tree?
#' @param showlevels       (Deprecated) Same as showvarnames.
#' @param varnamepointsize Font size (in points) to use when displaying variable names.
#' @param legendpointsize  Font size (in points) to use when displaying legend.
#' @param prune            List of vectors that specifies nodes to prune.
#'                         The name of each element of the
#'                         list must be one of the variable names in \code{vars}.
#'                         Each element is a vector of character strings that
#'                         identifies the values of the variable (i.e. the nodes) to prune.
#' @param prunebelow       Like \code{prune} but instead of pruning the specified nodes,
#'                         their descendants are pruned.
#' @param prunesmaller     Prune any nodes with count less than specified number.
#' @param keep             Like \code{prune} but specifies which nodes to \emph{keep}.
#'                         The other nodes will be pruned.
#' @param follow           Like \code{keep} but specifies which nodes to "follow",
#'                         i.e. which nodes' \emph{descendants} to keep.
#' @param prunelone        (Deprecated) A vector of values specifying "lone nodes" (of \emph{any} variable) to prune.
#'                         A lone node is a node that has no siblings (an "only child").
#' @param pruneNA          (Deprecated) Prune all missing values?
#'                         This is problematic because "valid" percentages
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
#'                         See \strong{Displaying summary information} below for details.
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
#' @param ptable           Generate a pattern table instead of a variable tree? 
#'                         Only applies when \code{pattern=TRUE}.
#' @param showroot         Show the root node?
#'                         When \code{seq=TRUE}, it may be useful to set \code{showroot=FALSE}.
#' @param Venn             Display multi-way set membership information?
#'                         This provides an alternative to a Venn diagram.
#'                         This sets \code{showpct=FALSE} and \code{shownodelabels=FALSE}.
#'                         Assumption: all of the specified variables are logicals or 0/1 numeric variables.
#' @param palette          A vector of palette numbers (which can range between 1 and 14).
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
#' @param mincount         Minimum count to include in a pattern tree or pattern table.
#' @param maxcount         Maximum count to include in a pattern tree or pattern table.
#'                         (Overrides mincount.)
#' @param pxwidth          Width in pixels of the PNG bitmap to be rendered
#'                         when \code{vtree} is called from R Markdown.
#'                         If neither \code{pxwidth} nor \code{pxheight} is specified,
#'                         \code{pxwidth} is automatically set to 2000 pixels.
#' @param pxheight         Height in pixels of the PNG bitmap to be rendered
#'                         when \code{vtree} is called from R Markdown.
#' @param imagewidth       A character string specifying the width of the PNG image
#'                         to be rendered when \code{vtree} is called from R Markdown,
#'                         e.g. \code{"4in"}
#' @param imageheight      A character string specifying the height of the PNG image
#'                         to be rendered when \code{vtree} is called from R Markdown,
#'                         e.g. \code{"5in"}.
#'                         If neither \code{imageheight} nor \code{imagewidth} is specified,
#'                         \code{imageheight} is set to 3 inches.
#' @param arrowhead        DOT arrowhead style. Defaults to \code{"normal"}.
#'                         Other choices include \code{"none"}, \code{"vee"}.
#' @param maxNodes         An error occurs if the number of nodes exceeds \code{maxNodes},
#'                         which defaults to 1000.
#' @param unchecked        Vector of string values interpreted as "unchecked". 
#' @param checked          Vector of string values interpreted as "checked".
#' @param just             Text justification ("l"=left, "c"=center, "r"=right).
#' @param verbose          Report additional details?
#' @param folder           Optional path to a folder where the PNG file should stored
#'                         when called during knit
#' @param as.if.knit       Behave as if called while knitting?
#' @param pngknit          Generate a PNG file when called during knit?
#'
#' @return
#' The value returned by \code{vtree} varies
#' depending on both the parameter values specified
#' and the context in which \code{vtree} is called.
#' 
#' First, there are two special cases where \code{vtree} does not show a variable tree:
#'  
#' \itemize{
#'   \item If \code{ptable=TRUE}, the return value is a data frame representing a pattern table.
#'   \item Otherwise, if \code{getscript=TRUE}, the return value is a character string,
#'         consisting of a DOT script that describes the variable tree.
#' }
#' 
#' If neither of the above cases applies, the return value is as follows.
#' If knitting is \emph{not} taking place
#' (such as when \code{vtree} is used \strong{interactively}):
#' \itemize{
#'   \item the return value is an object of class \code{htmlwidget} (see \link[DiagrammeR]{DiagrammeR}).
#'         It will intelligently print itself into HTML in a variety of contexts
#'         including the R console, within R Markdown documents,
#'         and within Shiny output bindings.
#' }
#' 
#' If knitting \emph{is} taking place:
#' \itemize{
#'   \item If \code{pngknit=TRUE} (the default),
#'         the return value is a character string of
#'         pandoc markdown code to embed a PNG file with fully-specified path.
#'         The character string will have class \code{knit_asis} so that
#'         knitr will treat it as is
#'         (the effect is the same as the chunk option results = 'asis')
#'         when it is written to the output. (See \code{?knitr::asis_output})
#'   \item If \code{pngknit=FALSE}, the return value is the same as when knitting is not
#'         taking place, i.e. an object of class \code{htmlwidget}.
#' }
#'
#' @section R Markdown:
#' As noted in the \strong{Value} section above,
#' \code{vtree} has special support for R Markdown.
#' 
#' By default, when knitting an R Markdown file,
#' \code{vtree} generates PNG files and embeds them automatically in the output document.
#' This feature is needed when knitting to a \code{.docx} file.
#' When knitting to HTML, it is not necessary to generate PNG files
#' because HTML browsers can directly display htmlwidgets.
#' 
#' To generate htmlwidgets instead of PNG files, specify \code{pngknit=FALSE}.
#' (Note, however, that there are some advantages to embedding PNG files in an HTML file.
#' For example,
#' some browsers perform poorly when numerous htmlwidgets are included in an HTML file.)
#'
#' When PNG files are generated, they are stored by default in a temporary folder.
#' The folder can also be specified using the \code{folder} parameter.
#' (Using the base R function \code{options}, 
#' a custom option \code{vtree_folder} is used to automatically keep track of this.)
#' Successive PNG files generated by an R Markdown file
#' are named \code{vtree1.png}, \code{vtree2.png}, etc.
#' (A custom option \code{vtree_count} is used to automatically keep track of the number of PNG files.)
#' 
#' @section Displaying summary information:
#' The \code{summary} parameter allows you to specify information to display
#' in each node. The parameter can be specified as a vector of character strings,
#' where each element represents a different variable to summarize.
#' When an element of \code{summary} is specified as a single variable name, 
#' the following default set of summary statistics is shown:
#' the variable name, number of missing values, mean and standard deviation,
#' median and interquartile range and range.
#' A customized summary is shown when an element of \code{summary}
#' is specified as a character string with the following structure:
#' \itemize{
#'   \item{First, the name of the variable for which a summary is desired.}
#'   \item{Next a space.}
#'   \item{The remainder of the string specifies what to display, with text as well as special codes (see \strong{Summary codes} below) to indicate the type of summary desired and to control which nodes display the summary, etc.}
#' }
#'
#' @section Summary codes:
#' \itemize{
#'  \item{\code{\%mean\%} }{mean. Variant \code{\%meanx\%} does not report missing values.}
#'  \item{\code{\%SD\%} }{standard deviation. Variant \code{\%SDx\%} does not report missing values.}
#'  \item{\code{\%sum\%} }{sum. Variant \code{\%sumx\%} does not report missing values.}
#'  \item{\code{\%min\%} }{minimum. Variant \code{\%minx\%} does not report missing values.}
#'  \item{\code{\%max\%} }{maximum. Variant \code{\%maxx\%} does not report missing values.}
#'  \item{\code{\%pX\%} }{Xth percentile, e.g. p50 means the 50th percentile}
#'  \item{\code{\%median\%} }{median. Variant \code{\%medianx\%} does not report missing values.}
#'  \item{\code{\%IQR\%} }{interquartile range. Variant \code{\%IQRx\%} does not report missing values.}
#'  \item{\code{\%npct\%} }{number and percentage of \code{TRUE} values}
#'  \item{\code{\%pct\%} }{percentage of \code{TRUE} values}
#'  \item{\code{\%freqpct\%} }{frequency and percentage of values of a variable. Variant \code{\%freqpct_\%} shows each value on a separate line }
#'  \item{\code{\%freq\%} }{frequency of values of a variable. Variant \code{\%freq_\%} shows each value on a separate line}
#'  \item{\code{\%list\%} }{list of the individual values. Variant \code{\%list_\%} shows each value on a separate line}
#'  \item{\code{\%mv\%} }{the number of missing values}
#'  \item{\code{\%nonmv\%} }{the number of non-missing values}
#'  \item{\code{\%v\%} }{the name of the variable}
#'  \item{\code{\%noroot\%} }{flag: Do not show summary in the root node.}
#'  \item{\code{\%leafonly\%} }{flag: Only show summary in leaf nodes.}
#'  \item{\code{\%var=}V\code{\%} }{flag: Only show summary in nodes of variable V.}
#'  \item{\code{\%node=}N\code{\%} }{flag: Only show summary in nodes with value N.}
#'  \item{\code{\%trunc=}n\code{\%} }{flag: Truncate the summary to the first n characters.}
#' }
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
#' 
#' The following palettes
#' (obtained from \code{RColorBrewer}) are used in the order indicated:
#' 
#' \tabular{rlcrlcrlcrl}{
#'  1 \tab  Reds        \tab \tab 5 \tab  Purples \tab \tab 9  \tab YlOrBr \tab \tab 13 \tab RdYlGn \cr
#'  2 \tab  Blues       \tab \tab 6 \tab  YlGn    \tab \tab 10 \tab PuBuGn \tab \tab 14 \tab Set1 \cr 
#'  3 \tab  Greens      \tab \tab 7 \tab  PuBu    \tab \tab 11 \tab BuPu   \tab \tab    \tab \cr
#'  4 \tab  Oranges     \tab \tab 8 \tab PuRd     \tab \tab 12 \tab YlOrRd \tab \tab    \tab \cr
#' }
#'
#' @examples
#' 
#' # Call to vtree 
#' vtree(FakeData,"Sex Severity")
#' 
#' # R Markdown inline call to vtree
#' # `r vtree(FakeData,"Sex Severity")`
#'
#' # A single-level hierarchy
#' vtree(FakeData,"Severity")
#'
#' # A two-level hierarchy
#' vtree(FakeData,"Severity Sex")
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
#' # Adding text to specified nodes of a tree ("targeted text")
#' vtree(FakeData,"Severity Sex",ttext=list(
#'   c(Severity="Severe",Sex="M",text="\nMales with Severe disease"),
#'   c(Severity="NA",text="\nUnknown severity")))
#'
#' @export

vtree <- function (z, vars, auto=FALSE, splitspaces=TRUE,
  prune=list(), prunebelow = list(), keep=list(), follow=list(),
  prunelone=NULL,pruneNA=FALSE,prunesmaller=NULL,
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
  seq=FALSE, pattern=FALSE, ptable=FALSE,
  showroot=TRUE,
  text = list(),ttext=list(),
  plain = FALSE, squeeze = 1,
  showvarinnode=FALSE,shownodelabels=TRUE,
  showvarnames = TRUE, showlevels = TRUE,
  showpct=TRUE, showlpct=TRUE,
  showcount=TRUE, showlegend=FALSE,
  varnamepointsize = 18,
  legendpointsize = 14,
  HTMLtext = FALSE,
  digits = 0,cdigits=1,
  splitwidth = 20, lsplitwidth=15,
  getscript = FALSE,
  nodesep = 0.5, ranksep = 0.5, margin=0.2, vp = TRUE,
  horiz = TRUE, summary = "", runsummary = NULL, retain=NULL,
  width=NULL,height=NULL,
  graphattr="",nodeattr="",edgeattr="",
  color = c("blue", "forestgreen", "red", "orange", "pink"), colornodes = FALSE,
  mincount=1,maxcount,
  showempty = FALSE, rounded = TRUE,
  nodefunc = NULL, nodeargs = NULL, 
  choicechecklist = TRUE,
  arrowhead="normal",
  pxwidth,pxheight,imagewidth="",imageheight="",folder,
  pngknit=TRUE,as.if.knit=FALSE,
  maxNodes=1000,
  unchecked=c("0","FALSE","No","no"),
  checked=c("1","TRUE","Yes","yes"),
  just="c",
  verbose=FALSE,
  parent = 1, last = 1, root = TRUE)
{
  
  makeHTML <- function(x) {
    if (is.list(x)) {
      lapply(x, convertToHTML,just=just)
    }
    else {
      convertToHTML(x,just=just)
    }
  }
  makeHTMLnames <- function(x) {
    if (is.list(x)) {
      x <- lapply(x,
        function(u) {
          names(u) <- convertToHTML(names(u),just=just)
          u
        })
    }
    else {
      names(x) <- convertToHTML(names(x),just=just)
    }
    x
  }
  
  if (HTMLtext) {
    sepN <- "<BR/>"
  } else {
    sepN <- "\n"
  }  

  novars <- FALSE

  ### ----------- Begin code for root only ------------

  if (root) {

    unknowncolor <- "pink"

    argname <- sapply(as.list(substitute({z})[-1]), deparse)

    # Check some inputs
    if (!is.logical(splitspaces)) stop("splitspaces must be TRUE or FALSE")
    
    if (ptable & !(pattern | seq | check.is.na)) {
      pattern <- TRUE
    }

    if (!auto) {
      if (missing(vars)) {
        # Special case where z is provided as a vector instead of a data frame
        if (!is.data.frame(z)) {
          z <- data.frame(z)
          colnames(z)[1] <- argname
          vars <- argname
        } else {
          novars <- TRUE
          vars <- ""
        }
      } else
      if (length(vars)==1) {
        if (!is.na(vars) & vars=="") {
          novars <- TRUE
        } else
        if (splitspaces) {
          vars <- strsplit(vars,"\\s+")[[1]]
          # In case the first element is empty
          # (due to whitespace at the beginning of the string)
          if (vars[1]=="") vars <- vars[-1]
        }
      }
    }

    
    if (auto) {
      if (missing(showvarinnode) & !check.is.na) showvarinnode <- TRUE
      vars <- c()
      non_discrete_vars <- c()
      
      for (candidate in names(z)) {
        if (length(unique(z[[candidate]]))<5) {
          vars <- c(vars,candidate)
        } else {
          non_discrete_vars <- c(non_discrete_vars,candidate)
        }
      }
  
      # Calculate a quick approximation to the cumulative number of nodes
      nodes <- 1
      level <- 1
      excluded_discrete_vars <- c()
      while (level<=length(vars)) {
        nodes <- nodes*length(unique(z[[vars[level]]]))
        if (nodes>maxNodes) {
          ev <- vars[-seq_len(level)]
          vars <- vars[seq_len(level)]
          excluded_discrete_vars <- c(ev,excluded_discrete_vars)
          break
        }
        level <- level+1
      }
      if (verbose) message("--Discrete variables included: ",paste(vars,collapse=" "))
      if (verbose && length(excluded_discrete_vars)>0) 
        message("--Discrete variables excluded: ",paste(excluded_discrete_vars,collapse=" "))
      if (verbose && length(non_discrete_vars)>0)
        message("Additional variables excluded: ",paste(non_discrete_vars,collapse=" "))
    }
     
    # -------------------------------------------------------------------------
    # Variable specifications
    # -------------------------------------------------------------------------
    
    if (!(all(vars==""))) {
 
      # Process = tag in variable names 
      findequal <- grep("=",vars)
      if (length(findequal)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findequal) {
            equalvar <- sub("(\\S+)(=)(\\S+)","\\1",vars[i])
            if (is.null(z[[equalvar]]))
              stop(paste("Unknown variable:",equalvar))                    
            equalval <- sub("(\\S+)(=)(\\S+)","\\3",vars[i])
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
      
      # Process > tag in variable names
      findgt <- grep(">",vars)
      if (length(findgt)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findgt) {
            gtvar <- sub("(\\S+)(>)(\\S+)","\\1",vars[i])
            if (is.null(z[[gtvar]]))
              stop(paste("Unknown variable:",gtvar))                    
            gtval <- sub("(\\S+)(>)(\\S+)","\\3",vars[i])
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
      
      # Process < tag in variable names
      findlt <- grep("<",vars)
      if (length(findlt)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findlt) {
            ltvar <- sub("(\\S+)(<)(\\S+)","\\1",vars[i])
            if (is.null(z[[ltvar]]))
              stop(paste("Unknown variable:",ltvar))                    
            ltval <- sub("(\\S+)(<)(\\S+)","\\3",vars[i])
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
        for (i in seq_len(length(vars))) {
          if (i %in% findna) {
            navar <- sub("^is\\.na:(\\S+)$","\\1",vars[i])
            if (is.null(z[[navar]]))
              stop(paste("Unknown variable:",navar))
            m <- is.na(z[[navar]])
            z[[navar]] <- factor(m, levels = c(FALSE, TRUE),c("not N/A","N/A"))
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
        for (i in seq_len(length(vars))) {
          if (i %in% findstem) {
            stem <- sub("^stem:(\\S+)$","\\1",vars[i])
            expanded_stem <- names(z)[grep(paste0("^",stem,"___[0-9]+.*$"),names(z))]
            # remove any variable name that contains ".factor"
            expanded_stem <- expanded_stem[grep("\\.factor",expanded_stem,invert=TRUE)]
            if (length(expanded_stem)==0) {
              stop(paste0("Could not find variables with names matching the specified stem: ",stem))
            }
            if (verbose) message(paste0(vars[i]," expands to: ",paste(expanded_stem,collapse=", ")))
            rexp0 <- "\\(choice=.+\\)"
            rexp1 <- "(.+) \\(choice=(.+)\\)"
            rexp2 <- "(.+): (.+)"          
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
  
      
      #
      # Process complex variable name specification
      # including REDCap variables, intersections, and wildcards
      #
      regex <- "^([iroa]+:)*([^[:space:]@\\*#]*)([@\\*#]?)$"
      match_regex <- grep(regex,vars)
      if (length(match_regex)>0) {
        expandedvars <- c()
        for (i in seq_len(length(vars))) {
          if (i %in% match_regex) {
            y <- rep("",nrow(z))
            prefix <- sub(regex,"\\1",vars[i])
            text_part <- sub(regex,"\\2",vars[i])
            wildcard <- sub(regex,"\\3",vars[i])
            #browser()
            if (prefix=="" && wildcard=="") {
              expandedvars <- c(expandedvars,vars[i]) 
            } else
            if (prefix=="") {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with matching names")
              }            
              expandedvars <- c(expandedvars,matching_vars)
            } else
            if (prefix=="o:" | prefix=="a:") {
              
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with matching names")
              }      
              if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
              if (prefix=="o:") {
                for (j in 1:length(matching_vars)) {
                  convertedToLogical <- 
                    ifelse(z[[matching_vars[j]]] %in% checked,TRUE,
                      ifelse(z[[matching_vars[j]]] %in% unchecked,FALSE,NA))
                  if (j==1) {
                    output <- convertedToLogical
                  } else {
                   output <- output | convertedToLogical
                  }
                }
                NewVarName <- paste0("Any: ",text_part)
              } else
              if (prefix=="a:") {
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
                NewVarName <- paste0("All: ",text_part)
              } else {
                stop("Unknown prefix")
              }            
              z[[NewVarName]] <- output
              expandedvars <- c(expandedvars,NewVarName)            
              
            } else
            if (prefix=="i:") {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with matching names")
              }
              if (verbose) message(paste0(codevar[i]," expands to: ",paste(matching_vars,collapse=", ")))
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
              expandedvars <- c(expandedvars,newvar)
            } else
            if (wildcard=="@") {
              matching_vars <- names(z)[grep(paste0("^",text_part,"___[0-9]+.*$"),names(z))]
              # remove any variable name that contains ".factor"
              matching_vars <- matching_vars[grep("\\.factor",matching_vars,invert=TRUE)]
              if (length(matching_vars)==0) {
                stop(paste0("Could not find variables with names matching the specified stem: ",text_part))
              }
              if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
              rexp0 <- "\\(choice=.+\\)"
              rexp1 <- "(.+) \\(choice=(.+)\\)"
              rexp2 <- "(.+): (.+)"
              if (prefix=="ra:" || prefix=="ar:") {
                
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
                REDCap_var_label_any <- paste0("All: ",REDCap_var_label)
                z[[REDCap_var_label_any]] <- output
                expandedvars <- c(expandedvars,REDCap_var_label_any)
                
              } else
              if (prefix=="ro:" || prefix=="or:") {
                
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
                      output <- output | convertedToLogical
                    }
                  }
                } 
                REDCap_var_label_any <- paste0("Any: ",REDCap_var_label)
                z[[REDCap_var_label_any]] <- output
                expandedvars <- c(expandedvars,REDCap_var_label_any)
                
              } else                
              if (prefix=="r:") {
                if (choicechecklist) {
                  for (j in 1:length(matching_vars)) {
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
                    z[[choice]] <- z[[matching_vars[j]]]
                    expandedvars <- c(expandedvars,choice)
                  }
                } 
              } else
              if (prefix=="ri:" || prefix=="ir:") {
  
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
                  }            
                }
                y[y %in% ""] <- "*None"
                NewVarName <- paste0("stem:",text_part)
                z[[NewVarName]] <- y
                expandedvars <- c(expandedvars,NewVarName)
              }
            } else 
            if (wildcard=="") {
              if (!(text_part %in% names(z))) {
                stop("Could not find variable named ",text_part)
              }
              if (choicechecklist) {
                rexp1 <- ".+\\(choice=(.+)\\)"
                rexp2 <- ".+: (.+)"
                lab <- attributes(z[[text_part]])$label
                if (length(grep(rexp1,lab))>0) {
                  choice <- sub(rexp1,"\\1",lab)
                } else
                if (length(grep(rexp2,lab))>0) {
                  choice <- sub(rexp2,"\\1",lab)
                } else {
                  stop("Could not find value of checklist item")
                }
                z[[choice]] <- z[[text_part]]
                expandedvars <- c(expandedvars,choice)
              } else {
                expandedvars <- c(expandedvars,text_part)
              }
            }         
          } else {
            expandedvars <- c(expandedvars,vars[i])
          }
        }
        vars <- expandedvars
      }
      
      
      # Process any: tag in variable names to handle REDCap checklists automatically
      findstem <- grep("^any:",vars)
      if (length(findstem)>0) {
        expandedvars <- c()
        for (i in seq_len(length(vars))) {
          if (i %in% findstem) {
            stem <- sub("^any:(\\S+)$","\\1",vars[i])
            expanded_stem <- names(z)[grep(paste0("^",stem,"___[0-9]+.*$"),names(z))]
            # remove any variable name that contains ".factor"
            expanded_stem <- expanded_stem[grep("\\.factor",expanded_stem,invert=TRUE)]
            if (length(expanded_stem)==0) {
              stop(paste0("Could not find variables with names matching the specified stem: ",stem))
            }
            if (verbose) message(paste0(vars[i]," expands to: ",paste(expanded_stem,collapse=", ")))
            anychecked <- rep(FALSE,nrow(z))
            rexp0 <- "\\(choice=.+\\)"
            rexp1 <- "(.+) \\(choice=(.+)\\)"
            rexp2 <- "(.+): (.+)"
            lab1 <- attributes(z[[expanded_stem[1]]])$label
            if (length(grep(rexp0,lab1))>0) {
              REDCap_var_label <- sub(rexp1,"\\1",lab1)
            } else {
              REDCap_var_label <- sub(rexp2,"\\1",lab1)
            }
            if (choicechecklist) {
              for (j in 1:length(expanded_stem)) {
                lab <- attributes(z[[expanded_stem[j]]])$label
                if (length(grep(rexp0,lab))>0) {
                  choice <- sub(rexp1,"\\2",lab)
                } else
                if (length(grep(rexp2,lab))>0) {
                  choice <- sub(rexp2,"\\2",lab)
                } else {
                  stop("Could not find value of checklist item")
                }
                anychecked <- anychecked | z[[expanded_stem[j]]]
              }
            } else {
              for (j in 1:length(expanded_stem)) {
                anychecked <- anychecked | z[[expanded_stem[j]]]
              }
            }
            REDCap_var_label_any <- paste0("Any: ",REDCap_var_label)
            z[[REDCap_var_label_any]] <- anychecked
            expandedvars <- c(expandedvars,REDCap_var_label_any)
          } else {
            expandedvars <- c(expandedvars,vars[i])
          }
        }
        vars <- expandedvars
      }
              
      # Process rc: tag in variable names to handle single REDCap checklist items automatically
      findtag <- grep("^rc:",vars)
      if (length(findtag)>0) {
        expandedvars <- c()
        for (i in seq_len(length(vars))) {
          if (i %in% findtag) {
            rcvar <- sub("^rc:(\\S+)$","\\1",vars[i])
            if (choicechecklist) {
              rexp1 <- ".+\\(choice=(.+)\\)"
              rexp2 <- ".+: (.+)"
              lab <- attributes(z[[rcvar]])$label
              if (length(grep(rexp1,lab))>0) {
                choice <- sub(rexp1,"\\1",lab)
              } else
              if (length(grep(rexp2,lab))>0) {
                choice <- sub(rexp2,"\\1",lab)
              } else {
                stop("Could not find value of checklist item")
              }
              z[[choice]] <- z[[rcvar]]
              expandedvars <- c(expandedvars,choice)
            } else {
              expandedvars <- c(expandedvars,rcvar)
            }
          } else {
            expandedvars <- c(expandedvars,vars[i])
          }
        }
        vars <- expandedvars
      }    
    }
    # end of variable specifications
    
  
    
    if (!missing(showlevels)) showvarnames <- showlevels

    allvars <- vars

    
    # -------------------------------------------------------------------------
    # Set up summaries if requested
    # -------------------------------------------------------------------------
    
    regex <- "^(\\S+)\\s(.+)$"
    if (!all(summary=="")) {
      codevar <- gsub(regex, "\\1", summary)
      summaryvarlist <- headinglist <- as.list(codevar)
      
      summaryformat <- gsub(regex, "\\2", summary)
      summaryformat[grep(regex,summary,invert=TRUE)] <- ""
      summaryformatlist <- as.list(summaryformat)
      
      extra_variables <- NULL

      # Process != tag in variable names in summary argument
      # (Note that this comes before the = tag so that it doesn't match first.)
	    regex <- "^(\\S+)(\\!=)(\\S+)$"
      findequal <- grep(regex,codevar)
      if (length(findequal)>0) {
        for (i in seq_len(length(codevar))) {    
          if (i %in% findequal) {
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
            summaryvarlist[[i]] <- thevar
			      # codevar[i] <- thevar
          }
        }
      }
      
      # Process = tag in variable names in summary argument
      findequal <- grep("=",codevar)
      if (length(findequal)>0) {
        for (i in seq_len(length(codevar))) {    
          if (i %in% findequal) {
            equalvar <- sub("^(\\S+)(=)(\\S+)","\\1",codevar[i])
            if (is.null(z[[equalvar]]))
              stop(paste("Unknown variable in summary:",equalvar))                      
            equalval <- sub("^(\\S+)(=)(\\S+)","\\3",codevar[i])
            # Check to see if any of the values of the specified variable contain spaces.
            # If they do, replace underscores in the specified value with spaces.
            if (any(length(grep(" ",names(table(z[[equalvar]]))))>0)) {
              equalval <- gsub("_"," ",equalval)
            }
            m <- z[[equalvar]]==equalval
            z[[equalvar]] <- m
            summaryvarlist[[i]] <- equalvar
			      # codevar[i] <- equalvar			
          }
        }
      } 
      
      # Process > tag in variable names in summary argument
      findgt <- grep(">",codevar)
      if (length(findgt)>0) {
        for (i in seq_len(length(codevar))) {    
          if (i %in% findgt) {
            gtvar <- sub("(\\S+)(>)(\\S+)","\\1",codevar[i])
            if (is.null(z[[gtvar]]))
              stop(paste("Unknown variable in summary:",gtvar))                             
            gtval <- sub("(\\S+)(>)(\\S+)","\\3",codevar[i])
            # Check to see if any of the values of the specified variable contain spaces
            # If they do, replace underscores in the specified value with spaces.
            if (any(length(grep(" ",names(table(z[[gtvar]]))))>0)) {
              gtval <- gsub("_"," ",gtval)
            }
            m <- z[[gtvar]]>as.numeric(gtval)
            z[[gtvar]] <- m
            summaryvarlist[[i]] <- gtvar
			      # codevar[i] <- gtvar			
          }
        }
      }
      
      # Process < tag in variable names in summary argument
      findlt <- grep("<",codevar)
      if (length(findlt)>0) {
        for (i in seq_len(length(codevar))) {    
          if (i %in% findlt) {
            ltvar <- sub("(\\S+)(<)(\\S+)","\\1",codevar[i])
            if (is.null(z[[ltvar]]))
              stop(paste("Unknown variable in summary:",ltvar))                             
            ltval <- sub("(\\S+)(<)(\\S+)","\\3",codevar[i])
            # Check to see if any of the values of the specified variable contain spaces
            # If they do, replace underscores in the specified value with spaces.
            if (any(length(grep(" ",names(table(z[[ltvar]]))))>0)) {
              ltval <- gsub("_"," ",ltval)
            }
            m <- z[[ltvar]]<as.numeric(ltval)
            z[[ltvar]] <- m
            summaryvarlist[[i]] <- ltvar
			      # codevar[i] <- ltvar			
          }
        }
      } 
      
      # If an element of codevar is not the name of a variable in z,
      # perhaps it's an expression that can be evaluated in z
      for (i in seq_len(length(summaryvarlist))) { 
        if (length(grep("^([oair]+[oair]*:)*(\\S*)([\\*#@])$",summaryvarlist[[i]]))==0) {
          if (length(grep("^stem:",summaryvarlist[[i]]))==0) {   # except for stems
            if (length(grep("^stemc:",summaryvarlist[[i]]))==0) {   # except for stems
              if (length(grep("\\*$",summaryvarlist[[i]]))==0) {   # except for ending in *
               if (length(grep("#$",summaryvarlist[[i]]))==0) {   # except for ending in #
                 if (!(summaryvarlist[[i]] %in% names(z))) {
                   derivedvar <- with(z,eval(parse(text=summaryvarlist[[i]],keep.source=FALSE))) 
                   z[[summaryvarlist[[i]]]] <- derivedvar
                 }
                }
              }
            }
          }
        }
      }      
      
      # Process stem: tag in variable names in summary argument
      regex <- "^stem:(\\S+)$"
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
            summaryvarlist[[i]] <- expandedvars
            headinglist[[i]] <- expandedvars            
            extra_variables <- c(extra_variables,expanded_stem)
            summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(expanded_stem))
          }
        }
      }
      
      # Process stemc: tag in variable names in summary argument
      regex <- "^stemc:(\\S+)$"
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
            summaryvarlist[[i]] <- newvar
            headinglist[[i]] <- ""            
            extra_variables <- c(extra_variables,newvar)
            summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(expanded_stem))
          }
        }
      }      
      
      #
      # Process complex variable summary specification
      # including REDCap variables, intersections, and wildcards
      #
      regex <- "^([oair]+[oair]*:)*([^[:space:]]*)([\\*#@])$"
      match_regex <- grep(regex,codevar)
      if (length(match_regex)>0) {
        for (i in seq_len(length(codevar))) {    
          if (i %in% match_regex) {
            y <- rep("",nrow(z))
            none <- rep(TRUE,nrow(z))
            prefix <- sub(regex,"\\1",codevar[i])
            text_part <- sub(regex,"\\2",codevar[i])
            wildcard <- sub(regex,"\\3",codevar[i])
            if (prefix=="o:" | prefix=="a:") {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard")
              }
              expandedvars <- c()
              if (length(matching_vars)==0) {
                stop("Could not find variables with matching names")
              }      
              if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
              if (prefix=="o:") {
                for (j in 1:length(matching_vars)) {
                  convertedToLogical <- 
                    ifelse(z[[matching_vars[j]]] %in% checked,TRUE,
                      ifelse(z[[matching_vars[j]]] %in% unchecked,FALSE,NA))
                  if (j==1) {
                    output <- convertedToLogical
                  } else {
                    output <- output | convertedToLogical
                  }
                }
                NewVarName <- paste0("Any: ",text_part)
              } else
              if (prefix=="a:") {
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
                NewVarName <- paste0("All: ",text_part)
              } else {
                stop("Unknown prefix")
              }           
              z[[NewVarName]] <- output
              expandedvars <- c(expandedvars,NewVarName)            
                
              newvarheading <- NewVarName
              summaryvarlist[[i]] <- NewVarName
              headinglist[[i]] <- NewVarName          
              extra_variables <- c(extra_variables,NewVarName)
              summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))
            
            } else
            if (prefix=="r:" || prefix=="ir:" || prefix=="ri:" || prefix=="or:" || prefix=="ro:" || prefix=="ar:" || prefix=="ra:") {
              if (wildcard=="@") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"___[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard")
              }
              if (length(matching_vars)==0) {
                stop("summary: Could not find variables with matching names")
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
                summaryvarlist[[i]] <- expandedvars
                headinglist[[i]] <- expandedvars            
                extra_variables <- c(extra_variables,matching_vars)
                summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))
              } else
              if (prefix=="or:" || prefix=="ro:") {
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
                      output <- output | convertedToLogical
                    }
                  }
                } 
                REDCap_var_label_any <- paste0("Any: ",REDCap_var_label)
                z[[REDCap_var_label_any]] <- output
                expandedvars <- c(expandedvars,REDCap_var_label_any)
                summaryvarlist[[i]] <- expandedvars
                headinglist[[i]] <- expandedvars            
                extra_variables <- c(extra_variables,matching_vars)
                summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))
              } else
              if (prefix=="ar:" || prefix=="ra:") {
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
                REDCap_var_label_any <- paste0("All: ",REDCap_var_label)
                z[[REDCap_var_label_any]] <- output
                expandedvars <- c(expandedvars,REDCap_var_label_any)
                summaryvarlist[[i]] <- expandedvars
                headinglist[[i]] <- expandedvars            
                extra_variables <- c(extra_variables,matching_vars)
                summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))
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
                summaryvarlist[[i]] <- newvar
                headinglist[[i]] <- ""            
                extra_variables <- c(extra_variables,newvar)
                summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))                
              }  
            } else {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard")
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
                summaryvarlist[[i]] <- newvar
                headinglist[[i]] <- newvarheading           
                extra_variables <- c(extra_variables,newvar)
                summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))
              } else 
              if (prefix=="") {
                summaryvarlist[[i]] <- matching_vars
                headinglist[[i]] <- matching_vars          
                summaryformatlist[[i]] <- rep(summaryformatlist[[i]],length(matching_vars))
              } else {
                stop("Unknown prefix")
              }
            }
          }
        }
      }            
      
      summaryvars <- unlist(summaryvarlist)
      headings <- unlist(headinglist)
      
      allvars <- c(allvars,summaryvars) 

      if (!is.null(runsummary)) {
        if (length(runsummary) != length(summary)) {
          stop("runsummary argument is not the same length as summary argument.")
        }
      }
      
      nodefunc <- summaryNodeFunction
      nodeargs <- list(
        var = summaryvars, format = unlist(summaryformatlist),
        original_var=headings,
        sf = runsummary, digits = digits, cdigits = cdigits, sepN=sepN)
    }
    # end of section for summary argument
    

    # Add any extra variables needed
    allvars <- c(allvars,retain)

    numvars <- length(vars)
    
    # Each element of the following list
    # is a matrix where the rows are the different hues (one for each variable).
    # The 1st matrix is for a single-valued variable,
    # The 2nd matrix is for a two-valued variable,
    # and so on.
    col <- list(
    rbind(
      c("#DE2D26"),
      c("#3182BD"),
      c("#31A354"),
      c("#E6550D"),
      c("#756BB1"),
      c("#31A354"),
      c("#2B8CBE"),
      c("#DD1C77"),
      c("#D95F0E"),
      c("#1C9099"),
      c("#8856A7"),
      c("#F03B20"),
      c("#91CF60"),
      c("#4DAF4A")
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
      c("#FFEDA0","#F03B20"),
      c("#FC8D59","#91CF60"),
      c("#E41A1C","#4DAF4A")
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
      c("#FFEDA0","#FEB24C","#F03B20"),
      c("#FC8D59","#FFFFBF","#91CF60"),
      c("#E41A1C","#377EB8","#4DAF4A")
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
      c("#FFFFB2","#FECC5C","#FD8D3C","#E31A1C"),
      c("#D7191C","#FDAE61","#A6D96A","#1A9641"),
      c("#E41A1C","#377EB8","#4DAF4A","#984EA3")
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
      c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026"),
      c("#D7191C","#FDAE61","#FFFFBF","#A6D96A","#1A9641"),
      c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00")
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
      c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#F03B20","#BD0026"),
      c("#D73027","#FC8D59","#FEE08B","#D9EF8B","#91CF60","#1A9850"),
      c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33")
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
      c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026"),
      c("#D73027","#FC8D59","#FEE08B","#FFFFBF","#D9EF8B","#91CF60","#1A9850"),
      c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628")
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
      c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026"),
      c("#D73027","#F46D43","#FDAE61","#FEE08B","#D9EF8B","#A6D96A","#66BD63","#1A9850"),
      c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF")
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
      c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026"),
      c("#D73027","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#D9EF8B","#A6D96A","#66BD63","#1A9850"),
      c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
    ))
    
    # Now add some single-shade palettes
    #
    # for (i in seq_len(length(col))) {
    #   singles <- c(
    #     "#FFFFFF","#CCCCCC","#FFCC00","#CC99FF","#6699CC","#33CCFF",
    #     "#FF9933","#CCFFCC","#FFCCCC","#99CC99","#99CCCC") 
    #   for (this in singles) {
    #     col[[i]] <- rbind(col[[i]],rep(this,i))
    #   }
    # }
    
    # Duplicate the color gradients 3 times to allow for huge trees.
    for (i in seq_len(length(col))) {
      col[[i]] <- rbind(col[[i]],col[[i]],col[[i]])
    }

    # When a variable has a single value,
    # should nodes be colored light (1) medium (2) or dark (3)?
    if (singlecolor==1) { col[[1]] <- col[[3]][,1,drop=FALSE] }
    if (singlecolor==2) { col[[1]] <- col[[3]][,2,drop=FALSE] }
    if (singlecolor==3) { col[[1]] <- col[[3]][,3,drop=FALSE] }

    # Identify any "tri:" variables
    tri.variable <- rep(FALSE,length(allvars))
    findtri <- grep("tri:",allvars)
    ALLVARS <- allvars
    if (length(findtri)>0) {
      tri.variable[findtri] <- TRUE
      for (i in seq_len(length(allvars))) {    
        if (i %in% findtri) {
          trivar <- sub("^tri:(\\S+)$","\\1",allvars[i])
          ALLVARS[i] <- trivar
        }
      }
    }

    # Check that all of named variables are in the data frame
    if (novars) ALLVARS <- ALLVARS[ALLVARS!=""]
    findallvars <- ALLVARS %in% names(z)
    if (any(!findallvars)) {
      stop("The following variables were not found in the data frame: ",
        paste(ALLVARS[!findallvars], collapse = ", "))
    }

    # Use a data frame that *only* contains the variables of interest.
    # This greatly speeds things up!
    z <- z[ALLVARS]
    
    if (Venn) {
      if (missing(shownodelabels)) shownodelabels <- FALSE
      if (missing(showpct)) showpct <- FALSE
      if (missing(showlegend)) showlegend <- FALSE
      if (missing(showlpct)) showlpct <- FALSE
    }

    if (check.is.na) {
      if (missing(pattern)) pattern <- TRUE
      if (missing(shownodelabels)) shownodelabels <- FALSE
    }

    if (length(labelvar) > 0) {
        namesvarheaders <- names(labelvar)
        labelvar <- splitlines(labelvar, splitwidth, sp = sepN, at = c(" ", ".", "-", "+", "_", "=", "/"))
        names(labelvar) <- namesvarheaders
    }

    if (!missing(labelnode) && !is.list(labelnode)) stop("labelnode must be a list.")

    if (length(labelnode) > 0) {
      for (i in seq_len(length(labelnode))) {
        names(labelnode[[i]]) <- splitlines(names(labelnode[[i]]),splitwidth,sp =sepN, at=" ")
      }
    }

    if (check.is.na) {
      OLDVARS <- vars
      NEWVARS <- c()
      for (v in vars) {
        newvar <- paste0("MISSING_", v)
        m <- is.na(z[[v]])
        z[[newvar]] <- factor(m, levels = c(FALSE, TRUE),c("not N/A","N/A"))
        # Note that available comes before N/A in alphabetical sorting.
        # Similarly FALSE comes before TRUE.
        # And 0 (representing FALSE) comes before 1 (representing TRUE) numerically.
        # This is convenient, especially when when using the seq parameter.
        NEWVARS <- c(NEWVARS, newvar)
      }
      vars <- NEWVARS
    }
    
    if (pattern | seq) {
      if (missing(showroot)) showroot <- FALSE
      if (pattern) {
        edgeattr <- paste(edgeattr,"arrowhead=none")
      }
      for (i in 1:length(vars)) {
        if (i==1) {
          PATTERN <- paste(z[[vars[i]]])
        } else {
          PATTERN <- paste(PATTERN,z[[vars[i]]])
        }
      }
      TAB <- table(PATTERN)
      
      if (!missing(maxcount)) {
        TAB <- TAB[TAB<=maxcount]
      } else {
        TAB <- TAB[TAB>=mincount]
      }
      
      if (showroot) {
        PATTERN_levels <- names(sort(TAB))
      } else {
        o <- order(as.numeric(TAB),tolower(names(TAB)),decreasing=TRUE)
        PATTERN_levels <- names(TAB)[o]
      }
      
      select <- PATTERN %in% PATTERN_levels
      PATTERN <- PATTERN[select]
      
      z <- z[select,,drop=FALSE]
      
      #PATTERN[!(PATTERN) %in% PATTERN_levels] <- "Other"
      #PATTERN_levels <- c(PATTERN_levels,"Other")
      PATTERN_values <- data.frame(matrix("",nrow=length(PATTERN_levels),ncol=length(vars)),
        stringsAsFactors=FALSE)
      
      names(PATTERN_values) <- vars
      for (i in seq_len(length(PATTERN_levels))) {
        patternRow <- z[PATTERN==PATTERN_levels[i],,drop=FALSE]
        for (j in 1:length(vars)) {
          PATTERN_values[[vars[j]]][i] <- as.character(patternRow[[vars[j]]][1])
        }
      }
      PATTERN <- factor(PATTERN,levels=PATTERN_levels)
      
      if (pattern) {
        z$pattern <- PATTERN
        vars <- c("pattern",vars)
      } else {
        z$sequence <- PATTERN
        vars <- c("sequence",vars)
      }
      tri.variable <- c(tri.variable,FALSE)
      if (check.is.na) {
        OLDVARS <- c("pattern",OLDVARS)
      }
      numvars <- length(vars)
      if (pattern) {
        if (missing(showcount)) showcount <- c(pattern=TRUE)
        if (missing(showpct)) showpct <- c(pattern=TRUE)
        if (missing(shownodelabels)) shownodelabels <- c(pattern=FALSE)
      } else {
        if (missing(showcount)) showcount <- c(sequence=TRUE)
        if (missing(showpct)) showpct <- c(sequence=TRUE)
        if (missing(shownodelabels)) shownodelabels <- c(sequence=FALSE)
      }
    } else {
      if (arrowhead!="normal") {
        edgeattr <- paste(edgeattr,paste0("arrowhead=",arrowhead))
      }
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

    findvars <- names(shownodelabels) %in% vars
    if (is.null(names(shownodelabels))) {
      shownodelabels <- rep(shownodelabels[1],numvars)
      names(shownodelabels) <- vars
    } else {
      if (any(!findvars)) {
        stop("The following variables named in shownodelabels were not in vars: ",
          paste(names(shownodelabels)[!findvars], collapse = ", "))
      }
      if (all(shownodelabels)) {
        sn <- rep(FALSE,numvars)
        names(sn) <- vars
      } else
      if (all(!shownodelabels)) {
        sn <- rep(TRUE,numvars)
        names(sn) <- vars
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

    holdvarlabelcolors <- FALSE
    if (missing(fillcolor)) {
      varlabelcolors <- rep(unknowncolor,numvars)
    } else {
      varlabelcolors <- fillcolor
      varlabelcolors[fillcolor=="white"] <- "black"  # So that varlabels are visible on a white background
      if (singleColor || plain) varlabelcolors[TRUE] <- "black"
      holdvarlabelcolors <- TRUE
    }
    
    if (!is.null(palette)) {
      if (length(palette)==1) {
        if (is.null(names(palette))) {
          # Use the specified palette for all variables.
          palette <- rep(palette,numvars)
          names(palette) <- vars
          if (missing(rootfillcolor)) rootfillcolor <- col[[1]][palette,1]
        }
      } else {
        if (length(vars)<=length(palette)) {
          names(palette) <- c(vars,rep("NoVariable",length(palette)-length(vars)))
        } else {
          names(palette) <- vars[seq_len(length(palette))]
        }
      }
    }

    if (!plain) {
      FC <- vector("list",numvars)
      names(FC) <- vars
      numPalettes <- nrow(col[[1]])
      for (i in seq_len(numvars)) {
        if (tri.variable[i]) {
          thisvar <- factor(c("low","mid","high","NA"),levels=c("high","mid","low","NA"))
        } else {
          thisvar <- z[[vars[i]]]
        }
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
          if (missing(fillcolor) & (Nnonmissing>length(col) || (seq & (vars[i]=="sequence")) || (pattern & (vars[i]=="pattern")) || (row==0))) {
            # Too many values to permit distinct colors
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
              if (!holdvarlabelcolors) {
                varlabelcolors[i] <- fillcolor[names(fillcolor)==vars[i]]
              }
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
      colorIndex <- rep(1:numPalettes,length.out=numvars)
      names(varlabelcolors) <- vars
      if (check.is.na) {
        names(varlabelcolors) <- OLDVARS
      }
    }
    
    # If fillcolor isn't a list, create a list
    if (!is.list(fillcolor)) {
      FC <- vector("list",numvars)
      names(FC) <- vars
      for (i in seq_len(length(vars))) {
        values <- names(table(z[[vars[i]]],exclude=NULL))
        values[is.na(values)] <- "NA"
        valuecolors <- rep(fillcolor[i],length(values))
        names(valuecolors) <- values
        FC[[vars[i]]] <- valuecolors
      }
      fillcolor <- FC
    }
    
    z_names <- names(z)

    # Special case with a single variable being relabled and variable name not specified
    if (!missing(labelvar) && is.null(names(labelvar))) {
      if ((numvars==1) && (length(labelvar)==1)) {
        names(labelvar) <- z_names
      }
    }
  
    findvars <- names(labelvar) %in% z_names
    if (verbose && any(!findvars)) {
      message("The following variables named in labelvar were not found in vars: ",
        paste(names(labelvar)[!findvars], collapse = ", "))
    }
  
    findvars <- names(prunebelow) %in% z_names
    if (verbose && any(!findvars)) {
      message("The following variables named in prunebelow were not found in vars: ",
        paste(names(prunebelow)[!findvars], collapse = ", "))
    }
  
    findvars <- names(prune) %in% z_names
    if (verbose && any(!findvars)) {
      message("The following variables named in prune were not found in vars: ",
        paste(names(prune)[!findvars], collapse = ", "))
    }
  
    findvars <- names(follow) %in% z_names
    if (verbose && any(!findvars)) {
      message("The following variables named in follow were not found in vars: ",
          paste(names(follow)[!findvars], collapse = ", "))
    }
    
    findvars <- names(keep) %in% z_names
    if (verbose && any(!findvars)) {
      message("The following variables named in keep were not found in vars: ",
        paste(names(keep)[!findvars], collapse = ", "))
    }
    
  
    if (length(prune)>0 && (!is.list(prune) || is.null(names(prune)))) {
      stop("The argument of prune should be a named list.")
    }
    
    if (length(prunebelow)>0 && (!is.list(prunebelow) || is.null(names(prunebelow)))) {
      stop("The argument of prunebelow should be a named list.")
    }    
    
    if (length(follow)>0 && (!is.list(follow) || is.null(names(follow)))) {
      stop("The argument of follow should be a named list.")
    }
    
    if (length(keep)>0 && (!is.list(keep) || is.null(names(keep)))) {
      stop("The argument of keep should be a named list.")
    }        
    
  }

  
  ### -------------------------------------------------------------------  
  ### --------------------- End code for root only ----------------------
  ### -------------------------------------------------------------------  
  
  numvars <- length(vars)

  # Node outline colors
  if (!colornodes) color <- rep("black", 100)

  if (is.null(z) || is.null(vars)) {
    #cat("Return NULL because z is NULL or vars is NULL\n")
    return(NULL)
  }
  
  if (nrow(z) == 0 || numvars == 0) {
    #cat("Return NULL because z is empty or vars has zero length\n")
    return(NULL)
  }
  
  # Process tri: tag in variable names 
  actualvarname <- vars[1]
  findtri <- grep("tri:",vars[1])
  if (length(findtri)>0) {
    trivar <- sub("^tri:(\\S+)$","\\1",vars[1])
    med <- median(z[[trivar]],na.rm=TRUE)
    iqrange <- 
      quantile(z[[trivar]],0.75,na.rm=TRUE)-
      quantile(z[[trivar]],0.25,na.rm=TRUE)
    upper <- med+1.5*iqrange
    lower <- med-1.5*iqrange
    m <- ifelse(z[[trivar]]<lower,"low",
          ifelse(z[[trivar]]>=lower & z[[trivar]]<upper,"mid",
            ifelse(z[[trivar]]>=upper,"high","impossible")))
    z[[vars[1]]] <- factor(m,levels=c("high","mid","low"))
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
    summarytext <- vector("list",length=length(CAT))
    names(summarytext) <- CAT
    for (value in CAT) {
      zselect <- z[qqq == value,,drop=FALSE]
      for (i in seq_len(ncol(zselect))) {
        attr(zselect[,i],"label") <- attr(z[,i],"label")
      }
      summarytext[[value]] <- nodefunc(zselect, vars[1], value, args = nodeargs)
      nodetext <- paste0(summarytext[[value]],collapse="")
      nodetext <- splitlines(nodetext, width = splitwidth, sp = sepN, at=" ")
      ThisLevelText <- c(ThisLevelText, paste0(nodetext,sepN))
    }
    if (root) {
      topnodeargs <- nodeargs
      topnodeargs$root <- TRUE
      topnodeargs$leaf <- FALSE
      overallsummary <- nodefunc(z, "", value = NA, args = topnodeargs)
      nodetext <- paste0(overallsummary,collapse="")
      nodetext <- splitlines(nodetext, width = splitwidth,sp = sepN, at=" ")
      TopText <- paste0(nodetext,sepN)
    }
    names(ThisLevelText) <- CAT
  } else {
    ThisLevelText <- text[[vars[1]]]
    summarytext <- NULL
  }
  
  if (pattern & vars[1]!="pattern") ThisLevelText <- ""
  if (seq  & vars[1]!="sequence") ThisLevelText <- ""

  if (novars) {
    zvalue <- rep(0,nrow(z))
    showCOUNT <- showcount
    showPCT <- showpct
  } else {
    zvalue <- z[[vars[1]]]
    showCOUNT <- showcount[[vars[1]]]
    showPCT <- showpct[vars[1]]
  }
  
  fc <- flowcat(zvalue, root = root, novars=novars, title = title, parent = parent,
    var=vars[[1]],
    last = last, labels = labelnode[[vars[1]]], tlabelnode=tlabelnode, labelvar = labelvar[vars[1]],
    varminwidth=varminwidth[vars[1]],varminheight=varminheight[vars[1]],varlabelloc=varlabelloc[vars[1]],
    check.is.na=check.is.na,
    sameline=sameline,
    showvarinnode=showvarinnode,shownodelabels=shownodelabels[vars[1]],
    showpct=showPCT,
    showcount=showCOUNT,
    prune=prune[[vars[1]]],
    prunelone=prunelone,
    prunesmaller=prunesmaller,
    HTMLtext = HTMLtext, showvarnames = showvarnames,
    keep=keep[[vars[1]]],
    pruneNA=pruneNA,
    text = ThisLevelText, ttext=ttext,TopText = TopText, digits = digits, cdigits = cdigits,
    splitwidth = splitwidth, showempty = showempty, topcolor = color[1],
    color = color[2], topfillcolor = rootfillcolor, fillcolor = fillcolor[[vars[1]]],
    vp = vp, rounded = rounded, just=just, showroot=showroot,verbose=verbose)
  
  if (length(fc$nodenum)>0 && fc$nodenum[length(fc$nodenum)]>maxNodes) {
    stop(
      "Too many nodes. ",
      "Specify different variables ",
      "or change maxNodes parameter (currently set to ",maxNodes,").")
  }
  
  if (root & ptable) {
    if (length(labelvar)>0) {
      for (CNP in colnames(PATTERN_values)) {
        if (CNP %in% names(labelvar)) {
          for (i in 1:length(labelvar)) {
            colnames(PATTERN_values)[colnames(PATTERN_values)==names(labelvar)[i]] <- labelvar[i]
          }
        }
      }
    }
    if (vars[1]=="pattern" | vars[1]=="sequence") {
      patternTable <- data.frame(n=fc$n,pct=fc$pct,
        PATTERN_values[seq_len(length(fc$n)),],check.names=FALSE)
      if (length(summarytext)>0) {
        numsum <- max(sapply(summarytext,length))
        for (j in 1:numsum) {
          patternTable[[paste0("summary_",j)]] <- ""
        }
        for (i in 1:length(summarytext)) {
          sm <- gsub("\n"," ",summarytext[[PATTERN_levels[i]]])
          sm <- gsub("<BR/>"," ",sm)
          for (j in 1:length(sm)) {
            patternTable[[paste0("summary_",j)]][i] <- sm[j]
          }
        }
      }
    } 
  }  
  
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
    condition_to_follow <- 
      !(varlevel %in% prunebelowlevels) & 
      (is.null(followlevels) | (varlevel %in% followlevels)) &
      !(varlevel=="NA" & length(keep)>0 & (!is.null(keep[[CurrentVar]]) & !("NA" %in% keep[[CurrentVar]])))

    if (condition_to_follow) {
      if (varlevel == "NA") {
          select <- is.na(z[[CurrentVar]]) | (!is.na(z[[CurrentVar]]) & z[[CurrentVar]]=="NA")
      }
      else {
          select <- which(z[[CurrentVar]] == varlevel)
      }
      if (length(select)>0 & numvars>=1) {
        zselect <- z[select, , drop = FALSE]
        for (index in seq_len(ncol(zselect))) {
          attr(zselect[[index]],"label") <- attr(z[[index]],"label")
        }
        fcChild <- vtree(zselect,
          vars[-1], auto=FALSE,parent = fc$nodenum[i], last = max(fc$nodenum),
          labelnode = labelnode,
          tlabelnode = TLABELNODE,
          colorvarlabels=colorvarlabels,
          check.is.na=check.is.na,
          showvarinnode=showvarinnode,shownodelabels=shownodelabels,
          showpct=showpct,
          showcount=showcount,
          sameline=sameline, showempty = showempty,
          root = FALSE, prune=prune, prunebelow = prunebelow, prunesmaller=prunesmaller,
          labelvar = labelvar,
          varminwidth = varminwidth, varminheight = varminheight, varlabelloc=varlabelloc,
          prunelone=prunelone,
          nodefunc = nodefunc, nodeargs = nodeargs, digits = digits,
          showvarnames = showvarnames,
          keep=keep,
          follow=follow,
          pruneNA=pruneNA,
          pattern=pattern,seq=seq,
          text = text, ttext=TTEXT,gradient=gradient,
          maxNodes=maxNodes,
          colornodes = colornodes, color = color[-1], fillnodes = fillnodes,
          fillcolor = fillcolor, splitwidth = splitwidth,
          vp = vp, rounded = rounded, just=just,verbose=verbose)
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
        VARS <- convertToHTML(VARS,just=just)
      }

      if (colorvarlabels) {
        colored_VARS <- paste0('<FONT COLOR="',varlabelcolors,'">',"<B>",VARS,'  </B>','</FONT>')
      } else {
        colored_VARS <- VARS
      }
      colored_VARS <- paste0('<FONT POINT-SIZE="',varnamepointsize,'">',colored_VARS,'</FONT>')
      marginalText <- rep("",numvars)
      
      #-------------------------------------------------------------------------
      #-V------------------------------V--------------------------------------V-
      
      if (showroot) {
        NL <- "Node_L0_0 [style=invisible]\n"
      } else {
        NL <- ""
      }
      
      if (rounded) {
        styleString <- ' style="rounded,filled"'
      } else {
        styleString <- ' style=filled'
      }
      
      for (i in seq_len(numvars)) {
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
        
        CAT <- names(categoryCounts)

        # Relabel the nodes if labels have been specified
        labels <- labelnode[[thisvarname]]
        for (label in labels) {
          if (label %in% CAT) {
            m <- match(label,CAT)
            CAT[m] <- names(labels)[labels==label]
          }
        }
        
        
        labels <- paste0(
          'label=<',
          colored_VARS[i],
          '>')        
        
        nlheading <- paste0("Node_L",i,"_0",
         '[',
         labels,
          ' shape=none margin=0]',collapse = '\n')
          
        FILLCOLOR <- fillcolor[[thisvarname]][seq_len(length(categoryCounts))]

        if (HTMLtext) {
          displayCAT <- CAT
        } else {
          displayCAT <- convertToHTML(CAT,just=just)
        }       
        
        legendlabel <- paste0(displayCAT,", ",npctString)
        
        labels <- paste0(
          'label=<<FONT POINT-SIZE="',legendpointsize,'">',
          legendlabel,
          '</FONT>>')        
        
        if (!horiz) {
          labels <- rev(labels)
          FILLCOLOR <- rev(FILLCOLOR)
        }
        
        nl <- paste0("Node_L",i,"_",seq_len(length(categoryCounts)),
          '[',
          labels,
          ' color=',color[i+1],' ',
          styleString,
          ' fillcolor=<',FILLCOLOR,'> height=0]',
          collapse = '\n')
        
        nl_allnodes <- paste0("Node_L",i,"_",seq(0,length(categoryCounts)),collapse=" ")

        if (showlegend) {
          nl <- paste0(
            "subgraph cluster_",i," {\n",
            "style=rounded\n",
            "color=<#bdbdbd>\n",
            "{rank=same"," ",nl_allnodes,"}\n",
            nlheading,
            "\n",
            nl,
            "\n}\n",
            paste0("Node_L",i-1,"_0 -> Node_L",i,"_0 [style=invisible arrowhead=none]\n"))  
        } else {
          link <- paste0("Node_L",i-1,"_0 -> Node_L",i,"_0 [style=invisible arrowhead=none]\n")
          if (i==1 & !showroot) link <- ""
          nl <- paste0(
            nlheading,
            "\n",
            link)
        }
        NL <- paste0(NL,"\n",nl)
        
      #-^------------------------------^--------------------------------------^-       
      #-------------------------------------------------------------------------
          
      }
      
    }
    else {
      NL <- ''
    }
    
    if (ptable) {
      pt <- patternTable[nrow(patternTable):1,]
      rownames(pt) <- NULL
      pt
    } else {    
      if (novars) NL <- ""
      flowchart <- showflow(fc, getscript = getscript, nodesep = nodesep,
        ranksep=ranksep, margin=margin, nodelevels = NL, horiz = horiz,
        width=width,height=height,
        graphattr=graphattr,nodeattr=nodeattr,edgeattr=edgeattr)
      
      if (getscript || !pngknit || (!isTRUE(getOption('knitr.in.progress')) && !as.if.knit)) {
        return(flowchart)
      }
      
      if (is.null(getOption("vtree_count"))) {
        options("vtree_count"=0)
        if (missing(folder)) {
          options("vtree_folder"=tempdir())
        } else {
          options("vtree_folder"=folder)
        }        
      }
      
      options("vtree_count"=getOption("vtree_count")+1)

      filename <- paste0("vtree",getOption("vtree_count"),".png")
      
      if (missing(pxheight)) {
        if (missing(pxwidth)) {
          grVizToPNG(flowchart,width=2000,filename=filename,folder=getOption("vtree_folder"))
        } else {
          grVizToPNG(flowchart,width=pxwidth,filename=filename,folder=getOption("vtree_folder"))
        }
      } else {
        if (missing(pxwidth)) {
          grVizToPNG(flowchart,height=pxheight,filename=filename,folder=getOption("vtree_folder"))
        } else {
          grVizToPNG(flowchart,width=pxwidth,height=pxheight,filename=filename,folder=getOption("vtree_folder"))
        }
      }  
      
      fullpath <- file.path(folder=getOption("vtree_folder"),filename)
    
      embedded <- paste0("![](",fullpath,")")
    
      if (imageheight=="") {
        if (imagewidth=="") {
          result <- paste0(embedded,"{ height=3in }")
        } else {
          result <- paste0(embedded,"{width=",imagewidth,"}")
        }
      } else {
        if (imagewidth=="") {
          result <- paste0(embedded,"{height=",imageheight,"}")
        } else {
          result <- paste0(embedded,"{width=",imagewidth," height=",imageheight,"}")
        }
      }
      
      knitr::asis_output(result)
    }
  } else {
      fc
  }
}


















