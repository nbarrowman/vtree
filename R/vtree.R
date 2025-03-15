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
#' For a comprehensive introduction see the \href{../doc/vtree.html}{vignette}.
#' 
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#' 
#' @seealso 
#' \itemize{
#'   \item \url{https://nbarrowman.github.io/vtree}
#'   \item \url{https://github.com/nbarrowman/vtree}
#'   \item Report bugs at \url{https://github.com/nbarrowman/vtree/issues}
#' }
#' 
"_PACKAGE"
 

#'
#' Draw a variable tree
#'
#' @description
#' Variable trees display information about nested subsets of a data frame,
#' in which the subsetting is defined by the values of categorical variables.
#'
#' @author Nick Barrowman <nbarrowman@cheo.on.ca>
#'
#' @param data             Required: Data frame, or a single vector.
#' @param vars             Required (unless \code{data} is a single vector):
#'                         Variables to use for the tree. Can be 
#'                         (1) a character string of whitespace-separated variable names,
#'                         (2) a vector of variable names,
#'                         (3) a formula without a left-hand side,
#'                         e.g. \code{~ Age + Sex},
#'                         but note that extended variable specifications cannot be used in this case.
#'                         
#' @param showuniform      Show a variable even when it only has one value?
#' @param hideconstant     Hide a variable if its only value is one of the specified strings.
#'
#' @param words            A list of named vectors of values.
#'                         Used to build a variable tree 
#'                         representing all permutations of these values.
#'                         No counts will be shown.
#'                                                                           
#' @param prune,keep,prunebelow,follow
#'                         List of named vectors that specify pruning.
#'                          (see \strong{Pruning} below)
#' @param tprune,tkeep,tprunebelow,tfollow
#'                         List of lists of named vectors that specify "targeted" pruning.
#'                          (see \strong{Pruning} below)
#'                          
#' @param prunesmaller     Prune any nodes with count less than specified number.
#' @param prunebigger      Prune any nodes with count greater than specified number.
#' @param splitspaces      When \code{vars} is a character string,
#'                         split it by spaces to get variable names?
#'                         It is only rarely necessary to use this parameter.
#'                         This should only be \code{FALSE} when a single variable name
#'                         that contains spaces is specified.
#' @param horiz            Should the tree be drawn horizontally?
#'                         (i.e. root node on the left, with the tree growing to the right)
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
#' 
#' @param varlabelloc      A named vector of vertical label locations
#'                         ("t", "c", or "b" for top, center, or bottom, respectively)
#'                         for nodes of each variable.
#'                         (Sets the Graphviz \code{labelloc} attribute.)
#' @param title            Label for the root node of the tree.
#' @param font             Font.
#' @param varnamepointsize Font size (in points) to use when displaying variable names.
#' @param varnamebold      Show the variable name in bold?
#' @param legendpointsize  Font size (in points) to use when displaying legend.
#' @param sameline         Display node label on the same line as the count and percentage?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param check.is.na      Replace each variable named in \code{vars} with a logical vector indicating
#'                         whether or not each of its values is missing?
#' @param summary          A character string used to specify summary statistics to display in the nodes.
#'                         See \strong{Displaying summary information} below for details.
#' @param tsummary         A list of character-string vectors.
#'                         The initial elements of each character string vector point to a specific node.
#'                         The final element of each character string vector is a summary string,
#'                         with the same structure as \code{summary}.
#' @param text             A list of vectors containing extra text to add to
#'                         nodes corresponding to specified values of a specified variable.
#'                         The name of each element of the list
#'                         must be one of the variable names in \code{vars}.
#'                         Each element is a vector of character strings.
#'                         The names of the vector identify the nodes to which the text should be added.
#' @param ttext            A list of vectors, each of which specifies a particular node,
#'                         as well as text to add to that node ("targeted" text).
#'                         The names of each vector specify variable names,
#'                         except for an element named \code{text}, which specifies the text to add.
#' @param HTMLtext         Is the text formatted in HTML?
#' @param splitwidth,vsplitwidth
#'                         The minimum number of characters before an automatic
#'                         linebreak is inserted.
#'                         \code{splitwidth} is for node labels, \code{vsplitwidth} is for variable names.
#' @param vp               Use \emph{valid percentages}?
#'                         Valid percentages are computed by first excluding any missing values,
#'                         i.e. restricting attention to the set of "valid" observations.
#'                         The denominator is thus the number of non-missing observations.
#'                         When \code{vp=TRUE}, nodes for missing values show the number of missing values
#'                         but do not show a percentage;
#'                         all the other nodes show valid percentages.
#'                         When \code{vp=FALSE}, all nodes (including nodes for missing values)
#'                         show percentages of the total number of observations.
#' @param getscript        Instead of displaying the variable tree,
#'                         return the DOT script as a character string?
#' 
#' @param digits,cdigits
#'                         Number of decimal digits to show in percentages (\code{digits})
#'                         and in continuous values displayed via the summary parameter (\code{cdigits}).
#'
#' @param fillnodes        [Color] Fill the nodes with color?
#' @param gradient         [Color] Use gradients of fill color across the values of each variable?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} values for named variables is interpreted as
#'                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} values for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param revgradient      [Color] Should the gradient be reversed (i.e. dark to light instead of light to dark)?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} values for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} values for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param sortfill         [Color] Sort colors in order of node count?
#'                         When a \code{gradient} fill is used, this results in
#'                         the nodes with the smallest counts having the lightest shades
#'                         and the nodes with the largest counts having the darkest shades.
#' @param colorvarlabels   [Color] Color the variable labels?
#' @param fillcolor        [Color] A named vector of colors for filling the nodes of each variable.
#'                         If an unnamed, scalar color is specified,
#'                         all nodes will have this color.
#' @param specfill         [Color] A list with specified color values for specified variables.
#' @param NAfillcolor      [Color] Fill-color for missing-value nodes.
#'                         If \code{NULL}, fill colors of missing value nodes will be consistent
#'                         with the fill colors in the rest of the tree.
#' @param rootfillcolor    [Color] Fill-color for the root node.                         
#' @param palette          [Color] A vector of palette numbers (which can range between 1 and 14).
#'                         The names of the vector indicate the corresponding variable.
#'                         See \strong{Palettes} below for more information.
#' @param singlecolor      [Color] When a variable has a single value,
#'                         this parameter is used to specify whether nodes should have a
#'                         (1) light shade, (2) a medium shade, or (3) a dark shade.
#'                         specify \code{singlecolor=1} to assign a light shade.
#' @param color            [Color] A vector of color names for the \emph{outline} of the nodes in each layer.
#' @param colornodes       [Color] Color the node outlines?
#' @param plain            [Color] Use "plain" settings?
#'                         These settings are as follows: for each variable all nodes are the same color,
#'                         namely a shade of blue (with each successive variable using a darker shade);
#'                         all variable labels are black; and the \code{squeeze} parameter is set to 0.6.
#' 
#' @param width,height
#' Width and height (in pixels) to be passed to \code{DiagrammeR::grViz}.
#'
#'                         
#' @param showpct,showlpct
#' Show percentage? \code{showpct} is for nodes, \code{showlpct} is for legends.                     
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} for named variables is interpreted as
#'                         \code{FALSE} for those variables and TRUE for all others.
#' @param showvarinnode    Show the variable name in each node?
#' @param shownodelabels   Show node labels?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         Otherwise, a named logical vector indicates which variables should have their
#'                         node labels shown.
#'                         If the vector consists of only \code{TRUE} values,
#'                         it is interpreted as \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         Similarly, if the vector consists of only \code{FALSE} values, 
#'                         it is interpreted as \code{FALSE} for those variables and \code{TRUE} for all others.
#'                         
#' @param showvarnames     Show the name of the variable next to each layer of the tree?
#' @param showcount        Show count in each node?
#'                         A single value (with no names) specifies the setting for all variables.
#'                         A logical vector of \code{TRUE} for named variables is interpreted as
#`                         \code{TRUE} for those variables and \code{FALSE} for all others.
#'                         A logical vector of \code{FALSE} for named variables is interpreted as
#'                         \code{FALSE} for those variables and \code{TRUE} for all others.
#' @param prefixcount      Text that will precede each count.
#' @param showrootcount    Should count in root node?
#' @param showlegend       Show legend (including marginal frequencies) for each variable?
#' @param showlegendsum    Show summary information in the legend?
#'                         (Provided \code{summary} has been specified).
#' @param showempty        Show nodes that do not contain any observations?
#'
#' @param seq              Display the variable tree using \emph{sequences}?
#'                         Each unique sequence (i.e. pattern) of values will be shown separately.
#'                         The sequences are sorted from least frequent to most frequent.
#' @param pattern          Display the variable tree using \emph{patterns}?
#'                         These are the same as \code{seq}, but lines without arrows are drawn,
#'                         and instead of a sequence variable, a pattern variable is shown.
#' @param ptable           Generate a pattern table instead of a variable tree? 
#'                         Only applies when \code{pattern=TRUE}.
#' @param showroot         Show the root node?
#'                         When \code{seq=TRUE}, it may be useful to set \code{showroot=FALSE}.
#' @param Venn             Display multi-way set membership information?
#'                         This provides an alternative to a Venn diagram.
#'                         This sets \code{showpct=FALSE} and \code{shownodelabels=FALSE}.
#'                         Assumption: all of the specified variables are logicals or 0/1 numeric variables.
#'                         
#' @param choicechecklist  When REDCap checklists are specified using the \code{stem:} syntax,
#'                         automatically extract the names of choices and use them as variable names? 
#'                         
#' @param mincount,maxcount
#'                         Minimum or maximum count to include in a pattern tree or pattern table.
#'                         (\code{maxcount} overrides \code{mincount}.)
#'                         
#' @param pxwidth,pxheight
#'                         Width and height of the PNG bitmap to be rendered
#'                         when \code{vtree} is called from R Markdown.
#'                         If neither \code{pxwidth} nor \code{pxheight} is specified,
#'                         \code{pxwidth} is automatically set to 2000 pixels.
#'                         
#' @param trim             (LaTeX Sweave only.) Crop the image using a feature
#'                         of \code{\\includegraphics}.
#'                         Vector of bp (big points) to trim in the order
#'                         left, lower, right, upper.
#'                         
#' @param imagewidth,imageheight
#' Character strings representing width and height of the PNG image
#'                         to be rendered when \code{vtree} is called from R Markdown,
#'                         e.g. \code{"4in"}
#'                         If neither \code{imageheight} nor \code{imagewidth} is specified,
#'                         \code{imageheight} is set to 3 inches.
#'                         
#' @param maxNodes         An error occurs if the number of nodes exceeds \code{maxNodes}.
#'                         
#' @param unchecked,checked
#' Vector of character strings interpreted as "unchecked" and "checked" respectively.
#' 
#' @param just             Text justification ("l"=left, "c"=center, "r"=right).
#' @param justtext         Like \code{just}, but only for extra text, like summaries.
#' @param thousands        Thousands separator for big numbers.
#' @param folder,format,imageFileOnly,pngknit
#' Control image file generation.
#' \code{folder}: a path to a folder where image file will be stored.
#' \code{format}: "png" or "pdf" format.
#' \code{imageFileOnly}: should an image file should be produced but not displayed?
#' \code{pngknit}: generate a PNG file when called during knit?
#' (See \strong{Knitr, R Markdown, Sweave} below for more information.)
#'
#' @param auto             Automatically choose variables? (\code{vars} should not be specified)
#' 
#' @param rounded          [Graphviz] Use rounded boxes for nodes?
#'
#' @param varminwidth,varminheight
#' [Graphviz] Named vector of minimum initial widths or heights for nodes of each variable.
#' 
#' \code{varminwidth} sets the Graphviz \code{width} attribute.
#' \code{varminheight} sets the Graphviz \code{height} attribute.
#' 
#' @param squeeze          [GraphViz] The degree (between 0 and 1) to which the tree will be "squeezed".
#'                         This controls two Graphviz parameters: \code{margin} and \code{nodesep}.                         
#' @param arrowhead        [Graphviz] arrowhead style. Defaults to \code{"normal"}.
#'                         Other choices include \code{"none"}, \code{"vee"}.                         
#' @param nodesep,ranksep,margin
#'                         [Graphviz] attributes for node separation amount,
#'                         rank separation amount, and node margin.
#' 
#' @param graphattr,nodeattr,edgeattr
#' [Graphviz] Character string: Graphviz attributes for the graph, node, and edge respectively.
#' 
#' @param nodefunc,nodeargs
#'                         Node function and node arguments (see \strong{Node functions} below).
#' @param verbose          Report additional details?
#' @param runsummary       A list of functions, with the same length as \code{summary}.
#'                         Each function must take a data frame as its sole argument,
#'                         and return a logical value.
#'                         Each string in \code{summary} will only be interpreted if
#'                         the corresponding logical value is \code{TRUE}.
#'                         the corresponding string in \code{summary} will be evaluated.
#' @param retain           Vector of names of additional variables in the data frame that need to be
#'                         available to execute the functions in \code{runsummary}.
#'
#' @param parent,last      [Internal use only.] Node number of parent and last node.
#' 
#' @param root             [Internal use only.] Is this the root node of the tree?
#' @param subset           [Internal use only.] A vector representing the subset of observations.
#' @param numsmallernodes  [Internal use only.] Counting nodes that were suppressed by prunesmaller.
#' @param sumsmallernodes  [Internal use only.] Summing nodes that were suppress by prunesmaller.
#' @param numbiggernodes   [Internal use only.] Counting nodes that were suppressed by prunebigger.
#' @param sumbiggernodes   [Internal use only.] Summing nodes that were suppress by prunebigger.
#' @param as.if.knit       (Deprecated) Behave as if called while knitting?
#' @param prunelone        (Deprecated) A vector of values specifying "lone nodes" (of \emph{any} variable) to prune.
#'                         A lone node is a node that has no siblings (an "only child").
#' @param pruneNA          (Deprecated) Prune all missing values?
#'                         This is problematic because "valid" percentages
#'                         are hard to interpret when NAs are pruned.
#' @param lsplitwidth      (Deprecated) In legends, the minimum number of characters before an automatic
#'                         linebreak is inserted.
#' @param showlevels       (Deprecated) Same as showvarnames.
#' @param z                (Deprecated) This was replaced by the \code{data} parameter
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
#'         
#'         The \code{info} attribute of the return object is a list whose top
#'         level represents the root node of the tree.
#'         Within this list is a list named after the first variable in the tree.
#'         In turn, within this list are lists named after the observed
#'         values of that variable.
#'         In turn, each of these lists is an element named after
#'         the next variable in the tree.
#'         And so on.
#'         The root element as well as each list element named after a value of a variable also 
#'         contains elements \code{.n} (representing the number of observations),
#'         \code{.pct} (representing the percentage), and
#'         \code{.txt} (representing additional text such as summaries).
#'         
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
#' @section Knitr, R Markdown, Sweave:
#' If \code{folder} is not specified and knitting to LaTeX,
#' the folder will be set to the value of \code{knitr::opts_chunk$get("fig.path")}.
#' (If this folder does not exist, it will be created.)
#' If \code{folder} is not specified and knitting to markdown,
#' a temporary folder will be used.
#' 
#' If \code{format} is not specified and knitting is taking place,
#' then a PNG file is generated, unless a LaTeX document is 
#' being generated (e.g. via Sweave), in which case a PDF file is generated.   
#' PNG image files will end in \code{.png}.
#' PDF image files will end in \code{.pdf}.
#'                                                 
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
#' are named \code{vtree001.png}, \code{vtree002.png}, etc.
#' (A custom option \code{vtree_count} is used to automatically keep track of the number of PNG files.)
#' 
#' @section Pruning:
#' Each of the parameters \code{prune}, \code{keep}, \code{prunebelow}, \code{follow}
#' takes a named list of vectors as its argument.
#' Each vector specifies nodes of a variable.
#' \itemize{
#'   \item \code{prune}: which nodes should be pruned.
#'   \item \code{keep}: which nodes should \emph{not} be pruned.
#'   \item \code{prunebelow}: which nodes should have their descendants pruned.
#'   \item \code{follow}: which nodes should \emph{not} have their descendants pruned.
#' }
#' The \code{tprune} parameter specifies "targeted" pruning.
#' Standard pruning removes all nodes with the specified value of the specified variable.
#' The \code{tprune} parameter specifies one or more particular paths from the root of the tree
#' down to a node to be pruned.
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
#'   \item{The remainder of the string specifies what to display, with text as well as special codes (such as \code{\%mean\%}) to indicate the type of summary desired and to control which nodes display the summary, etc. See the vignette for more details.}
#' }
#'
#' @section Palettes:
#' The following palettes
#' (obtained from \code{RColorBrewer}) are used in the order indicated:
#' 
#' \tabular{rlcrlcrlcrlcclcr}{
#'  1 \tab Reds     \tab \tab 4 \tab Oranges  \tab \tab 7  \tab PuBu   \tab \tab 10 \tab PuBuGn \tab \tab 13 \tab RdYlGn \cr
#'  2 \tab Blues    \tab \tab 5 \tab Purples  \tab \tab 8  \tab PuRd   \tab \tab 11 \tab BuPu   \tab \tab 14 \tab Set1   \cr 
#'  3 \tab Greens   \tab \tab 6 \tab YlGn     \tab \tab 9  \tab YlOrBr \tab \tab 12 \tab YlOrRd \tab \tab    \tab        \cr
#' }
#'
#' @seealso
#' \href{../doc/vtree.html}{\code{vignette("vtree")}}
#' 
#' @examples
#' 
#' # Call vtree and give the root node a title
#' vtree(FakeData,"Sex Severity",title="People")
#' 
#' # R Markdown inline call to vtree
#' # `r vtree(FakeData,"Sex Severity")`
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
#' # Use the summary parameter to list ID numbers (truncated to 40 characters) in specified nodes
#' vtree(FakeData,"Severity Sex",summary="id \nid = %list% %var=Severity% %trunc=40%")
#'
#' # Add text to specified nodes of a tree ("targeted text")
#' vtree(FakeData,"Severity Sex",ttext=list(
#'   c(Severity="Severe",Sex="M",text="\nMales with Severe disease"),
#'   c(Severity="NA",text="\nUnknown severity")))
#'
#' @importFrom utils capture.output
#'
#' @export

vtree <- function (
  data=NULL,
  vars,
  showuniform = TRUE,
  hideconstant = NULL,
  words = NULL,
  horiz = TRUE, 
  title = "",
  sameline=FALSE,
  vp = TRUE,
  prune=list(),
  tprune=list(),
  keep=list(),
  tkeep=list(),
  prunebelow = list(),
  tprunebelow = list(),
  follow=list(),
  tfollow=list(),
  prunesmaller=NULL,
  prunebigger=NULL,
  summary =NULL,
  tsummary=NULL,
  shownodelabels=TRUE,
  showvarnames = TRUE, 
  showpct=TRUE, 
  showlpct=TRUE,
  showcount=TRUE,
  prefixcount="",
  showrootcount=TRUE,
  showlegend=FALSE,
  showroot=TRUE,
  showvarinnode=FALSE,
  showlegendsum=FALSE,  
  labelvar = NULL,
  labelnode = list(),
  tlabelnode=NULL,
  digits = 0,
  cdigits=1,  
  fillcolor = NULL,
  specfill = NULL,
  fillnodes = TRUE,
  NAfillcolor="white",
  rootfillcolor="#EFF3FF",
  palette=NULL,
  gradient=TRUE,
  revgradient=FALSE,
  sortfill=FALSE,
  singlecolor=2,
  colorvarlabels=TRUE,
  color = c("blue", "forestgreen", "red", "orange", "pink"), 
  colornodes = FALSE,
  plain = FALSE, 
  Venn = FALSE, 
  check.is.na = FALSE,
  seq=FALSE, 
  pattern=FALSE, 
  ptable=FALSE,
  text = list(),
  ttext=list(),
  varlabelloc=NULL,
  font = "Arial",
  varnamepointsize = 24,
  varnamebold=FALSE,
  legendpointsize = 14,
  HTMLtext = FALSE,
  splitwidth = 20, 
  vsplitwidth=8,
  splitspaces=TRUE,
  getscript = FALSE,
  mincount=1,
  maxcount,
  showempty = FALSE, 
  choicechecklist = TRUE,
  just="c",
  justtext=NULL,
  thousands="",
  folder=NULL,
  format="",
  imageFileOnly=FALSE,
  pngknit=TRUE,
  pxwidth=NULL,
  pxheight=NULL,
  imagewidth="",
  imageheight="",  
  width=NULL,
  height=NULL,  
  maxNodes=1000,
  unchecked=c("0","FALSE","No","no"),
  checked=c("1","TRUE","Yes","yes"),
  trim=NULL,
  rounded = TRUE,
  varminwidth=NULL,
  varminheight=NULL,  
  squeeze = 1,
  arrowhead="normal",
  nodesep = 0.5, 
  ranksep = 0.5, 
  margin = 0.2,   
  graphattr="",
  nodeattr="",
  edgeattr="",  
  nodefunc = NULL, 
  nodeargs = NULL,
  verbose=FALSE,
  runsummary = NULL, 
  retain=NULL,  
  auto=FALSE, 
  parent = 1,
  last = 1,
  root = TRUE,
  subset = 1:nrow(z),
  numsmallernodes = 0,
  sumsmallernodes = 0,
  numbiggernodes = 0,
  sumbiggernodes = 0,  
  as.if.knit=FALSE,
  prunelone=NULL,
  pruneNA=FALSE,
  lsplitwidth=15,
  showlevels = TRUE,
  z=NULL)
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
  
  if (missing(z)) z <- data
  
  # *************************************************************************
  # Begin code for root only ----
  # *************************************************************************
  
  if (root) {
    
    if (!missing(words)) {
      showcount <- FALSE
      showpct <- FALSE
      showrootcount <- FALSE
      data <- expand.grid(words)
      z <- data
      vars <- names(words)
    }    
    
    # *************************************************************************
    ## Begin: Check arguments  ----
    # *************************************************************************

    #if (!is.data.frame(z)) { 
    #  stop("The argument of data must be a data frame.")
    #}
    if (!missing(words) && !is.list(words)) {
      stop("The argument of words must be a list.")
    }
    if (!is.logical(splitspaces)) {
      stop("The argument of splitspaces must be TRUE or FALSE")
    }
    if (!missing(labelnode) && !is.list(labelnode)) {
      stop("The argument of labelnode must be a list.")
    }
    if (!missing(tlabelnode) && !is.list(tlabelnode)) {
      stop("The argument of tlabelnode must be a list.")
    }
    if (length(prune)>0 && (!is.list(prune) || is.null(names(prune)))) {
      stop("The argument of prune should be a named list.")
    }
    if (!missing(tprune) && !is.list(tprune)) {
      stop("The argument of tprune should be a list of lists.")
    }
    if (!missing(tkeep) && !is.list(tkeep)) {
      stop("The argument of tkeep should be a list of lists.")
    }  
    if (!missing(tfollow) && !is.list(tfollow)) {
      stop("The argument of tfollow should be a list of lists.")
    }        
    if (!missing(tprunebelow) && !is.list(tprunebelow)) { 
        stop("The argument of tprunebelow should be a list of lists.")
    }
    if (!missing(tsummary) && (!is.list(tsummary))) {
      stop("The argument of tsummary should be a list")
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
    
    # *************************************************************************
    # End: Check arguments  ----
    # *************************************************************************
    
 
    unknowncolor <- "pink"

    argname <- sapply(as.list(substitute({data})[-1]), deparse)
    
    #
    # Start of section: Show messages about deprecated parameters
    #
    
    if (!missing(prunelone)) {
      message("prunelone is deprecated and will be removed in an upcoming release.")
    }

    if (!missing(pruneNA)) {
      message("pruneNA is deprecated and will be removed in an upcoming release.")
    }
    
    if (!missing(showlevels)) {
      message("showlevels is deprecated and will be removed in an upcoming release. Use showvarnames instead.")
    }
    
    if (!missing(lsplitwidth) & missing(vsplitwidth)) {
      message("lsplitwidth is deprecated and will be removed in an upcoming release. Use vsplitwidth instead")
      vsplitwidth=lsplitwidth
    }
    
    #
    # End of section about deprecated parameters
    #
    

    
    if (is.null(justtext)) justtext <- just
    
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
        if (inherits(vars,"formula")) {  # There is no is.formula function in R
          vars <- all.vars(vars)
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
      layer <- 1
      excluded_discrete_vars <- c()
      while (layer<=length(vars)) {
        nodes <- nodes*length(unique(z[[vars[layer]]]))
        if (nodes>maxNodes) {
          ev <- vars[-seq_len(layer)]
          vars <- vars[seq_len(layer)]
          excluded_discrete_vars <- c(ev,excluded_discrete_vars)
          break
        }
        layer <- layer+1
      }
      if (verbose) message("--Discrete variables included: ",paste(vars,collapse=" "))
      if (verbose && length(excluded_discrete_vars)>0) 
        message("--Discrete variables excluded: ",paste(excluded_discrete_vars,collapse=" "))
      if (verbose && length(non_discrete_vars)>0)
        message("Additional variables excluded: ",paste(non_discrete_vars,collapse=" "))
    }
     
    # *************************************************************************
    # Begin: Variable specifications ----
    # *************************************************************************
    
    #
    # The following complex regular expression is used for both 
    # variable specifications and summary arguments.
    #
    
    regexVarName <- "([a-zA-Z0-9~@#()_|,.]+)"
    regexComplex <- "^((i|r|any|anyx|all|allx|notall|notallx|none|nonex)+:)*([^([:space:]|:)@\\*#]*)([@\\*#]?)(.*)$"
    
    if (!(all(vars==""))) {
 
      # Process != tag in variable names 
      regex <- paste0("^",regexVarName,"(\\!=)",regexVarName)
      findnotequal <- grep(regex,vars)
      if (length(findnotequal)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findnotequal) {
            equalvar <- sub(regex,"\\1",vars[i])
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
      
      # Process = tag in variable names 
      regex <- paste0("^",regexVarName,"(=)",regexVarName)
      findequal <- grep(regex,vars)
      if (length(findequal)>0) {
        for (i in seq_len(length(vars))) {    
          if ((i %in% findequal) && !(i %in% findnotequal)) {
            equalvar <- sub(regex,"\\1",vars[i])
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
      regex <- paste0("^",regexVarName,"(>)",regexVarName)
      findgt <- grep(regex,vars)
      if (length(findgt)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findgt) {
            gtvar <- sub(regex,"\\1",vars[i])
            if (is.null(z[[gtvar]]))
              stop(paste("Unknown variable:",gtvar))
            gtval <- sub(regex,"\\3",vars[i])
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
      regex <- paste0("^",regexVarName,"(<)",regexVarName)
      findlt <- grep(regex,vars)
      if (length(findlt)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findlt) {
            ltvar <- sub(regex,"\\1",vars[i])
            if (is.null(z[[ltvar]]))
              stop(paste("Unknown variable:",ltvar))                    
            ltval <- sub(regex,"\\3",vars[i])
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
      
      # Process > tag in variable names
      regex <- paste0("^",regexVarName,"(>)",regexVarName)
      findgt <- grep(regex,vars)
      if (length(findgt)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findgt) {
            gtvar <- sub(regex,"\\1",vars[i])
            if (is.null(z[[gtvar]]))
              stop(paste("Unknown variable:",gtvar))                    
            gtval <- sub(regex,"\\3",vars[i])
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
      
      # Process >= tag in variable names
      regex <- paste0("^",regexVarName,"(>=)",regexVarName)
      findgte <- grep(regex,vars)
      if (length(findgte)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findgte) {
            gtevar <- sub(regex,"\\1",vars[i])
            if (is.null(z[[gtevar]]))
              stop(paste("Unknown variable:",gtevar))                    
            gteval <- sub(regex,"\\3",vars[i])
            # Check to see if any of the values of the specified variable contain spaces
            # If they do, replace underscores in the specified value with spaces.
            if (any(length(grep(" ",names(table(z[[gtevar]]))))>0)) {
              gteval <- gsub("_"," ",gteval)
            }
            m <- z[[gtevar]]>=as.numeric(gteval)
            z[[gtevar]] <- factor(m, levels = c(FALSE, TRUE),
              c(paste0("<",gteval),paste0(">=",gteval)))
            vars[i] <- gtevar
          }
        }
      }                    
      
      # Process <= tag in variable names
      regex <- paste0("^",regexVarName,"(<=)",regexVarName)
      findlte <- grep(regex,vars)
      if (length(findlte)>0) {
        for (i in seq_len(length(vars))) {    
          if (i %in% findlte) {
            ltevar <- sub(regex,"\\1",vars[i])
            if (is.null(z[[ltevar]]))
              stop(paste("Unknown variable:",ltevar))                    
            lteval <- sub(regex,"\\3",vars[i])
            # Check to see if any of the values of the specified variable contain spaces
            # If they do, replace underscores in the specified value with spaces.
            if (any(length(grep(" ",names(table(z[[ltevar]]))))>0)) {
              lteval <- gsub("_"," ",lteval)
            }
            m <- z[[ltevar]]<=as.numeric(lteval)
            z[[ltevar]] <- factor(m, levels = c(FALSE, TRUE),
              c(paste0(">",lteval),paste0("<=",lteval)))
            vars[i] <- ltevar
          }
        }
      }                          
      
      # Process is.na: tag in variable names to handle individual missing value checks
      regex <- paste0("^is\\.na:",regexVarName,"$")
      findna <- grep(regex,vars)
      if (length(findna)>0) {
        for (i in seq_len(length(vars))) {
          if (i %in% findna) {
            navar <- sub(regex,"\\1",vars[i])
            if (is.null(z[[navar]]))
              stop(paste("Unknown variable:",navar))
            NewVar <- paste0("is.na:",navar)
            m <- is.na(z[[navar]])
            z[[NewVar]] <- factor(m, levels = c(FALSE, TRUE),c("not N/A","N/A"))
            # Note that available comes before N/A in alphabetical sorting.
            # Similarly FALSE comes before TRUE.
            # And 0 (representing FALSE) comes before 1 (representing TRUE) numerically.
            # This is convenient, especially when when using the seq parameter.
            vars[i] <- NewVar
          }
        }
      }
      
      # Process stem: tag in variable names to handle REDCap checklists automatically
      regex <- paste0("^stem:",regexVarName,"$")
      findstem <- grep(regex,vars)
      if (length(findstem)>0) {
        expandedvars <- c()
        for (i in seq_len(length(vars))) {
          if (i %in% findstem) {
            stem <- sub(regex,"\\1",vars[i])
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
      # Uses the same regular expression as for variable specifications,
      # namely the string regex
      
      match_regex <- grep(regexComplex,vars)
      if (length(match_regex)>0) {
        expandedvars <- c()
        #
        # Regular expressions for extracting REDCap checklist choices
        # (to be used a bit later)
        #
        rexp0 <- "\\(choice=.+\\)"
        rexp1 <- "(.+) \\(choice=(.+)\\)"
        rexp2 <- "(.+): (.+)"
        #
        for (i in seq_len(length(vars))) {
          if (i %in% match_regex) {
            y <- rep("",nrow(z))
            prefix <- sub(regexComplex,"\\1",vars[i])
            text_part <- sub(regexComplex,"\\3",vars[i])
            wildcard <- sub(regexComplex,"\\4",vars[i])
            tail <- sub(regexComplex,"\\5",vars[i])
            if (prefix=="" && wildcard=="") {
              expandedvars <- c(expandedvars,vars[i]) 
            } else
            if (prefix=="") {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*",tail,"$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+",tail,"$"),names(z))]
              } else {
                stop("Invalid wildcard in variable specification")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with names matching variable specification")
              }            
              expandedvars <- c(expandedvars,matching_vars)
            } else
            if (prefix=="r:" && (wildcard=="*" || wildcard=="#")) {
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard in variable specification")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with names matching variable specification")
              }              
              if (choicechecklist) {
                for (j in 1:length(matching_vars)) {
                  lab <- attributes(z[[matching_vars[j]]])$label
                  if (length(grep(rexp0,lab))>0) {
                    REDCap_var_label <- sub(rexp1,"\\1",lab)
                    choice <- sub(rexp1,"\\2",lab)
                    if (choice %in% names(z)) {
                      choice <- paste0(choice,".")
                    }
                    z[[choice]] <- z[[matching_vars[j]]]
                  } else
                  if (length(grep(rexp2,lab))>0) {
                    choice <- sub(rexp2,"\\2",lab)
                    if (choice %in% names(z)) {
                      choice <- paste0(choice,".")
                    }                    
                    z[[choice]] <- z[[matching_vars[j]]]
                  } else {
                    #stop("Could not find value of REDCap checklist item in variable specification")
                    choice <- matching_vars[j]
                  }
                  expandedvars <- c(expandedvars,choice)
                }
              } 
            } else   
            if (prefix=="any:"    || prefix=="anyx:"    || 
                prefix=="none:"   || prefix=="nonex:"   ||
                prefix=="all:"    || prefix=="allx:"    ||
                prefix=="notall:" || prefix=="notallx:" ) {
              
              if (wildcard=="*") {
                matching_vars <- names(z)[grep(paste0("^",text_part,".*$"),names(z))]
              } else
              if (wildcard=="#") {
                matching_vars <- names(z)[grep(paste0("^",text_part,"[0-9]+$"),names(z))]
              } else {
                stop("Invalid wildcard in variable specification")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with names matching variable specification")
              }      
              if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
              out <- combineVars(prefix,text_part,matching_vars,checked,unchecked,z)
              output <- out$output
              NewVarName <- out$NewVarName   
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
                stop("Invalid wildcard in variable specification")
              }
              if (length(matching_vars)==0) {
                stop("Could not find variables with names matching variable specification")
              }
              if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
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
                stop(paste0("Could not find variables with names matching variable specification"))
              }
              if (verbose) message(paste0(vars[i]," expands to: ",paste(matching_vars,collapse=", ")))
              if (prefix=="rall:" || prefix=="allr:") {    # with wildcard @
                
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
              if (prefix=="rnotall:" || prefix=="notallr:") {    # with wildcard @
                
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
                REDCap_var_label_any <- paste0("Not all: ",REDCap_var_label)
                z[[REDCap_var_label_any]] <- !output
                expandedvars <- c(expandedvars,REDCap_var_label_any)
                
              } else                
              if (prefix=="rany:" || prefix=="anyr:") {     # with wildcard @
                
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
              if (prefix=="rnone:" || prefix=="noner:") {     # with wildcard @
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
              } else                
              if (prefix=="r:") {                         # with wildcard @
                if (choicechecklist) {
                  for (j in 1:length(matching_vars)) {
                    lab <- attributes(z[[matching_vars[j]]])$label
                    if (length(grep(rexp0,lab))>0) {
                      REDCap_var_label <- sub(rexp1,"\\1",lab)
                      choice <- sub(rexp1,"\\2",lab)
                      if (choice %in% names(z)) {
                        choice <- paste0(choice,".")
                      }                
                      z[[choice]] <- z[[matching_vars[j]]]
                    } else
                    if (length(grep(rexp2,lab))>0) {
                      choice <- sub(rexp2,"\\2",lab)
                      if (choice %in% names(z)) {
                        choice <- paste0(choice,".")
                      }                      
                      z[[choice]] <- z[[matching_vars[j]]]
                    } else {
                      choice <- matching_vars[j]
                    }
                    expandedvars <- c(expandedvars,choice)
                  }
                } 
              } else
              if (prefix=="ri:" || prefix=="ir:") {       # with wildcard @
  
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
                      stop("Could not find value of REDCap checklist item in variable specification")
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
                  stop("Could not find value of REDCap checklist item in variable specification")
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
      
              
      # Process rc: tag in variable names to handle single REDCap checklist items automatically
      regex <- "^rc:(\\S+)$"
      findtag <- grep(regex,vars)
      if (length(findtag)>0) {
        expandedvars <- c()
        for (i in seq_len(length(vars))) {
          if (i %in% findtag) {
            rcvar <- sub(regex,"\\1",vars[i])
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
    
    # *************************************************************************
    # End: Variable specifications ----
    # *************************************************************************
    
  
    
    if (!missing(showlevels)) showvarnames <- showlevels

    allvars <- vars

    
    # *************************************************************************
    # Begin: Summaries  ----
    # *************************************************************************
    
    summaryvarlist <- summaryvaluelist <- headinglist <- summaryformatlist <- list()
    
    if (!is.null(tsummary)) {
      for (TSUMMARY in tsummary) {
        lastTSUMMARY <- TSUMMARY[length(TSUMMARY)]
        result <- parseSummary(z,vars=vars,
          summary=lastTSUMMARY,verbose=verbose,choicechecklist=choicechecklist,
          checked=checked,unchecked=unchecked)
        z <- result$z
        summaryvarlist <- c(summaryvarlist,result$summaryvar)
        summaryvaluelist <- c(summaryvaluelist,TSUMMARY[length(TSUMMARY)-1])
        headingslist <- c(headinglist,result$heading)
        summaryformatlist <- c(summaryformatlist,result$format)
      }
      
      summaryvars <- unlist(summaryvarlist)
      summaryvalues <- unlist(summaryvaluelist)
      headings <- unlist(headingslist)
      
      allvars <- c(allvars,summaryvars) 
      
      if (!is.null(runsummary)) {
        if (length(runsummary) != length(summary)) {
          stop("runsummary argument is not the same length as summary argument.")
        }
      }
      
      nodefunc <- summaryNodeFunction
      nodeargs <- list(
        var = summaryvars, value = summaryvalues, format = unlist(summaryformatlist),
        original_var=headings,
        sf = runsummary, digits = digits, cdigits = cdigits, sepN=sepN, 
        thousands = thousands)
      
    } else
    if (!is.null(summary)) {
      for (SUMMARY in summary) {
        result <- parseSummary(z,vars=vars,
          summary=SUMMARY,verbose=verbose,choicechecklist=choicechecklist,
          checked=checked,unchecked=unchecked)
        z <- result$z
        summaryvarlist <- c(summaryvarlist,result$summaryvar)
        headingslist <- c(headinglist,result$heading)
        summaryformatlist <- c(summaryformatlist,result$format)
      }
      
      summaryvars <- unlist(summaryvarlist)
      headings <- unlist(headingslist)
      
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
        sf = runsummary, digits = digits, cdigits = cdigits, sepN=sepN,
        thousands = thousands)
      
    }  
    
    # *************************************************************************
    # End: Summaries ----
    # *************************************************************************
    

    # Add any extra variables needed
    allvars <- c(allvars,retain)

    numvars <- length(vars)
    
    # *************************************************************************
    # Begin: Color palettes ----
    # *************************************************************************
    
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
        c("#43A2CA"),
        c("#2C7FB8"),
        c("#C51B8A"),
        c("#2CA25F"),
        c("#E34A33"),
        c("#636363")
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
        c("#E0F3DB","#43A2CA"),
        c("#EDF8B1","#2C7FB8"),
        c("#FDE0DD","#C51B8A"),
        c("#E5F5F9","#2CA25F"),
        c("#FEE8C8","#E34A33"),
        c("#F0F0F0","#636363")
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
        c("#E0F3DB","#A8DDB5","#43A2CA"),
        c("#EDF8B1","#7FCDBB","#2C7FB8"),
        c("#FDE0DD","#FA9FB5","#C51B8A"),
        c("#E5F5F9","#99D8C9","#2CA25F"),
        c("#FEE8C8","#FDBB84","#E34A33"),
        c("#F0F0F0","#BDBDBD","#636363")
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
        c("#F0F9E8","#BAE4BC","#7BCCC4","#2B8CBE"),
        c("#FFFFCC","#A1DAB4","#41B6C4","#225EA8"),
        c("#FEEBE2","#FBB4B9","#F768A1","#AE017E"),
        c("#EDF8FB","#B2E2E2","#66C2A4","#238B45"),
        c("#FEF0D9","#FDCC8A","#FC8D59","#D7301F"),
        c("#F7F7F7","#CCCCCC","#969696","#525252")
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
        c("#F0F9E8","#BAE4BC","#7BCCC4","#43A2CA","#0868AC"),
        c("#FFFFCC","#A1DAB4","#41B6C4","#2C7FB8","#253494"),
        c("#FEEBE2","#FBB4B9","#F768A1","#C51B8A","#7A0177"),
        c("#EDF8FB","#B2E2E2","#66C2A4","#2CA25F","#006D2C"),
        c("#FEF0D9","#FDCC8A","#FC8D59","#E34A33","#B30000"),
        c("#F7F7F7","#CCCCCC","#969696","#636363","#252525")
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
        c("#F0F9E8","#CCEBC5","#A8DDB5","#7BCCC4","#43A2CA","#0868AC"),
        c("#FFFFCC","#C7E9B4","#7FCDBB","#41B6C4","#2C7FB8","#253494"),
        c("#FEEBE2","#FCC5C0","#FA9FB5","#F768A1","#C51B8A","#7A0177"),
        c("#EDF8FB","#CCECE6","#99D8C9","#66C2A4","#2CA25F","#006D2C"),
        c("#FEF0D9","#FDD49E","#FDBB84","#FC8D59","#E34A33","#B30000"),
        c("#F7F7F7","#D9D9D9","#BDBDBD","#969696","#636363","#252525")
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
        c("#F0F9E8","#CCEBC5","#A8DDB5","#7BCCC4","#4EB3D3","#2B8CBE","#08589E"),
        c("#FFFFCC","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#0C2C84"),
        c("#FEEBE2","#FCC5C0","#FA9FB5","#F768A1","#DD3497","#AE017E","#7A0177"),
        c("#EDF8FB","#CCECE6","#99D8C9","#66C2A4","#41AE76","#238B45","#005824"),
        c("#FEF0D9","#FDD49E","#FDBB84","#FC8D59","#EF6548","#D7301F","#990000"),
        c("#F7F7F7","#D9D9D9","#BDBDBD","#969696","#737373","#525252","#252525")
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
        c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","#4EB3D3","#2B8CBE","#08589E"),
        c("#FFFFD9","#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#0C2C84"),
        c("#FFF7F3","#FDE0DD","#FCC5C0","#FA9FB5","#F768A1","#DD3497","#AE017E","#7A0177"),
        c("#F7FCFD","#E5F5F9","#CCECE6","#99D8C9","#66C2A4","#41AE76","#238B45","#005824"),
        c("#FFF7EC","#FEE8C8","#FDD49E","#FDBB84","#FC8D59","#EF6548","#D7301F","#990000"),
        c("#FFFFFF","#F0F0F0","#D9D9D9","#BDBDBD","#969696","#737373","#525252","#252525")
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
        c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","#4EB3D3","#2B8CBE","#0868AC","#084081"),
        c("#FFFFD9","#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#253494","#081D58"),
        c("#FFF7F3","#FDE0DD","#FCC5C0","#FA9FB5","#F768A1","#DD3497","#AE017E","#7A0177","#49006A"),
        c("#F7FCFD","#E5F5F9","#CCECE6","#99D8C9","#66C2A4","#41AE76","#238B45","#006D2C","#00441B"),
        c("#FFF7EC","#FEE8C8","#FDD49E","#FDBB84","#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"),
        c("#FFFFFF","#F0F0F0","#D9D9D9","#BDBDBD","#969696","#737373","#525252","#252525","#000000")
      ))
    
    # *************************************************************************
    # End: Color palettes ----
    # *************************************************************************    
    
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
    
    # Check that all of the named variables are in the data frame
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
        labelvar <- splitlines(labelvar, vsplitwidth, sp = sepN, at = c(" ", ".", "-", "+", "_", "=", "/"))
        names(labelvar) <- namesvarheaders
    }
    
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
    
    # *************************************************************************
    # Begin: Process patterns  ----
    # *************************************************************************    
    
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
      
      if (!is.null(prunesmaller)) {
        tabpattern <- table(PATTERN)
        # Uniform variables are defined in terms of the patterns that will be shown
        if (!is.null(hideconstant) || !showuniform) {
          #browser()
          sel <- PATTERN %in% names(tabpattern[tabpattern>=prunesmaller])
          patterns_pruned <- sum(tabpattern[tabpattern>=prunesmaller])
          cases_pruned <- sum(!sel)
          cases_pruned_pct <- round(100*cases_pruned/nrow(z))
          z <- z[sel,]
          PATTERN <- PATTERN[sel]
          if (patterns_pruned==1) {
            description1 <- " pattern was pruned, for a total of "
          } else {
            description1 <- " patterns were pruned, for a total of "
          }                   
          if (cases_pruned==1) {
            description2 <- paste0(
              " case (",cases_pruned_pct,"% of total).")
          } else {
            description2 <- paste0(
              " cases (",cases_pruned_pct,"% of total).")
          }         
          message("Since prunesmaller=",prunesmaller,", ",
            patterns_pruned,description1,cases_pruned,description2)
        }
      }

      if (!is.null(prunebigger)) {
        tabpattern <- table(PATTERN)
        # Uniform variables are defined in terms of the patterns that will be shown
        if (!is.null(hideconstant) || !showuniform) {
          #browser()
          sel <- PATTERN %in% names(tabpattern[tabpattern<=prunebigger])
          patterns_pruned <- sum(tabpattern[tabpattern<=prunebigger])
          cases_pruned <- sum(!sel)
          cases_pruned_pct <- round(100*cases_pruned/nrow(z))
          z <- z[sel,]
          PATTERN <- PATTERN[sel]
          if (patterns_pruned==1) {
            description1 <- " pattern was pruned, for a total of "
          } else {
            description1 <- " patterns were pruned, for a total of "
          }                   
          if (cases_pruned==1) {
            description2 <- paste0(
              " case (",cases_pruned_pct,"% of total).")
          } else {
            description2 <- paste0(
              " cases (",cases_pruned_pct,"% of total).")
          }         
          message("Since prunebigger=",prunebigger,", ",
            patterns_pruned,description1,cases_pruned,description2)
        }
      }    
      
      if (!is.null(hideconstant)) {
        for (var in vars) {
          if (length(unique(z[[var]]))==1) {
            if ((unique(z[[var]]) %in% hideconstant)) {
              message(paste0("Not showing ",var,", since its only value is ",z[[var]][1]))
              vars <- vars[vars!=var]
            }  
          }
        }        
      } else
      if (!showuniform) {
        for (var in vars) {
          #message(var)
          #print(table(z[[var]]))
          if (length(unique(z[[var]]))==1) {
            message(paste0("Not showing ",var,", since the only value is ",z[[var]][1]))
            vars <- vars[vars!=var]
          }
        }
      }
      
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
      
      if (!is.null(hideconstant)) {
        for (var in vars) {
          if (length(unique(z[[var]]))==1) {
            if ((unique(z[[var]]) %in% hideconstant)) {
              message(paste0("Not showing ",var,", since the only value is ",z[[var]][1]))
              vars <- vars[vars!=var]
            }  
          }
        }        
      } else      
      if (!showuniform) {
        for (var in vars) {
          #message(var)
          #print(table(z[[var]]))
          if (length(unique(z[[var]]))==1) {
            message(paste0("Not showing ",var,", since the only value is ",z[[var]][1]))
            vars <- vars[vars!=var]
          }
        }
        if (length(vars)==0) {
          novars <- TRUE
          vars <- ""
        }
      }
    }
    
    # *************************************************************************
    # End: Process patterns  ----
    # *************************************************************************    
    
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
    
    if (is.null(names(sameline))) {
      sameline <- rep(sameline[1],numvars)
      names(sameline) <- vars
    } else {
      if (all(sameline)) {
        sl <- rep(FALSE,numvars)
      } else
      if (all(!sameline)) {
        sl <- rep(TRUE,numvars)
      } else
      if (length(sameline)!=numvars) {
        stop("sameline: ambiguous specification.")
      } else {
        sl <- rep(NA,numvars)
      }
      if (any(names(sameline) %in% vars)) {
        m <- match(names(sameline),vars)
        sl[m[!is.na(m)]] <- sameline[!is.na(m)]
      }
      names(sl) <- vars
      sameline <- sl
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
    if (!is.null(fillcolor) && (length(fillcolor)==1) && (is.null(names(fillcolor)))) {
      singleColor <- TRUE
      fillcolor <- rep(fillcolor,numvars)
      names(fillcolor) <- vars
      if (is.null(rootfillcolor)) rootfillcolor <- fillcolor
    }

    holdvarlabelcolors <- FALSE
    if (is.null(fillcolor)) {
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
          if (is.null(rootfillcolor)) rootfillcolor <- col[[1]][palette,1]
        }
      } else {
        if (length(vars)<=length(palette)) {
          names(palette) <- c(vars,rep("NoVariable",length(palette)-length(vars)))
        } else {
          names(palette) <- vars[seq_len(length(palette))]
        }
      }
    }

    # *************************************************************************
    # Begin: Assign colors  ----
    # *************************************************************************    
    if (!plain) {
      FILLCOLOR <- vector("list",numvars)
      names(FILLCOLOR) <- vars
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
          if (is.null(fillcolor) & (Nnonmissing>length(col) || (seq & (vars[i]=="sequence")) || (pattern & (vars[i]=="pattern")) || (row==0))) {
            # Too many values to permit distinct colors
            valuecolors[values!="NA"] <- col[[1]][row] # "grey90"
            names(valuecolors) <- values
            varlabelcolors[i] <- col[[1]][row] # "grey90"
          } else {
            if (!is.null(fillcolor) && (vars[i] %in% names(fillcolor))) {
              if (is.null(NAfillcolor)) {
                valuecolors[TRUE] <- fillcolor[names(fillcolor)==vars[i]]
              } else {
                valuecolors[values!="NA"] <- fillcolor[names(fillcolor)==vars[i]]
              }
              if (!holdvarlabelcolors) {
                varlabelcolors[i] <- fillcolor[names(fillcolor)==vars[i]]
              }
            } else
            if (!is.null(specfill)) {
              if (is.null(NAfillcolor)) {
                valuecolors[TRUE] <- specfill[[vars[[i]]]]
              } else {
                valuecolors[values!="NA"] <- specfill[[vars[[i]]]]
              }
              varlabelcolors[i] <- "black"              
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
        FILLCOLOR[[vars[i]]] <- valuecolors
      }
      fillcolor <- FILLCOLOR
      colorIndex <- rep(1:numPalettes,length.out=numvars)
      names(varlabelcolors) <- vars
      if (check.is.na) {
        names(varlabelcolors) <- OLDVARS
      }
    }
    # *************************************************************************
    # End: Assign colors  ----
    # *************************************************************************        
    
    # If fillcolor isn't a list, create a list
    if (!is.list(fillcolor)) {
      FILLCOLOR <- vector("list",numvars)
      names(FILLCOLOR) <- vars
      for (i in seq_len(length(vars))) {
        values <- names(table(z[[vars[i]]],exclude=NULL))
        values[is.na(values)] <- "NA"
        valuecolors <- rep(fillcolor[i],length(values))
        names(valuecolors) <- values
        FILLCOLOR[[vars[i]]] <- valuecolors
      }
      fillcolor <- FILLCOLOR
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
    
  }

  # *************************************************************************
  # End code for root only ----
  # *************************************************************************
  

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
  
  tsummaryLen <- sapply(tsummary,length)

  qqq <- z[[vars[1]]]
  qqq <- as.character(qqq)
  qqq[is.na(qqq)] <- "NA"
  categoryCounts <- table(qqq, exclude = NULL)
  CAT <- names(categoryCounts)
  
  ThisLayerText <- rep("",length(CAT))       
  
  # Process summaries to include in nodes  ----
  
  if (any(tsummaryLen==2)) {
    #
    # Prepare targeted summaries
    #
    for (i in seq_len(length(tsummary))) {
      if (tsummaryLen[i]==2) {
        if (numvars == 1)
          nodeargs$leaf <- TRUE
        summarytext <- vector("list",length=length(CAT))
        names(summarytext) <- CAT
        for (k in seq_len(length(CAT))) {
          value <- CAT[k]
          zselect <- z[qqq == value,,drop=FALSE]
          for (j in seq_len(ncol(zselect))) {
            attr(zselect[,j],"label") <- attr(z[,j],"label")
          }
          if (tsummary[[i]][1]==value) {
            choose <- nodeargs$value==value
            if (any(choose)) {
              args <- nodeargs
              args$var <- args$var[choose]
              args$format <- args$format[choose]
              args$value <- args$value[choose] # not really necessary
              summarytext[[value]] <- nodefunc(zselect, vars[1], value, args = args)
              nodetext <- paste0(summarytext[[value]],collapse="")
              nodetext <- splitlines(nodetext, width = splitwidth, sp = sepN, at=" ")
              ThisLayerText[k] <- paste0(ThisLayerText[k], paste0(nodetext,sepN))
            } 
          }
        }
        names(ThisLayerText) <- CAT
      }
    }
  } else
  if (is.null(tsummary) & !is.null(nodefunc)) {
    #
    # Prepare non-targeted summaries
    #    
    if (numvars == 1)
        nodeargs$leaf <- TRUE
    summarytext <- vector("list",length=length(CAT))
    names(summarytext) <- CAT
    for (k in seq_len(length(CAT))) {
      value <- CAT[k]
      zselect <- z[qqq == value,,drop=FALSE]
      for (i in seq_len(ncol(zselect))) {
        attr(zselect[,i],"label") <- attr(z[,i],"label")
      }
      summarytext[[value]] <- nodefunc(zselect, vars[1], value, args = nodeargs)
      nodetext <- paste0(summarytext[[value]],collapse="")
      nodetext <- splitlines(nodetext, width = splitwidth, sp = sepN, at=" ")
      ThisLayerText[k] <- paste0(ThisLayerText[k], paste0(nodetext,sepN))
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
    names(ThisLayerText) <- CAT
  } else {
    ThisLayerText <- text[[vars[1]]]
    summarytext <- NULL
  }
  
  if (pattern & vars[1]!="pattern") ThisLayerText <- ""
  if (seq  & vars[1]!="sequence") ThisLayerText <- ""

  if (novars) {
    zvalue <- rep(0,nrow(z))
    showCOUNT <- showcount
    showPCT <- showpct
    sameLINE <- sameline
  } else {
    zvalue <- z[[vars[1]]]
    showCOUNT <- showcount[[vars[1]]]
    showPCT <- showpct[vars[1]]
    sameLINE <- sameline[vars[1]]
  }
  
  
  # *************************************************************************
  # Build a canopy ----
  # *************************************************************************
  
  tree <- buildCanopy(zvalue, root = root, novars=novars, title = title, parent = parent,
    var=vars[[1]],
    last = last, labels = labelnode[[vars[1]]], tlabelnode=tlabelnode, labelvar = labelvar[vars[1]],
    varminwidth=varminwidth[vars[1]],varminheight=varminheight[vars[1]],varlabelloc=varlabelloc[vars[1]],
    check.is.na=check.is.na,
    sameline=sameLINE,
    showvarinnode=showvarinnode,shownodelabels=shownodelabels[vars[1]],
    showpct=showPCT,
    showrootcount=showrootcount,
    showcount=showCOUNT,
    prefixcount=prefixcount,
    prune=prune[[vars[1]]],
    tprune=tprune,
    prunelone=prunelone,
    prunesmaller=prunesmaller,
    prunebigger=prunebigger,
    HTMLtext = HTMLtext, showvarnames = showvarnames,
    keep=keep[[vars[1]]],
    tkeep=tkeep,
    pruneNA=pruneNA,
    text = ThisLayerText, ttext=ttext,TopText = TopText, digits = digits, cdigits = cdigits,
    splitwidth = splitwidth, showempty = showempty, topcolor = color[1],
    color = color[2], topfillcolor = rootfillcolor, fillcolor = fillcolor[[vars[1]]],
    vp = vp, rounded = rounded, just=just, justtext=justtext, thousands=thousands, showroot=showroot,
    verbose=verbose,sortfill=sortfill)
  
  numsmallernodes <- tree$numsmallernodes
  sumsmallernodes <- tree$sumsmallernodes
  numbiggernodes  <- tree$numbiggernodes
  sumbiggernodes  <- tree$sumbiggernodes
  
  if (root) {
    treedata <- list(.n=nrow(z),.pct=100)
  }
  if (vars[[1]]!="") {
    if (root) {
      treedata <- list(.n=nrow(z),.pct=100)
    } else {
      treedata <- list()
    }
    children <- list()
    for (i in seq_len(length(tree$value))) {
      if (tree$extraText[i]!="") {
        children[[tree$value[i]]] <- list(.n=tree$n[i],.pct=tree$pct[i],.text=tree$extraText[i])
      } else  {
         children[[tree$value[i]]] <- list(.n=tree$n[i],.pct=tree$pct[i])
      }        
    }
    treedata[[vars[1]]] <- children
  }

  if (length(tree$nodenum)>0 && tree$nodenum[length(tree$nodenum)]>maxNodes) {
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
      patternTable <- data.frame(n=tree$n,pct=tree$pct,
        PATTERN_values[seq_len(length(tree$n)),],check.names=FALSE)
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
  
  
  tfollow_this_var <- FALSE
  if (length(tfollow)>0) {
    for (j in seq_len(length(tfollow))) {
      if (length(tfollow[[j]])==1 && any(names(tfollow[[j]])==CurrentVar)) {
        tfollow_this_var <- TRUE
      }
    }
  }
  
  tprunebelow_this_var <- FALSE
  if (length(tprunebelow)>0) {
    for (j in seq_len(length(tprunebelow))) {
      if (length(tprunebelow[[j]])==1 && any(names(tprunebelow[[j]])==CurrentVar)) {
        tprunebelow_this_var <- TRUE
      }
    }
  }  
  
  
  tfollowlevels <- NULL
  tprunebelowlevels <- NULL
  
  
  # *************************************************************************
  # Begin: Loop over variable levels  ----
  # *************************************************************************

  varlevelindex <- 0
  for (varlevel in tree$levels) { 
    
    varlevelindex <- varlevelindex + 1
    
    if (tfollow_this_var) {
      for (j in seq_len(length(tfollow))) {
        tfollowlevels <- c(tfollowlevels,unlist(tfollow[[j]][names(tfollow[[j]])==CurrentVar]))
      }
    }    

    if (tprunebelow_this_var) {
      for (j in seq_len(length(tprunebelow))) {
        tprunebelowlevels <- c(tprunebelowlevels,unlist(tprunebelow[[j]][names(tprunebelow[[j]])==CurrentVar]))
      }
    }
    
    
    # *************************************************************************
    ## Begin: Tracking of targeted nodes ----
    # *************************************************************************    
    
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

    TSUMMARY <- tsummary
    j <- 1
    while (j <= length(TSUMMARY)) {
      if (!any(names(TSUMMARY[[j]])==CurrentVar)) {
        TSUMMARY[[j]] <- ""
      } else {
        if (TSUMMARY[[j]][CurrentVar]==varlevel) {
          TSUMMARY[[j]] <- TSUMMARY[[j]][names(TSUMMARY[[j]])!=CurrentVar]
        } else {
          if (TSUMMARY[[j]][CurrentVar]!=varlevel) {
            TSUMMARY[[j]] <- ""
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

    TPRUNE <- tprune
    j <- 1
    while (j <= length(TPRUNE)) {
      if (!any(names(TPRUNE[[j]])==CurrentVar)) {
        TPRUNE[[j]] <- ""
      } else {
        if (TPRUNE[[j]][CurrentVar]==varlevel) {
          TPRUNE[[j]] <- TPRUNE[[j]][names(TPRUNE[[j]])!=CurrentVar]
        } else {
          if (TPRUNE[[j]][CurrentVar]!=varlevel) {
            TPRUNE[[j]] <- ""
          }
        }
      }
      j <-j + 1
    } 

    TKEEP <- tkeep
    j <- 1
    while (j <= length(TKEEP)) {
      if (!any(names(TKEEP[[j]])==CurrentVar)) {
        TKEEP[[j]] <- ""
      } else {
        if (TKEEP[[j]][CurrentVar]==varlevel) {
          TKEEP[[j]] <- TKEEP[[j]][names(TKEEP[[j]])!=CurrentVar]
        } else {
          if (TKEEP[[j]][CurrentVar]!=varlevel) {
            TKEEP[[j]] <- ""
          }
        }
      }
      j <-j + 1
    } 
    
    TFOLLOW <- tfollow
    j <- 1
    while (j <= length(TFOLLOW)) {
      if (!any(names(TFOLLOW[[j]])==CurrentVar)) {
        TFOLLOW[[j]] <- ""
      } else {
        if (TFOLLOW[[j]][CurrentVar]==varlevel) {
          TFOLLOW[[j]] <- TFOLLOW[[j]][names(TFOLLOW[[j]])!=CurrentVar]
        } else {
          if (TFOLLOW[[j]][CurrentVar]!=varlevel) {
            TFOLLOW[[j]] <- ""
          }
        }
      }
      j <-j + 1
    }     

    TPRUNEBELOW <- tprunebelow
    j <- 1
    while (j <= length(TPRUNEBELOW)) {
      if (!any(names(TPRUNEBELOW[[j]])==CurrentVar)) {
        TPRUNEBELOW[[j]] <- ""
      } else {
        if (TPRUNEBELOW[[j]][CurrentVar]==varlevel) {
          TPRUNEBELOW[[j]] <- TPRUNEBELOW[[j]][names(TPRUNEBELOW[[j]])!=CurrentVar]
        } else {
          if (TPRUNEBELOW[[j]][CurrentVar]!=varlevel) {
            TPRUNEBELOW[[j]] <- ""
          }
        }
      }
      j <-j + 1
    }     
    
    if (tfollow_this_var) {
      followlevels <- tfollowlevels
    }
    
    if (tprunebelow_this_var) {
      prunebelowlevels <- tprunebelowlevels
    }    
    
    # *************************************************************************
    # End: Tracking of targeted nodes ----
    # *************************************************************************    


   
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
        # *************************************************************************
        # Call vtree recursively  ----
        # ************************************************************************* 

        fcChild <- vtree(data=zselect,
          vars=vars[-1], auto=FALSE,parent = tree$nodenum[varlevelindex],
          last = max(tree$nodenum),
          labelnode = labelnode,
          tlabelnode = TLABELNODE,
          colorvarlabels=colorvarlabels,
          check.is.na=check.is.na,
          tsummary=TSUMMARY,
          showvarinnode=showvarinnode,shownodelabels=shownodelabels,
          showpct=showpct,
          showcount=showcount,
          prefixcount=prefixcount,
          sameline=sameline, showempty = showempty,
          root = FALSE,
          prune=prune, prunebelow = prunebelow, tprunebelow=TPRUNEBELOW,
          prunesmaller=prunesmaller,prunebigger=prunebigger,
          tprune=TPRUNE,
          tkeep=TKEEP,
          labelvar = labelvar,
          varminwidth = varminwidth, varminheight = varminheight,
          varlabelloc=varlabelloc,
          prunelone=prunelone,
          nodefunc = nodefunc, nodeargs = nodeargs, digits = digits,
          showvarnames = showvarnames,
          keep=keep,
          follow=follow,
          tfollow=TFOLLOW,
          pruneNA=pruneNA,
          pattern=pattern,seq=seq,
          numsmallernodes=numsmallernodes,sumsmallernodes=sumsmallernodes,
          numbiggernodes=numbiggernodes,sumbiggernodes=sumbiggernodes,
          text = text, ttext=TTEXT,gradient=gradient,sortfill=sortfill,
          maxNodes=maxNodes,
          colornodes = colornodes, color = color[-1], fillnodes = fillnodes,
          fillcolor = fillcolor, splitwidth = splitwidth,
          HTMLtext=HTMLtext,
          vp = vp, rounded = rounded, just=just, justtext=justtext,thousands=thousands,
          verbose=verbose)
        
        if (!is.null(fcChild$treedata)){
          treedata[[vars[1]]][[varlevel]] <- 
            c(treedata[[vars[1]]][[varlevel]],fcChild$treedata)
        }

        tree <- joinflow(tree,fcChild)
        #browser()
      }
    } 
  } 
  tree$treedata <- treedata

  # *************************************************************************
  # End: Loop over variable levels  ----
  # *************************************************************************
  
  if (length(tree$nodenum)==0) {
    tree <- NULL
  }
  
  # *************************************************************************
  # Begin code for root call only  ----
  # *************************************************************************  
  
  if (root) {
    
    if ((!is.null(prunesmaller) | !is.null(prunebigger)) && is.null(hideconstant)) {
      if (tree$numsmallernodes==0) {
        if (!is.null(prunesmaller)) {
          if (pattern) {
            message("No patterns had fewer than ",prunesmaller," cases")
          } else {
            message("No nodes were smaller than ",prunesmaller)
          }
        }
      }
      if (tree$numbiggernodes==0) {
        if (!is.null(prunebigger)) { 
          if (pattern) {
            message("No patterns had more than than ",prunebigger," cases")
          } else {
            message("No nodes were larger than ",prunebigger)
          }        
        }
      } else {
        if (!is.null(prunesmaller)) {
          if (pattern) {
            if (tree$numsmallernodes==1) {
              description <- " pattern was pruned, "
            } else {
              description <- " patterns were pruned, "
            }
            description <- paste0(description,
              "for a total of ",tree$sumsmallernodes," cases",
            " (",round(100*tree$sumsmallernodes/nrow(z)),"% of total)")
          } else {
            if (tree$numsmallernodes==1) {
              description <- " node was pruned."
            } else {
              description <- " nodes were pruned."
            }         
          }
          message("Since prunesmaller=",prunesmaller,", ",
            tree$numsmallernodes,description)
        }
        if (!is.null(prunebigger)) {
          if (pattern) {
            if (tree$numbiggernodes==1) {
              description <- " pattern was pruned, "
            } else {
              description <- " patterns were pruned, "
            }
            description <- paste0(description,
              "for a total of ",tree$sumbiggernodes," cases",
            " (",round(100*tree$sumbiggernodes/nrow(z)),"% of total)")
          } else {
            if (tree$numbiggernodes==1) {
              description <- " node was pruned."
            } else {
              description <- " nodes were pruned."
            }         
          }
          message("Since prunebigger=",prunebigger,", ",
            tree$numbiggernodes,description)
        }          
      }
    }

    NL <- labelsAndLegends(z=z,OLDVARS=OLDVARS,vars=vars,labelvar=labelvar,
      HTMLtext=HTMLtext,vsplitwidth=vsplitwidth,just=just,
      colorvarlabels=colorvarlabels,varnamebold=varnamebold,
      varlabelcolors=varlabelcolors,varnamepointsize=varnamepointsize,
      showroot=showroot,rounded=rounded,numvars=numvars,Venn=Venn,
      labelnode=labelnode,fillcolor=fillcolor,showlegendsum=showlegendsum,
      thousands=thousands,
      splitwidth=splitwidth,nodefunc=nodefunc,nodeargs=nodeargs,
      showvarnames=showvarnames,check.is.na=check.is.na,vp=vp,showlpct=showlpct,
      digits=digits,legendpointsize=legendpointsize,horiz=horiz,color=color,
      showlegend=showlegend,pattern=pattern,sepN=sepN)
    
    
    # *************************************************************************
    ## Outputs ----
    # *************************************************************************   
    
    if (ptable) {
      pt <- patternTable[nrow(patternTable):1,]
      rownames(pt) <- NULL
      pt
    } else {    
      if (novars) NL <- ""
      flowchart <- showflow(tree, getscript = getscript, font = font,
        nodesep = nodesep,
        ranksep=ranksep, margin=margin, nodelevels = NL, horiz = horiz,
        width=width,height=height,
        graphattr=graphattr,nodeattr=nodeattr,edgeattr=edgeattr)
      
      attributes(flowchart)$info <- treedata
      
      if (!imageFileOnly && 
        (getscript || !pngknit || (!isTRUE(getOption('knitr.in.progress')) && !as.if.knit))) {
        return(flowchart)
      }
      
      if (imageFileOnly) {
        if (is.null(folder)) {
          folder <- "."
        }
        options(vtree_folder=folder)
      }
      
      if (is.null(getOption("vtree_count"))) {
        options("vtree_count"=0)
        if (is.null(folder)) {
          if (isTRUE(getOption('knitr.in.progress'))) {
            if (is.null(options()$vtree_folder)) {
              if (knitr::opts_knit$get("out.format") %in% c("latex","sweave","markdown")) {
                knitr.fig.path <- knitr::opts_chunk$get("fig.path")
                options(vtree_folder=knitr.fig.path)
                if (!dir.exists(knitr.fig.path)){
                  tf <- tempfile()
                  cat("```{r}\nplot(0)\n```\n",file=tf)
                  OUTPUT <- utils::capture.output(suppressMessages(knitr::knit_child(tf,
                    options=list(fig.show='hide',warning=FALSE,message=FALSE))))
                }
              } else {
                options(vtree_folder=tempdir())
              }
            }
          }
        } else {
          options(vtree_folder=folder)
        }        
      }      
      
      options("vtree_count"=getOption("vtree_count")+1)
      padCount <- sprintf("%03d",getOption("vtree_count"))
      filenamestem <- paste0("vtree",padCount)          
      
      outfmt <- knitr::opts_knit$get("out.format")
      if (format=="") {
        if (is.null(outfmt)) {
          format <- "png"
        } else {
          if (outfmt %in% c("latex","sweave")) {
            format <- "pdf"
          } else
          if (outfmt %in% c("markdown")) {
            format <- "png"
          }
        }
      }
      
      if (is.null(pxheight)) {
        if (is.null(pxwidth)) {
          fullpath <- grVizToImageFile(flowchart,width=2000,
            format=format,filename=filenamestem,folder=getOption("vtree_folder"))
        } else {
          fullpath <- grVizToImageFile(flowchart,width=pxwidth,
            format=format,filename=filenamestem,folder=getOption("vtree_folder"))
        }
      } else {
        if (is.null(pxwidth)) {
          fullpath <- grVizToImageFile(flowchart,height=pxheight,
            format=format,filename=filenamestem,folder=getOption("vtree_folder"))
        } else {
          fullpath <- grVizToImageFile(flowchart,width=pxwidth,height=pxheight,
            format=format,filename=filenamestem,folder=getOption("vtree_folder"))
        }
      }
      
      if (verbose) message("Image file saved to ",fullpath)
      
      if (imagewidth=="" && imageheight=="") {
        if (imageFileOnly && (!isTRUE(getOption('knitr.in.progress')) && !as.if.knit)) {
          return(invisible(NULL))
        } else {
          output <- knitr::include_graphics(fullpath)
          attributes(output)$info <- treedata
          return(output)
        }
      }

      fmt <- knitr::opts_knit$get("out.format")
      if (!is.null(fmt) && fmt %in% c("latex","sweave")) {
        stuff <- "\n\\includegraphics["
        if (!is.null(trim)) {
          stuff <- paste0(stuff,"trim=",paste(trim,collapse=" "),", clip,")
        }    
        if (imageheight=="") {
          if (imagewidth=="") {
            result <- paste0(stuff," width=5.5in")
          } else {
            result <- paste0(stuff," width=",imagewidth)
          }
        } else {
          if (imagewidth=="") {
            result <- paste0(stuff," height=",imageheight) 
          } else {
            result <- paste0(stuff," width=",
              imagewidth,", height=",imageheight)          
          }
        }
        
        #if (absolutePath) {
        #  np <- normalizePath(fullpath,"/")
        #} else {
        
        np <- fullpath
        
        #}
          
        result <- paste0(result,",keepaspectratio]{",
          np,"}\n")
        
      } else {
    
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
      }
      
      if (imageFileOnly && (!isTRUE(getOption('knitr.in.progress')) && !as.if.knit)) {
        return(invisible(NULL))
      } else {
        output <- knitr::asis_output(result)
        attributes(output)$info <- treedata
        output
      }
    }
  } else {
      tree
  }
  
  # The End ----
}




