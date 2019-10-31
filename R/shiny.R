#' Vtree widget
#'
#' Shiny bindings for vtree. It is actually a wrapper around \code{\link[DiagrammeR]{grViz}}.
#' @param outputId output variable to read from
#' @param width,height must be a valid CSS unit in pixels
#'  or a number, which will be coerced to a string and have \code{"px"} appended.
#' @seealso \code{\link{renderVtree}}
#' @examples \dontrun{
#' library(shiny)
#' library(vtree)
#' 
#' ui <- fluidPage(
#'   vtreeOutput("vtree", width = "100%", height = "800px")
#' )
#' 
#' server <- function(input, output, session) {
#'   output$vtree <- renderVtree({
#'     vtree(FakeData,"Severity Sex",
#'           labelnode=list(Sex=(c("Male"="M","Female"="F"))),
#'           pngknit=FALSE)
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#' @export
vtreeOutput <- function(outputId, width = "100%", height = "100%"){
  htmlwidgets::shinyWidgetOutput(outputId, 'grViz', width, height,
                                 package = 'DiagrammeR')
}

#' Vtree widget
#'
#' Shiny bindings for vtree
#'
#' @param expr an expression that generates an vtree
#' @param env the environment in which to evaluate \code{expr}.
#' @param quoted is \code{expr} a quoted expression (with \code{quote()})? This
#'  is useful if you want to save an expression in a variable.
#' @seealso \code{\link{vtreeOutput}}, \code{\link{vtree}}
#' @importFrom htmlwidgets shinyRenderWidget shinyWidgetOutput
#' @examples \dontrun{
#' library(shiny)
#' library(vtree)
#' 
#' ui <- fluidPage(
#'   vtreeOutput("vtree", width = "100%", height = "800px")
#' )
#' 
#' server <- function(input, output, session) {
#'   output$vtree <- renderVtree({
#'     vtree(FakeData,"Severity Sex",
#'           labelnode=list(Sex=(c("Male"="M","Female"="F"))),
#'           pngknit=FALSE)
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' }
#' @export
renderVtree <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, vtreeOutput, env, quoted = TRUE)
}



# add_vtree_zoom <- function() {
#   
# }