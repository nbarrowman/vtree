#' @title Create a shiny vtree, with svg-pan-zoom functionality.
#' 
#' @param ... parameters to be passed to `vtree`
#'
#' @export
#'

svtree <- function(...) {
  shiny::shinyApp(
    shiny::fluidPage(
      use_svgzoom(),
      tags$head(tags$style(HTML("body {background-color: #9c9ca096;}"))),
      vtreeOutput("vtree", width = "100%", height = "500px")),
    function(input, output, session) {
      output$vtree <- renderVtree(vtree(...,pngknit=FALSE))
    })
}

