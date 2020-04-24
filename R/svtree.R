#' @title Create a Shiny vtree, with svg-pan-zoom functionality.
#' 
#' @param ... parameters to be passed to `vtree`
#'
#' @description
#' `svtree` uses Shiny and the svg-pan-zoom JavaScript library to 
#' create a variable tree with panning and zooming functionality. 
#' The mousewheel allows you to zoom in or out.
#' The variable tree can also be dragged to a different position.
#'
#' @details The svg-pan-zoom library webpage is
#' https://github.com/ariutta/svg-pan-zoom  
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

