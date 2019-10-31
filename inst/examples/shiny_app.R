library(shiny)
# remotes::install_github("nbarrowman/vtree@v3.0.7")
library(vtree)
# library(shinyjs)

## shiny ###########################
use_svgzoom <-  function() {
  includeScript(system.file("/www/svg_pan_zoom.js", package = "vtree"))
  # includeCSS(system.file("/www/svg_pan_zoom.css", package = "vtree"))
}

shiny::addResourcePath("vtree", system.file(package = "vtree"))
ui <- fluidPage(
  use_svgzoom(),
  tags$head(tags$style("body {background-color: blue;")),
  includeCSS(system.file("/www/svg_pan_zoom.css", package = "vtree")),
  includeScript(system.file("svg_init.js", package = "vtree")),
  helpText(div(style="font-weight: 800; font-size: large; color: black;",
               HTML("Doubleclick to start interactive mode.<br>",
               "Zooming and Panning is possible with mouse-drag and mouse-wheel <br>",
               "or with shortcuts; +,- and arrow-keys and CTRL+Backspace to resize+fit+center the svg.."))),
  vtreeOutput("vtree", width = "100%", height = "500px")
)

server <- function(input, output, session) {
  output$vtree <- renderVtree({
    a <- vtree(FakeData,"Severity Sex",
               labelnode=list(Sex=(c("Male"="M","Female"="F"))),
               pngknit=FALSE)
    # browser()
    # a$dependencies <- c(a$dependencies, 
    #                     system.file("/www/svg_pan_zoom.js", package = "vtree"))
    a
  })
  
  observeEvent(input$vtree_click, {
    print("input$vtree_click was clicked")
    # browser()
    cat("\n\n")
    print(rbind(input$vtree_click))
  })
}

shinyApp(ui, server)

