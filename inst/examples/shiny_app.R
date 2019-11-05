library(shiny)
library(vtree)

## ui ##################
ui <- fluidPage(
  use_svgzoom(),
  tags$head(tags$style(HTML("body {background-color: #9c9ca096;}"))),
  helpText(div(style="font-weight: 800; font-size: large; color: black;",
               HTML("Zooming and Panning is possible with mouse-drag ",
               "and mouse-wheel <br>or with shortcuts; +,- and arrow-keys ",
               "and CTRL+Backspace to resize+fit+center the svg.."))),
  vtreeOutput("vtree", width = "100%", height = "500px")
)

## server ##################
server <- function(input, output, session) {
  output$vtree <- renderVtree({
    vtree(FakeData,"Severity Sex", 
          horiz = F,
          labelnode=list(Sex=(c("Male"="M","Female"="F"))),
          pngknit=FALSE)
  })
}

shinyApp(ui, server)

