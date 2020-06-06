#' Setup for inline CSS
#'
#' Initial CSS setup for the SVG.
#'  
#' @param minheight minimum height in \code{"px"}. Default is "200px".
#' @param cursor_all The cursor symbol for the whole SVG. Default is "all-scroll".
#' @param overflow Overflow value for the whole SVG. Default is "inherit".
#' @param position CSS position of the SVG. Default is "sticky".
#' @param fill Fill color for the SVG background. Default is "transparent".
#' @param cursor_text The cursor symbol for text nodes. Default is "pointer".
#'
#' @importFrom shiny HTML
#' @seealso \code{\link{use_svgzoom}}
#' @keywords internal
#' @family Shiny Functions
inlineCssSetup <- function(minheight, cursor_all, overflow, position, 
                           fill, cursor_text) {
  
  svg <- sprintf(".grViz svg {
                    min-height: %s;
                    cursor: %s;
                    overflow: %s;
                    position: %s;
                  }",
                 minheight,
                 cursor_all, 
                 overflow,
                 position)
  
  poly <- sprintf(".grViz #graph0 > polygon {
                    fill: %s;
                  }",
                  fill)
  
  text <- sprintf(".grViz text {
                    cursor: %s;
                  }",
                  cursor_text)
  
  shiny::HTML(paste(svg, poly, text))
}

#' Setup for interactive Vtree
#'
#' This function must be called in the UI, in order to make the \code{\link{vtree}} interactive.
#'  
#' @inheritParams inlineCssSetup
#' @inheritParams init_js
#'
#' @importFrom shiny addResourcePath singleton tags
#' @seealso \code{\link{vtreeOutput}}, \code{\link{vtree}}
#' @family Shiny Functions
#' @examples \dontrun{
#' library(shiny)
#' library(vtree)
#' 
#' ui <- fluidPage(
#'   use_svgzoom(),
#'   helpText(div(style="font-weight: 800; font-size: large; color: black;",
#'                HTML("Zooming and Panning is possible with mouse-drag ",
#'                     "and mouse-wheel <br>, or with shortcuts;",
#'                     " +,- and arrow-keys and CTRL+Backspace to",
#'                     " resize+fit+center the svg.."))),
#'   vtreeOutput("vtree", width = "100%", height = "500px")
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
use_svgzoom <-  function(minheight = "200px", 
                         cursor_all = "all-scroll", 
                         overflow = "inherit !important", 
                         position = "sticky",
                         fill = "transparent",
                         cursor_text = "pointer",
                         init_event = c("mouseenter","click","dblclick"), 
                         onwindow_resize = TRUE,
                         shortcuts = TRUE
                         ) {
  init_event <- match.arg(init_event)
  addResourcePath("vtree", system.file(package = "vtree"))
  singleton(
    tags$head(
      tags$script(src = "vtree/www/svg_pan_zoom.js"),
      tags$style(inlineCssSetup(minheight, cursor_all, overflow, position, 
                                fill, cursor_text)),
      tags$script(init_js(init_event, onwindow_resize, shortcuts))
    )
  )
}

#' Initializing JS-Part
#' @param init_event The mouse event to activate zooming and panning.
#' Default is \code{mouseenter}.
#' @param onwindow_resize Should the SVG be resized when the window size changes?
#' Default is \code{TRUE}.
#' @param shortcuts Should Keyboard shortcuts be used to control the SVG?
#' Default is \code{TRUE}.
#' @family Shiny Functions
#' @keywords internal
init_js <- function(init_event, onwindow_resize, shortcuts) {

  ## Text snippets ######################## 
  wrap_conn <- '$(document).on("shiny:connected", function(event) {
    %s
    %s
  });'

  initjs <- paste0('$(document).on("%s", ".grViz svg", function(){
      if ($(".grViz svg").length > 0) {
        svgPanZoom(".grViz svg");
      }
    });')
  initjs <- sprintf(initjs, init_event)

  resize <- '$(window).on("resize", function(){
      if ($(".grViz svg").length > 0) {
        var inst = svgPanZoom(".grViz svg");
        inst.resize(); // update SVG cached size and controls positions
        inst.fit();
        inst.center();
      }
    });'

  ## Output ##################
  if (onwindow_resize) {
    initjs <- HTML(sprintf(wrap_conn, initjs, resize))
  } else {
    initjs <- HTML(sprintf(wrap_conn, initjs, ""))
  }
  
  if (shortcuts) {
    ## text snippet ###########################
    shcts <- "// Shortcuts Functions
$(document).on('keydown', function(e) {
  //e.preventDefault();
  //e.stopPropagation();
  if ($('.grViz') && $('.grViz').is(':hover')) {
    var ekey = e.keyCode;
    var panZoomTiger = svgPanZoom('.grViz svg');
    switch(ekey){
    // Zoom IN (+)
    case 171: {
      panZoomTiger.zoomBy(1.1);
      break;
    }
    // Zoom Out (-)
    case 173: {
      panZoomTiger.zoomBy(0.9);
      break;
    }
    // Center and Fit (CTRL + Backspace)
    case 8: {
      if (e.ctrlKey && e.ctrlKey === true) {
        panZoomTiger.resize(); // update SVG cached size and controls positions
        panZoomTiger.fit();
        panZoomTiger.center();
      }
      break;
    }
    // Pan Left (left arrow)
    case 37: {
      panZoomTiger.panBy({x: -50, y: 0});
      break;
    }
    // Pan Right (right arrow)
    case 39: {
      panZoomTiger.panBy({x: 50, y: 0});
      break;
    }
    // Pan Up (up arrow)
    case 38: {
      panZoomTiger.panBy({x: 0, y: -50});
      break;
    }
    // Pan Down (down arrow)
    case 40: {
      panZoomTiger.panBy({x: 0, y: 50});
      break;
    }
  }
  }
});"
    #######################
    HTML(paste(initjs, shcts))
  } else {
    initjs
  }
}

#' vtree widget
#'
#' Shiny bindings for vtree. It is actually a wrapper around \code{\link[DiagrammeR]{grViz}}.
#' @param outputId output variable to read from
#' @param width,height must be a valid CSS unit in pixels
#'  or a number, which will be coerced to a string and have \code{"px"} appended.
#' @seealso \code{\link{renderVtree}}
#' @family Shiny Functions
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

#' vtree widget
#'
#' Shiny bindings for vtree
#'
#' @param expr an expression that generates a variable tree
#' @param env the environment in which to evaluate \code{expr}.
#' @param quoted is \code{expr} a quoted expression (with \code{quote()})? This
#'  is useful if you want to save an expression in a variable.
#' @seealso \code{\link{vtreeOutput}}, \code{\link{vtree}}
#' @importFrom htmlwidgets shinyRenderWidget shinyWidgetOutput
#' @family Shiny Functions
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


