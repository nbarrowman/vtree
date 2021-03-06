% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny.R
\name{renderVtree}
\alias{renderVtree}
\title{vtree widget}
\usage{
renderVtree(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{expr}{an expression that generates a variable tree}

\item{env}{the environment in which to evaluate \code{expr}.}

\item{quoted}{is \code{expr} a quoted expression (with \code{quote()})? This
is useful if you want to save an expression in a variable.}
}
\description{
Shiny bindings for vtree
}
\examples{
\dontrun{
library(shiny)
library(vtree)

ui <- fluidPage(
  vtreeOutput("vtree", width = "100\%", height = "800px")
)

server <- function(input, output, session) {
  output$vtree <- renderVtree({
    vtree(FakeData,"Severity Sex",
          labelnode=list(Sex=(c("Male"="M","Female"="F"))),
          pngknit=FALSE)
  })
}

shinyApp(ui, server)
}
}
\seealso{
\code{\link{vtreeOutput}}, \code{\link{vtree}}

Other Shiny Functions: 
\code{\link{init_js}()},
\code{\link{inlineCssSetup}()},
\code{\link{use_svgzoom}()},
\code{\link{vtreeOutput}()}
}
\concept{Shiny Functions}
