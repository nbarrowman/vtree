#' @title Export an htmlwidget object into a PNG file
#'
#' @author Nick Barrowman
#'
#' @description
#' Export an \code{htmlwidget} object (produced by \code{DiagrammerR::grViz}) into a PNG file
#'
#' @param g        an object produced by the \code{grViz} function from the DiagrammmeR package
#' @param width    the width in pixels of the bitmap
#' @param height   the height in pixels of the bitmap
#' @param format   Graphics file format. Currently "png" and "pdf" are supported.
#' @param folder   path to folder where the PNG file should stored
#' @param filename an optional filename.
#'                 If not provided, the filename will be derived from the name 
#'                 of the argument of \code{g}.
#'
#' @details
#'   First the \code{grViz} object is exported to an SVG file (using \code{DiagrammeRsvg::export_svg}).
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

grVizToPNG <- function (g, width=NULL, height=NULL, folder = ".",filename) {

  if (missing(filename)) {
    filename <- paste0(sapply(as.list(substitute({g})[-1]), deparse),".png")
  }
  
  grVizToImage(g=g,width=width,height=heignt,format="png",folder=folder,filename=filename)
}
