#' @title Export an htmlwidget object into an image file
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
#' @param filenamestem an optional filename stem.
#'                 If not provided, the filename stem will be derived from the name 
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
#'   Returns the full path of the imagefile.
#'
#' @export
#'

grVizToImageFile <- function (g, width=NULL, height=NULL, format="png", folder = ".",filenamestem) {
  if (!("htmlwidget" %in% class(g)))
    stop("Argument must be of class htmlwidget.")
    
  if (is.null(folder)) { stop("folder parameter is NULL") }
  
  if (missing(filenamestem)) {
    filenamestem <- paste0(sapply(as.list(substitute({g})[-1]), deparse))
  }
  
  filename <- 
    ifelse(
      format=="png",
      paste0(filenamestem,".png"),
      ifelse(
        format=="pdf",
        paste0(filenamestem,".pdf"),
        stop("Unsupported format")))
  
  if (is.null(g)) {
    g <- format(DiagrammeR::grViz("digraph empty{ Node1[label='Empty'] }"))
  }
  # Convert any double backslashes to forward slashes.
  folder <- gsub("\\\\","/",folder)
  fullpath <- file.path(folder,filename)
  message <- utils::capture.output(svg <- format(DiagrammeRsvg::export_svg(g)))
  if (format=="png") {
    result <- rsvg::rsvg_png(charToRaw(svg),fullpath, width = width, height=height)
  } else 
  if (format=="pdf") {
    result <- rsvg::rsvg_pdf(charToRaw(svg),fullpath, width = width, height=height)
  }
  
  invisible(fullpath)
}
