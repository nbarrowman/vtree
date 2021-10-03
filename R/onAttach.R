.onAttach <- function(...) {
  
  if (interactive()) {
    packageStartupMessage("vtree version ",utils::packageVersion("vtree")," -- For more information, type: vignette(\"vtree\")")
  }
}
