joinflow <- function(...) {
#
# {join} information (from the flowcat function) about two or more {flow}charts
#

  edges <- labelassign <- labelshow <- nodenum <- c()
  flows <- list(...)
  for (i in seq_len(length(flows))) {
    if (!is.null(flows[[i]])) {
      nodenum <- c(nodenum,flows[[i]]$nodenum)
      if (length(edges)==0) {
        edges <- flows[[i]]$edges
      } else {
        edges <- paste0(edges,"\n",flows[[i]]$edges)
      }
      if (length(labelassign)==0) {
        labelassign <- flows[[i]]$labelassign
      } else {
        labelassign <- paste0(labelassign,"\n",flows[[i]]$labelassign)
      }
    }
  }
  return(list(nodenum=nodenum,edges=edges,labelassign=labelassign,labelshow=labelshow))
}
