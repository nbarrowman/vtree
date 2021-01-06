showflow <- function(flow,getscript=FALSE,font,nodesep=0.5,ranksep=0.5,margin=0.2,
nodelevels="",horiz=FALSE,width=NULL,height=NULL,
graphattr="",nodeattr="",edgeattr="") {
#
# {show} a {flow}chart produced by flowcat or by hier.
#
# showscript Only show the script generated rather than displaying the flowchart? Useful for debugging.
# nodesep    The nodesep graph attribute.
# ranksep    The ranksep graph attribute.
#

  nodePart <- paste0('node [fontname = ',font,', fontcolor = black,shape = rectangle, color = black, tooltip=" "')
  nodePart <- paste0(nodePart,",margin=",margin)
  nodePart <- paste0(nodePart,ifelse(nodeattr=="","",","),nodeattr)
  nodePart <- paste0(nodePart,"]\n")

  graphPart <- paste0('graph [nodesep=',nodesep,
    ', ranksep=',ranksep,', tooltip=" "')
  graphPart <- paste0(graphPart,ifelse(graphattr=="","",","),graphattr)
  graphPart <- paste0(graphPart,']\n')

  script <- paste0(
    'digraph vtree {\n',
    graphPart,
    nodePart)

  if (horiz) {
    script <- paste0(script,'rankdir=LR;\n')
  }

  script <- paste0(script,nodelevels)

  edgePart <- '\nedge[style=solid'
  edgePart <- paste0(edgePart,ifelse(edgeattr=="","",","),edgeattr)
  edgePart <- paste0(edgePart,']\n')

  script <- paste0(script,edgePart,
    flow$edges,"\n\n",flow$labelassign,sep="\n")

  script <- paste0(script,"\n}\n")
  if (getscript) { return(script) }
  flowchart <- DiagrammeR::grViz(script,width=width,height=height)
  
  flowchart
}

