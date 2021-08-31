labelsAndLegends <- function(z,OLDVARS,vars,labelvar,HTMLtext,vsplitwidth,just,colorvarlabels,
  varnamebold,varlabelcolors,varnamepointsize,showroot,rounded,numvars,Venn,labelnode,
  fillcolor,showlegendsum,thousands,splitwidth,nodefunc,nodeargs,showvarnames,check.is.na,
  vp,showlpct,digits,legendpointsize,horiz,color,showlegend,pattern,sepN) {
  
  if (showvarnames) {
    # Special case for check.is.na
    if (check.is.na) {
      VARS <- OLDVARS
    } else {
      VARS <- vars
    }
    if (!is.null(labelvar)) {
      for (i in 1:numvars) {
        if (!is.na(labelvar[vars[i]])) {
          VARS[i] <- labelvar[vars[i]]
        }
      }
    }
    
    if (!HTMLtext) {
      VARS <- splitlines(VARS,width=vsplitwidth,sp='\n',at = c(" ", ".", "-", "+", "_", "=", "/"))
      VARS <- convertToHTML(VARS,just=just)
    }
    
    if (colorvarlabels) {
      if (varnamebold) {
        colored_VARS <- paste0('<FONT COLOR="',varlabelcolors,'">',"<B>",VARS,'  </B>','</FONT>')
      } else {
        colored_VARS <- paste0('<FONT COLOR="',varlabelcolors,'">',VARS,'</FONT>')
      }
    } else {
      colored_VARS <- VARS
    }
    colored_VARS <- paste0('<FONT POINT-SIZE="',varnamepointsize,'">',colored_VARS,'</FONT>')
    marginalText <- rep("",numvars)
    
    # ***********************************************************************
    # Begin: Legend stuff  ----
    # ***********************************************************************
    
    if (showroot) {
      NL <- "Node_L0_0 [style=invisible]\n"
    } else {
      NL <- ""
    }
    
    if (rounded) {
      styleString <- ' style="rounded,filled"'
    } else {
      styleString <- ' style=filled'
    }
    
    for (i in seq_len(numvars)) {
      thisvarname <- vars[i]
      thisvar <- z[[thisvarname]]
      if (is.logical(thisvar)) {
        thisvar <- factor(thisvar, c("FALSE", "TRUE"))
      }
      categoryCounts <- table(thisvar,exclude=NULL)
      
      if (Venn) {
        names(categoryCounts)[which(names(categoryCounts)=="1" | names(categoryCounts)=="TRUE")] <- "Yes"
        names(categoryCounts)[which(names(categoryCounts)=="0" | names(categoryCounts)=="FALSE")] <- "No"
      }
      
      names(categoryCounts)[is.na(names(categoryCounts))] <- "NA"
      
      if (vp & any(is.na(thisvar))) {
        cc <- categoryCounts
        cc <- cc[names(cc)!="NA"]
        if (length(cc)>0) {
          if (showlpct) {
            npctString <- paste0(
              lapply(cc,function(x) format(x,big.mark=thousands)),
              " (",
              around(100*cc/sum(cc),digits),"%)")
          } else {
            npctString <- 
              lapply(cc,function(x) format(x,big.mark=thousands))
          }
        } else {
          npctString <- NULL
        }
        npctString <- c(npctString,categoryCounts["NA"])
      } else {
        if (showlpct) {
          #browser()
          npctString <- paste0(
            lapply(categoryCounts,function(x) format(x,big.mark=thousands)),
            " (",
            around(100*categoryCounts/length(thisvar),digits),"%)")
        } else {
          npctString <- 
            lapply(categoryCounts,function(x) format(x,big.mark=thousands))
        }
      }
      
      CAT <- names(categoryCounts)
      
      # Relabel the nodes if labels have been specified
      labels <- labelnode[[thisvarname]]
      for (label in labels) {
        if (label %in% CAT) {
          m <- match(label,CAT)
          CAT[m] <- names(labels)[labels==label]
        }
      }
      
      
      labels <- paste0(
        'label=<',
        colored_VARS[i],
        '>')        
      
      nlheading <- paste0("Node_L",i,"_0",
        '[',
        labels,
        ' shape=none margin=0]',collapse = '\n')
      
      FILLCOLOR <- fillcolor[[thisvarname]][seq_len(length(categoryCounts))]
      
      if (HTMLtext) {
        displayCAT <- splitlines(CAT,width=splitwidth,sp="<BR/>",at=" ")
      } else {
        displayCAT <- splitlines(CAT,width=splitwidth,sp="\n",at = c(" ", ".", "-", "+", "_", "=", "/"))
      }      
      
      if (HTMLtext) {
        displayCAT <- displayCAT
      } else {
        displayCAT <- convertToHTML(displayCAT,just=just)
      }
      
      legendlabel <- paste0(displayCAT,", ",npctString)
      
      ThisLayerText <- rep("",length(legendlabel)) 
      
      if (showlegendsum) {
        if (!is.null(nodefunc)) {
          if (numvars == 1)
            nodeargs$leaf <- TRUE
          ThisLayerText <- c()
          current_var <- as.character(thisvar)
          current_var[is.na(current_var)] <- "NA"
          summarytext <- vector("list",length=length(CAT))
          names(summarytext) <- CAT
          for (value in displayCAT) {
            df_subset <- z[current_var == value,,drop=FALSE]
            summarytext[[value]] <- nodefunc(df_subset, vars[i], value, args = nodeargs)
            nodetext <- paste0(summarytext[[value]],collapse="")
            nodetext <- splitlines(nodetext, width = splitwidth, sp = sepN, at=" ")
            ThisLayerText <- c(ThisLayerText, paste0(nodetext,sepN))
          }
        }
      }
      
      extendedlegendlabel <- paste0(legendlabel,convertToHTML(ThisLayerText,just=just))
      
      
      labels <- paste0(
        'label=<<FONT POINT-SIZE="',legendpointsize,'">',
        extendedlegendlabel,
        '</FONT>>')        
      
      if (!horiz) {
        labels <- rev(labels)
        FILLCOLOR <- rev(FILLCOLOR)
      }
      
      rgb <- grDevices::col2rgb(FILLCOLOR)
      red <- rgb["red",]; green <- rgb["green",]; blue <- rgb["blue",]
      FONTCOLOR <- ifelse((red*0.299 + green*0.587 + blue*0.114) > 186,"#000000","#ffffff")    
      nl <- paste0("Node_L",i,"_",seq_len(length(categoryCounts)),
        '[',
        labels,
        ' fontcolor=<',FONTCOLOR,'>', 
        ' color=',color[i+1],' ',
        styleString,
        ' fillcolor=<',FILLCOLOR,'> height=0]',
        collapse = '\n')
      
      nl_allnodes <- paste0("Node_L",i,"_",seq(0,length(categoryCounts)),collapse=" ")
      
      
      if (showlegend) {
        nl <- paste0(
          "subgraph cluster_",i," {\n",
          "style=rounded\n",
          "color=<#bdbdbd>\n",
          "{rank=same"," ",nl_allnodes,"}\n",
          nlheading,
          "\n",
          nl,
          "\n}\n",
          paste0("Node_L",i-1,"_0 -> Node_L",i,"_0 [style=invisible arrowhead=none]\n"))
        if ((pattern | check.is.na) && i==1) {
          nl <- "Node_L1_0[style=invisible arrowhead=none]\n"
        }
      } else {
        link <- paste0("Node_L",i-1,"_0 -> Node_L",i,"_0 [style=invisible arrowhead=none]\n")
        if (i==1 & !showroot) link <- ""
        nl <- paste0(
          nlheading,
          "\n",
          link)
      }
      NL <- paste0(NL,"\n",nl)
      
      #-^------------------------------^--------------------------------------^-
      # End: Legend stuff  ----
      #-------------------------------------------------------------------------
      
    }
    
  }
  else {
    NL <- ''
  }
  
  NL
}