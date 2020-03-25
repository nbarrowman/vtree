test_that("text",{

  expect_identical(vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,text=list(Severity=c(Mild="\n*Excluding\nnew diagnoses*"))),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3\nNode_2->Node_4 Node_2->Node_5 Node_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9 Node_3->Node_10 Node_3->Node_11\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<A<BR/>24 (52%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<B<BR/>22 (48%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_4[label=<Mild<BR/>10 (45%)<BR/><I>Excluding<BR/>new diagnoses</I>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_5[label=<Moderate<BR/>8 (36%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_6[label=<Severe<BR/>4 (18%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_7[label=<NA<BR/>2> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_8[label=<Mild<BR/>9 (50%)<BR/><I>Excluding<BR/>new diagnoses</I>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<Moderate<BR/>8 (44%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_10[label=<Severe<BR/>1 (6%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_11[label=<NA<BR/>4> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,"Group Severity",horiz=FALSE,showvarnames=FALSE,
    ttext=list(
    c(Group="B",Severity="Mild",text="\n*Excluding\nnew diagnoses*"),
    c(Group="A",text="\nSweden"),
    c(Group="B",text="\nNorway"))),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3\nNode_2->Node_4 Node_2->Node_5 Node_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9 Node_3->Node_10 Node_3->Node_11\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<A<BR/>24 (52%)<BR/>Sweden> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<B<BR/>22 (48%)<BR/>Norway> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_4[label=<Mild<BR/>10 (45%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_5[label=<Moderate<BR/>8 (36%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_6[label=<Severe<BR/>4 (18%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_7[label=<NA<BR/>2> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_8[label=<Mild<BR/>9 (50%)<BR/><I>Excluding<BR/>new diagnoses</I>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<Moderate<BR/>8 (44%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_10[label=<Severe<BR/>1 (6%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_11[label=<NA<BR/>4> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))    
    
})


test_that("cdigits",{
  expect_identical(vtree(FakeData,"Severity",summary="Score \nmean score: %mean%",cdigits=0,sameline=TRUE,horiz=FALSE),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\n\nNode_1[label=<46<BR/>mean score: 17 mv=2<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild, 19 (48%)<BR/>mean score: 12 mv=1<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46<BR/>mean score: 17 mv=2<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate, 16 (40%)<BR/>mean score: 17 mv=1<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46<BR/>mean score: 17 mv=2<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe, 5 (12%)<BR/>mean score: 38<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46<BR/>mean score: 17 mv=2<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA, 6<BR/>mean score: 15<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
})

    
test_that("variable specifications",{

  expect_identical(vtree(FakeData,"Ind*"),
    structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Ind1  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Ind2  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nNode_L3_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#31A354\"><B>Ind3  </B></FONT></FONT>> shape=none margin=0]\nNode_L2_0 -> Node_L3_0 [style=invisible arrowhead=none]\n\nNode_L4_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#E6550D\"><B>Ind4  </B></FONT></FONT>> shape=none margin=0]\nNode_L3_0 -> Node_L4_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4\nNode_2->Node_5 Node_2->Node_6\nNode_5->Node_7 Node_5->Node_8\nNode_7->Node_9 Node_7->Node_10\nNode_8->Node_11\nNode_6->Node_12 Node_6->Node_13 Node_6->Node_14\nNode_12->Node_15 Node_12->Node_16\nNode_13->Node_17\nNode_14->Node_18\nNode_3->Node_19 Node_3->Node_20\nNode_19->Node_21 Node_19->Node_22 Node_19->Node_23\nNode_21->Node_24 Node_21->Node_25\nNode_22->Node_26 Node_22->Node_27\nNode_23->Node_28\nNode_20->Node_29 Node_20->Node_30 Node_20->Node_31\nNode_29->Node_32 Node_29->Node_33\nNode_30->Node_34\nNode_31->Node_35\nNode_4->Node_36\nNode_36->Node_37\nNode_37->Node_38\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<0<BR/>23 (51%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<1<BR/>22 (49%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<NA<BR/>1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_5[label=<0<BR/>12 (52%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_6[label=<1<BR/>11 (48%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_7[label=<0<BR/>6 (50%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_8[label=<1<BR/>6 (50%)> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_9[label=<0<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_10[label=<1<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_11[label=<1<BR/>6 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_12[label=<0<BR/>6 (60%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_13[label=<1<BR/>4 (40%)> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_14[label=<NA<BR/>1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_15[label=<0<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_16[label=<1<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_17[label=<1<BR/>4 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_18[label=<1<BR/>1 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_19[label=<0<BR/>10 (45%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_20[label=<1<BR/>12 (55%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_21[label=<0<BR/>4 (44%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_22[label=<1<BR/>5 (56%)> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_23[label=<NA<BR/>1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_24[label=<0<BR/>2 (50%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_25[label=<1<BR/>2 (50%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_26[label=<0<BR/>2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_27[label=<1<BR/>3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_28[label=<0<BR/>1 (100%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_29[label=<0<BR/>6 (55%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_30[label=<1<BR/>5 (45%)> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_31[label=<NA<BR/>1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_32[label=<0<BR/>4 (67%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_33[label=<1<BR/>2 (33%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_34[label=<1<BR/>5 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_35[label=<1<BR/>1 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_36[label=<0<BR/>1 (100%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_37[label=<0<BR/>1 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_38[label=<1<BR/>1 (100%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,"Age<5"),
    structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Age  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<&ge;5<BR/>12 (31%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<&lt;5<BR/>27 (69%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<NA<BR/>7> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))

  expect_identical(vtree(FakeData,"is.na:Severity"),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<not N/A<BR/>40 (87%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<N/A<BR/>6 (13%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR")) 
    
})


test_that("knitr",{
  # Note that knitting to PNG a temporary folder is used by default,
  # so I'm using a regular expression to partially match it.
  expect_match(vtree(FakeData,"Severity Sex",as.if.knit=TRUE),
    "!\\[\\].+\\.png\\).*")
  
  expect_identical(vtree(FakeData,"Severity Sex",as.if.knit=TRUE,pngknit=FALSE),
    structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9\nNode_4->Node_10 Node_4->Node_11\nNode_5->Node_12 Node_5->Node_13\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild<BR/>19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate<BR/>16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_6[label=<F<BR/>11 (58%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<M<BR/>8 (42%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_8[label=<F<BR/>11 (69%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<M<BR/>5 (31%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_10[label=<F<BR/>2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_11[label=<M<BR/>3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_12[label=<F<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_13[label=<M<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))    
})
    

test_that("pruning",{
  
  expect_identical(vtree(FakeData,"Severity Sex",prune=list(Severity=c("Mild","Moderate"))),
    structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3\nNode_2->Node_4 Node_2->Node_5\nNode_3->Node_6 Node_3->Node_7\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Severe<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_4[label=<F<BR/>2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_5[label=<M<BR/>3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_6[label=<F<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<M<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,"Severity Sex",prunebelow=list(Severity=c("Mild","Moderate"))),
    structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_4->Node_6 Node_4->Node_7\nNode_5->Node_8 Node_5->Node_9\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild<BR/>19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate<BR/>16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_6[label=<F<BR/>2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<M<BR/>3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_8[label=<F<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<M<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,"Severity Sex Age Category",sameline=TRUE,prunesmaller=3),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nNode_L3_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#31A354\"><B>Age  </B></FONT></FONT>> shape=none margin=0]\nNode_L2_0 -> Node_L3_0 [style=invisible arrowhead=none]\n\nNode_L4_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#E6550D\"><B>Category  </B></FONT></FONT>> shape=none margin=0]\nNode_L3_0 -> Node_L4_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_2->Node_6 Node_2->Node_7\nNode_6->Node_8 Node_6->Node_9 Node_6->Node_10\nNode_9->Node_11\nNode_7->Node_12 Node_7->Node_13\nNode_12->Node_14\nNode_3->Node_15 Node_3->Node_16\nNode_15->Node_17 Node_15->Node_18 Node_15->Node_19 Node_15->Node_20\nNode_17->Node_21\nNode_4->Node_22\nNode_22->Node_23\nNode_5->Node_24 Node_5->Node_25\nNode_24->Node_26\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild, 19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate, 16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe, 5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA, 6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_6[label=<F, 11 (58%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<M, 8 (42%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_8[label=<3, 3 (33%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_9[label=<4, 4 (44%)> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_10[label=<NA, 2> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_11[label=<triple, 3 (75%)> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_12[label=<4, 3 (43%)> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_13[label=<NA, 1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_14[label=<single, 3 (100%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_15[label=<F, 11 (69%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_16[label=<M, 5 (31%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_17[label=<3, 3 (30%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_18[label=<4, 4 (40%)> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_19[label=<5, 3 (30%)> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_20[label=<NA, 1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_21[label=<single, 3 (100%)> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_22[label=<M, 3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_23[label=<NA, 1> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_24[label=<F, 3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_25[label=<M, 3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_26[label=<NA, 2> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))    
  
  expect_identical(vtree(FakeData[is.na(FakeData$Severity),],"Group Severity Sex",keep=list(Group="B")),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Group  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nNode_L3_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#31A354\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L2_0 -> Node_L3_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2\nNode_2->Node_3\nNode_3->Node_4 Node_3->Node_5\n\nNode_1[label=<6> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<B<BR/>4 (67%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_3[label=<NA<BR/>4> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_4[label=<F<BR/>2 (50%)> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_5[label=<M<BR/>2 (50%)> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))    
})


test_that("getscript",{
  
  expect_identical(vtree(FakeData,"Severity Sex",getscript=TRUE),
"digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9\nNode_4->Node_10 Node_4->Node_11\nNode_5->Node_12 Node_5->Node_13\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild<BR/>19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate<BR/>16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_6[label=<F<BR/>11 (58%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<M<BR/>8 (42%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_8[label=<F<BR/>11 (69%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<M<BR/>5 (31%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_10[label=<F<BR/>2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_11[label=<M<BR/>3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_12[label=<F<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_13[label=<M<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n"  )
  
  expect_identical(vtree(FakeData,"Severity Sex",horiz=FALSE,plain=TRUE,getscript=TRUE),
"digraph vtree {\ngraph [nodesep=0.46, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.18]\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\">Severity</FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\">Sex</FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9\nNode_4->Node_10 Node_4->Node_11\nNode_5->Node_12 Node_5->Node_13\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild<BR/>19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#C6DBEF>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate<BR/>16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#C6DBEF>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#C6DBEF>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<#C6DBEF>  ]\nNode_6[label=<F<BR/>11 (58%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_7[label=<M<BR/>8 (42%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_8[label=<F<BR/>11 (69%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_9[label=<M<BR/>5 (31%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_10[label=<F<BR/>2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_11[label=<M<BR/>3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_12[label=<F<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_13[label=<M<BR/>3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\n\n}\n"  )
  
  expect_identical(vtree(FakeData,"Severity Sex",showlegend=TRUE,shownodelabels=FALSE,getscript=TRUE),
"digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nsubgraph cluster_1 {\nstyle=rounded\ncolor=<#bdbdbd>\n{rank=same Node_L1_0 Node_L1_1 Node_L1_2 Node_L1_3 Node_L1_4}\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_1[label=<<FONT POINT-SIZE=\"14\">Mild, 19 (48%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#FEE0D2> height=0]\nNode_L1_2[label=<<FONT POINT-SIZE=\"14\">Moderate, 16 (40%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#FC9272> height=0]\nNode_L1_3[label=<<FONT POINT-SIZE=\"14\">Severe, 5 (12%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#DE2D26> height=0]\nNode_L1_4[label=<<FONT POINT-SIZE=\"14\">NA, 6</FONT>> color=black  style=\"rounded,filled\" fillcolor=<white> height=0]\n}\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nsubgraph cluster_2 {\nstyle=rounded\ncolor=<#bdbdbd>\n{rank=same Node_L2_0 Node_L2_1 Node_L2_2}\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L2_1[label=<<FONT POINT-SIZE=\"14\">F, 27 (59%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#DEEBF7> height=0]\nNode_L2_2[label=<<FONT POINT-SIZE=\"14\">M, 19 (41%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#3182BD> height=0]\n}\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9\nNode_4->Node_10 Node_4->Node_11\nNode_5->Node_12 Node_5->Node_13\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_6[label=<11 (58%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<8 (42%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_8[label=<11 (69%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<5 (31%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_10[label=<2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_11[label=<3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_12[label=<3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_13[label=<3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n")  
})


  
test_that("vtree test ptable",{
  expect_identical(vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,ptable=TRUE),
structure(list(n = c(1, 1, 1, 1, 2, 4, 4, 32), pct = c(2, 2, 
2, 2, 4, 9, 9, 70), MISSING_Severity = c("not N/A", "not N/A", 
"not N/A", "not N/A", "N/A", "N/A", "not N/A", "not N/A"), MISSING_Age = c("N/A", 
"not N/A", "not N/A", "not N/A", "N/A", "not N/A", "N/A", "not N/A"
), MISSING_Pre = c("N/A", "N/A", "N/A", "not N/A", "not N/A", 
"not N/A", "not N/A", "not N/A"), MISSING_Post = c("not N/A", 
"N/A", "not N/A", "N/A", "not N/A", "not N/A", "not N/A", "not N/A"
)), row.names = c(NA, -8L), class = "data.frame")
  )})

test_that("vtree test ptable VennTable",{
  expect_identical(VennTable(vtree(FakeData,"Severity Age Pre Post",check.is.na=TRUE,ptable=TRUE),sort=FALSE),
structure(c("1", "1", "1", "1", "2", "4", "4", "32", "46", "", 
"", "2", "2", "2", "2", "4", "9", "9", "70", "100", "", "", "0", 
"0", "0", "0", "2", "4", "0", "0", "", "6", "13", "1", "0", "0", 
"0", "2", "0", "4", "0", "", "7", "15", "1", "1", "1", "0", "0", 
"0", "0", "0", "", "3", "7", "0", "1", "0", "1", "0", "0", "0", 
"0", "", "2", "4"), .Dim = c(11L, 6L), .Dimnames = list(c("", 
"", "", "", "", "", "", "", "Total", "N", "pct"), c("n", "pct", 
"MISSING_Severity", "MISSING_Age", "MISSING_Pre", "MISSING_Post"
)))
  )})


test_that("vtree pattern tree test",{
  expect_identical(vtree(FakeData,"Severity Age Group",pattern=TRUE,getscript=TRUE),
"digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#FC9272\"><B>pattern  </B></FONT></FONT>> shape=none margin=0]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nNode_L3_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#31A354\"><B>Age  </B></FONT></FONT>> shape=none margin=0]\nNode_L2_0 -> Node_L3_0 [style=invisible arrowhead=none]\n\nNode_L4_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#E6550D\"><B>Group  </B></FONT></FONT>> shape=none margin=0]\nNode_L3_0 -> Node_L4_0 [style=invisible arrowhead=none]\n\nedge[style=solid, arrowhead=none]\n\nNode_2->Node_24\nNode_24->Node_25\nNode_25->Node_26\nNode_3->Node_27\nNode_27->Node_28\nNode_28->Node_29\nNode_4->Node_30\nNode_30->Node_31\nNode_31->Node_32\nNode_5->Node_33\nNode_33->Node_34\nNode_34->Node_35\nNode_6->Node_36\nNode_36->Node_37\nNode_37->Node_38\nNode_7->Node_39\nNode_39->Node_40\nNode_40->Node_41\nNode_8->Node_42\nNode_42->Node_43\nNode_43->Node_44\nNode_9->Node_45\nNode_45->Node_46\nNode_46->Node_47\nNode_10->Node_48\nNode_48->Node_49\nNode_49->Node_50\nNode_11->Node_51\nNode_51->Node_52\nNode_52->Node_53\nNode_12->Node_54\nNode_54->Node_55\nNode_55->Node_56\nNode_13->Node_57\nNode_57->Node_58\nNode_58->Node_59\nNode_14->Node_60\nNode_60->Node_61\nNode_61->Node_62\nNode_15->Node_63\nNode_63->Node_64\nNode_64->Node_65\nNode_16->Node_66\nNode_66->Node_67\nNode_67->Node_68\nNode_17->Node_69\nNode_69->Node_70\nNode_70->Node_71\nNode_18->Node_72\nNode_72->Node_73\nNode_73->Node_74\nNode_19->Node_75\nNode_75->Node_76\nNode_76->Node_77\nNode_20->Node_78\nNode_78->Node_79\nNode_79->Node_80\nNode_21->Node_81\nNode_81->Node_82\nNode_82->Node_83\nNode_22->Node_84\nNode_84->Node_85\nNode_85->Node_86\nNode_23->Node_87\nNode_87->Node_88\nNode_88->Node_89\n\n\nNode_2[label=<5 (11%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_3[label=<4 (9%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_4[label=<4 (9%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_5[label=<3 (7%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_6[label=<3 (7%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_7[label=<3 (7%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_8[label=<3 (7%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_9[label=<2 (4%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_10[label=<2 (4%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_11[label=<2 (4%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_12[label=<2 (4%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_13[label=<2 (4%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_14[label=<2 (4%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_15[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_16[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_17[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_18[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_19[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_20[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_21[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_22[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\n\nNode_23[label=<1 (2%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_24[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_25[label=<4<BR/>> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_26[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_27[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_28[label=<3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_29[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_30[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_31[label=<3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_32[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_33[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_34[label=<5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_35[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_36[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_37[label=<4<BR/>> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_38[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_39[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_40[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_41[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_42[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_43[label=<5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_44[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_45[label=<Severe<BR/>> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_46[label=<5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_47[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_48[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_49[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_50[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_51[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_52[label=<3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_53[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_54[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_55[label=<5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_56[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_57[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_58[label=<4<BR/>> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_59[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_60[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_61[label=<4<BR/>> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_62[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_63[label=<Severe<BR/>> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_64[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_65[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_66[label=<Severe<BR/>> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_67[label=<4<BR/>> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_68[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\nNode_69[label=<Severe<BR/>> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_70[label=<3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_71[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_72[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_73[label=<5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_74[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_75[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_76[label=<4<BR/>> color=black style=\"rounded,filled\" fillcolor=<#A1D99B>  ]\nNode_77[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_78[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_79[label=<NA<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_80[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_81[label=<Moderate<BR/>> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_82[label=<3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_83[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_84[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_85[label=<5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#31A354>  ]\nNode_86[label=<A<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE6CE>  ]\nNode_87[label=<Mild<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_88[label=<3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E5F5E0>  ]\nNode_89[label=<B<BR/>> color=black style=\"rounded,filled\" fillcolor=<#E6550D>  ]\n\n}\n")
})

test_that("VennTable",{
  
  expect_identical(VennTable(vtree(FakeData,"Severity Age Group",check.is.na=TRUE,ptable=TRUE),sort=FALSE),
  structure(c("2", "4", "5", "35", "46", "", "", "4", "9", "11", 
"76", "100", "", "", "2", "4", "0", "0", "", "6", "13", "2", 
"0", "5", "0", "", "7", "15", "0", "0", "0", "0", "", "0", "0"
), .Dim = c(7L, 5L), .Dimnames = list(c("", "", "", "", "Total", 
"N", "pct"), c("n", "pct", "MISSING_Severity", "MISSING_Age", 
"MISSING_Group"))))
  
  expect_identical(VennTable(vtree(FakeData,"Ind1 Ind2",ptable=TRUE),markdown=TRUE),
"&nbsp;|&nbsp;|&nbsp;|&nbsp;|&nbsp;|&nbsp;|Total|N|%\n-|-|-|-|-|-|-|-|-\nn|1|10|11|12|12|46|&nbsp;|&nbsp;\n%|2|22|24|26|26|100|&nbsp;|&nbsp;\nInd2|&nbsp;|&nbsp;|&#10004;|&nbsp;|&#10004;|&nbsp;|23|50\nInd1|-|&#10004;|&nbsp;|&nbsp;|&#10004;|&nbsp;|22|48"
  )
  
})


test_that("CONSORT",{
  
  expect_identical(vtree(FakeRCT,"eligible randomized group followup analyzed",plain=TRUE,
  keep=list(eligible="Eligible",randomized="Randomized",followup="Followed up"),
  horiz=FALSE,showvarnames=FALSE,title="Assessed for eligibility"),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.46, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.18]\n\nedge[style=solid]\nNode_1->Node_2\nNode_2->Node_3\nNode_3->Node_4 Node_3->Node_5\nNode_4->Node_6\nNode_6->Node_7\nNode_5->Node_8\nNode_8->Node_9\n\nNode_1[label=<Assessed for eligibility<BR/>12> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Eligible<BR/>8 (67%)> color=black style=\"rounded,filled\" fillcolor=<#C6DBEF>  ]\nNode_3[label=<Randomized<BR/>7 (88%)> color=black style=\"rounded,filled\" fillcolor=<#9ECAE1>  ]\nNode_4[label=<A<BR/>4 (57%)> color=black style=\"rounded,filled\" fillcolor=<#6BAED6>  ]\nNode_5[label=<B<BR/>3 (43%)> color=black style=\"rounded,filled\" fillcolor=<#6BAED6>  ]\nNode_6[label=<Followed up<BR/>3 (75%)> color=black style=\"rounded,filled\" fillcolor=<#4292C6>  ]\nNode_7[label=<Analyzed<BR/>3 (100%)> color=black style=\"rounded,filled\" fillcolor=<#2171B5>  ]\nNode_8[label=<Followed up<BR/>3 (100%)> color=black style=\"rounded,filled\" fillcolor=<#4292C6>  ]\nNode_9[label=<Analyzed<BR/>3 (100%)> color=black style=\"rounded,filled\" fillcolor=<#2171B5>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
})


test_that("summary",{
  
  expect_identical(vtree(FakeData,summary="Score"),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\n\nedge[style=solid]\n\n\nNode_1[label=<46<BR/>Score<BR/>missing 2<BR/>mean 17.1 SD 20.2<BR/>med 9.0 IQR 4.0, 23.5<BR/>range 0.0, 99.0<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,summary="Severity"),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\n\nedge[style=solid]\n\n\nNode_1[label=<46<BR/>Severity<BR/>Mild: 19 (48%)<BR/>Moderate: 16 (40%)<BR/>Severe: 5 (12%)<BR/>NA: 6<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(
    {dessert <- build.data.frame(
      c(   "group","IceCream___1","IceCream___2","IceCream___3"),
      list("A",     1,             0,             0,              7),
      list("A",     1,             0,             1,              2),
      list("A",     0,             0,             0,              1),
      list("B",     1,             0,             1,              1),
      list("B",     1,             0,             0,              2), 
      list("B",     0,             1,             1,              1),
      list("B",     0,             0,             0,              1))
    attr(dessert$IceCream___1,"label") <- "Ice cream (choice=Chocolate)"
    attr(dessert$IceCream___2,"label") <- "Ice cream (choice=Vanilla)"
    attr(dessert$IceCream___3,"label") <- "Ice cream (choice=Strawberry)"
    vtree(dessert,summary="stemc:IceCream",splitwidth=Inf)
    },
    structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\n\nedge[style=solid]\n\n\nNode_1[label=<15<BR/>Chocolate: 9 (60%)<BR/>Chocolate+Strawberry: 3 (20%)<BR/>*None: 2 (13%)<BR/>Vanilla+Strawberry: 1 (7%)<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))  

  expect_identical(vtree(FakeData,"Severity",horiz=FALSE,showvarnames=FALSE,splitwidth=Inf,sameline=TRUE,summary=c("Score \nScore: mean %meanx% SD %SD%","Pre \nPre: range %range%")),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\n\nNode_1[label=<46<BR/>Score: mean 17.1 SD 20.2 mv=2<BR/>Pre: range -2.5, 2.1 mv=3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild, 19 (48%)<BR/>Score: mean 12.1 SD 14.6 mv=1<BR/>Pre: range -2.5, 1.9 mv=2<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46<BR/>Score: mean 17.1 SD 20.2 mv=2<BR/>Pre: range -2.5, 2.1 mv=3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate, 16 (40%)<BR/>Score: mean 17.1 SD 24.4 mv=1<BR/>Pre: range -1.5, 2.1 mv=1<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46<BR/>Score: mean 17.1 SD 20.2 mv=2<BR/>Pre: range -2.5, 2.1 mv=3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe, 5 (12%)<BR/>Score: mean 37.6 SD 16.8<BR/>Pre: range -1.4, 0.5<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46<BR/>Score: mean 17.1 SD 20.2 mv=2<BR/>Pre: range -2.5, 2.1 mv=3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA, 6<BR/>Score: mean 15.0 SD 19.4<BR/>Pre: range -1.5, 0.8<BR/>> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,"Sex",summary="Severity=Mild \nMild: %npct%%leafonly%"),
structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3\n\nNode_1[label=<46<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<F<BR/>27 (59%)<BR/>Mild: 11 (46%) mv=3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46<BR/>> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<M<BR/>19 (41%)<BR/>Mild: 8 (50%) mv=3<BR/>> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
})


test_that("text wrapping",{
  expect_identical(vtree(FakeData,"Severity",labelnode=list(Severity=c("Extremely bad"="Severe")),splitwidth=5),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild<BR/>19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate<BR/>16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Extremely<BR/>bad<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
})


test_that("labelnode",{
  expect_identical(vtree(FakeData,"Group Sex",horiz=FALSE,labelnode=list(Sex=c(Male="M",Female="F"))),
 structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Group  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3\nNode_2->Node_4 Node_2->Node_5\nNode_3->Node_6 Node_3->Node_7\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<A<BR/>24 (52%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<B<BR/>22 (48%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_4[label=<Female<BR/>18 (75%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_5[label=<Male<BR/>6 (25%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_6[label=<Female<BR/>9 (41%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<Male<BR/>13 (59%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
  
  expect_identical(vtree(FakeData,"Severity",labelvar=c("Severity"="How bad it is"),lsplitwidth=6),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>How bad<BR/>it is  </B></FONT></FONT>> shape=none margin=0]\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<Mild<BR/>19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<Moderate<BR/>16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<Severe<BR/>5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<NA<BR/>6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))  
})


test_that("legend",{
  expect_identical(vtree(FakeData,"Severity Sex",showlegend=TRUE,shownodelabels=FALSE),
  structure(list(x = list(diagram = "digraph vtree {\ngraph [nodesep=0.1, ranksep=0.5, tooltip=\" \"]\nnode [fontname = Helvetica, fontcolor = black,shape = rectangle, color = black, tooltip=\" \",margin=0.1]\nrankdir=LR;\nNode_L0_0 [style=invisible]\n\nsubgraph cluster_1 {\nstyle=rounded\ncolor=<#bdbdbd>\n{rank=same Node_L1_0 Node_L1_1 Node_L1_2 Node_L1_3 Node_L1_4}\nNode_L1_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#DE2D26\"><B>Severity  </B></FONT></FONT>> shape=none margin=0]\nNode_L1_1[label=<<FONT POINT-SIZE=\"14\">Mild, 19 (48%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#FEE0D2> height=0]\nNode_L1_2[label=<<FONT POINT-SIZE=\"14\">Moderate, 16 (40%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#FC9272> height=0]\nNode_L1_3[label=<<FONT POINT-SIZE=\"14\">Severe, 5 (12%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#DE2D26> height=0]\nNode_L1_4[label=<<FONT POINT-SIZE=\"14\">NA, 6</FONT>> color=black  style=\"rounded,filled\" fillcolor=<white> height=0]\n}\nNode_L0_0 -> Node_L1_0 [style=invisible arrowhead=none]\n\nsubgraph cluster_2 {\nstyle=rounded\ncolor=<#bdbdbd>\n{rank=same Node_L2_0 Node_L2_1 Node_L2_2}\nNode_L2_0[label=<<FONT POINT-SIZE=\"18\"><FONT COLOR=\"#3182BD\"><B>Sex  </B></FONT></FONT>> shape=none margin=0]\nNode_L2_1[label=<<FONT POINT-SIZE=\"14\">F, 27 (59%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#DEEBF7> height=0]\nNode_L2_2[label=<<FONT POINT-SIZE=\"14\">M, 19 (41%)</FONT>> color=black  style=\"rounded,filled\" fillcolor=<#3182BD> height=0]\n}\nNode_L1_0 -> Node_L2_0 [style=invisible arrowhead=none]\n\nedge[style=solid]\nNode_1->Node_2 Node_1->Node_3 Node_1->Node_4 Node_1->Node_5\nNode_2->Node_6 Node_2->Node_7\nNode_3->Node_8 Node_3->Node_9\nNode_4->Node_10 Node_4->Node_11\nNode_5->Node_12 Node_5->Node_13\n\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_2[label=<19 (48%)> color=black style=\"rounded,filled\" fillcolor=<#FEE0D2>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_3[label=<16 (40%)> color=black style=\"rounded,filled\" fillcolor=<#FC9272>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_4[label=<5 (12%)> color=black style=\"rounded,filled\" fillcolor=<#DE2D26>  ]\nNode_1[label=<46> color=black style=\"rounded,filled\" fillcolor=<#EFF3FF>]\nNode_5[label=<6> color=black style=\"rounded,filled\" fillcolor=<white>  ]\nNode_6[label=<11 (58%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_7[label=<8 (42%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_8[label=<11 (69%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_9[label=<5 (31%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_10[label=<2 (40%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_11[label=<3 (60%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\nNode_12[label=<3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#DEEBF7>  ]\nNode_13[label=<3 (50%)> color=black style=\"rounded,filled\" fillcolor=<#3182BD>  ]\n\n}\n", 
    config = list(engine = "dot", options = NULL)), width = NULL, 
    height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, 
        padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL), 
        browser = list(defaultWidth = NULL, defaultHeight = NULL, 
            padding = NULL, fill = FALSE, external = FALSE), 
        knitr = list(defaultWidth = NULL, defaultHeight = NULL, 
            figure = TRUE)), dependencies = NULL, elementId = NULL, 
    preRenderHook = NULL, jsHooks = list()), class = c("grViz", 
"htmlwidget"), package = "DiagrammeR"))
})
  