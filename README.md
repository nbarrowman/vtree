vtree - An R package for calculating & drawing variable trees
=====

[![CRAN
status](http://www.r-pkg.org/badges/version/vtree)](https://cran.r-project.org/package=vtree)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/vtree)](https://cranlogs.r-pkg.org/badges/grand-total/vtree)

*Variable trees are diagrams that display information about nested subsets of a data frame.*

<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/v1.png" width="250">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/t7.png" width="200">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/t1.png" width="300">

# Notes

See installation details below.

Important notes about the current version of vtree (version 3.0.0) on CRAN:

* Compared to earlier versions, there are changes in how vtree supports R Markdown. By default when `vtree` is called from R Markdown, a PNG file is automatically generated and markdown code to embed it is generated. To generate an htmlwidget instead (as in older versions of vtree), specify `pngknit=FALSE`.

* The latest vtree release candidate (version 3.0.4) streamlines this functionality and corrects a bug.

* The latest vtree release candidate (version 3.0.4) also corrects a bug related to pruning with missing values.

# Installation

You can install the current version of vtree (version 3.0.0) from CRAN

```
install.packages("vtree")
```

Or install the latest release candidate from github (version 3.0.4)

```
remotes::install_github("nbarrowman/vtree@v3.0.4")
```

To get the vignette as well you need to specify `build_vignettes=TRUE`:

```
remotes::install_github("nbarrowman/vtree@v3.0.4",build_vignettes=TRUE)
```

# More information

* vtree is available on [CRAN](https://cran.r-project.org/package=vtree)

* A comprehensive introduction to vtree is available in the [vignette](https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html)

* A [cheat sheet](https://github.com/rstudio/cheatsheets/raw/master/vtree.pdf) (pdf) is available: 

* Examples: 
    * https://rpubs.com/nbarrowman/Data_exploration_with_vtree
    * https://rpubs.com/nbarrowman/missing_values_vtree
    * https://rpubs.com/nbarrowman/pruning_a_variable_tree
    * https://rpubs.com/nbarrowman/CONSORTvtree

* Please report bugs or other issues [here](https://github.com/nbarrowman/vtree/issues).
