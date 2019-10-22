vtree - An R package for calculating and drawing variable trees
=====

[![CRAN
status](http://www.r-pkg.org/badges/version/vtree)](https://cran.r-project.org/package=vtree)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/vtree)](https://cranlogs.r-pkg.org/badges/grand-total/vtree)

*Variable trees display information about nested subsets of a data frame, in which the subsetting is defined by the values of categorical variables.*

<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/v1.png" width="250">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/t7.png" width="200">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/t1.png" width="300">

# Installation

You can install the current version of vtree (version 2.0.0) from CRAN

```
install.packages("vtree")
```

Or the get the latest release candidate from github (also version 2.0.0)

```
remotes::install_github("nbarrowman/vtree@v3.0.0")
```

To get the vignette as well you need to use this longer command:

```
remotes::install_github("nbarrowman/vtree@v2.0.0",build_opts = c("--no-resave-data", "--no-manual"))
```

# More information

* vtree is available on [CRAN](https://cran.r-project.org/package=vtree)

* A comprehensive introduction to vtree is available in the [vignette](https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html)

* A [cheat sheet](https://github.com/nbarrowman/vtree/blob/master/cheatsheets/vtree_cheatsheet_2.pdf) (pdf) is available: 

* Examples: 
    * https://rpubs.com/nbarrowman/Data_exploration_with_vtree
    * https://rpubs.com/nbarrowman/missing_values_vtree
    * https://rpubs.com/nbarrowman/pruning_a_variable_tree
    * https://rpubs.com/nbarrowman/CONSORTvtree
