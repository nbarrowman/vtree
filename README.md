vtree - An R package for calculating & drawing variable trees
=====

[![CRAN
status](http://www.r-pkg.org/badges/version/vtree)](https://cran.r-project.org/package=vtree)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/vtree)](https://cranlogs.r-pkg.org/badges/grand-total/vtree)

*Variable trees are diagrams that display information about nested subsets of a data frame.*

<p align="center">
<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/v1.png" width="280">
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/t7.png" width="200">
&nbsp;&nbsp;&nbsp;
<img src="https://github.com/nbarrowman/vtree/blob/master/cheatsheets/png/t1.png" width="280">
</p>

# Installation

You can install the current version of vtree (version 5.1.9) from CRAN

```
install.packages("vtree")
```

Or install the latest development release (v5.4.0) from github

```
remotes::install_github("nbarrowman/vtree@v5.4.0")
```

To get the vignette as well you need to specify `build_vignettes=TRUE`:

```
remotes::install_github("nbarrowman/vtree@v5.4.0",build_vignettes=TRUE)
```

# More information

* vtree is available on [CRAN](https://cran.r-project.org/package=vtree)

* A comprehensive introduction to vtree is available in the [vignette](https://cran.r-project.org/web/packages/vtree/vignettes/vtree.html)

* A [cheat sheet](https://nbarrowman.github.io/cheatsheets/vtree_cheatsheet_5.0.0.pdf) (pdf) is available

* Examples: 
    * https://rpubs.com/nbarrowman/Data_exploration_with_vtree
    * https://rpubs.com/nbarrowman/missing_values_vtree
    * https://rpubs.com/nbarrowman/pruning_a_variable_tree
    * https://rpubs.com/nbarrowman/CONSORT-style

* Please report bugs or other issues [here](https://github.com/nbarrowman/vtree/issues).
