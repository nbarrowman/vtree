# vtree 5.4.6

## Changes

* Small changes to the vignette.

* Added new parameter `thousands` to specify separator (such as ",") between thousands.

* Fixed a small bug where the `%list%` summary option used `digits` rather than `cdigits`.

# vtree 5.4.0

## Changes

* Removed one color palette (`RdYlGn`) which is not colorblind-friendly. Also added a new colorblind-friendly palette (`Greys`). These palettes were pre-calculated using `RColorBrewer`.

* Added a single-line information message when the package is loaded in interactive mode.

* Added automatic text color (black for lighter-colored nodes, white for lighter-colored nodes).

* Added `tsummary` parameter for targeted summaries.

* Added `showuniform` parameter (TRUE by default). When FALSE, variables with no variation are not included.

* Added `words` parameter to allow specification of a tree without any counts.

* Added `showrootcount` parameter to control whether count is shown in the root node.

* Added `the.matrix` dataset based on the 3 Matrix movies.

* Added targetted versions of the pruning parameters, namely: `tprune`, `tkeep`, `tfollow`, and `tprunebelow`.

* When `ImageFileOnly=TRUE`, graphics files are now numbered successively, even when not knitting.

* Addeded `justtext` parameter so that justification of the extra text can be different.

* Added the following options to summary: `%nosort%`, `%freqx%`, `%freqpctx%`, `%freqpctx_%`.



# vtree 5.1.8

## Changes

* Variables can now be specified as a formula with no left-hand side.

* Added support for generating PDF images.

* Added `none:` and `notall:` for variable specifications and summaries (these are the complements of `any:` and `all:`} respectively).

* Added prefixes `anyx:`, `nonex:`, `allx:`, and `notallx:` where the `x` indicates that missing values will be removed.

* PNG files are now automatically named `vtree001.png`, `vtree002.png`, etc. (PDF files are named `vtree001.pdf`, etc.)

* Support was added for embedded variable trees in Sweave.

* New `trim` parameter to support trim feature in the `\includegraphics` LaTeX command.

* Renamed `z` parameter to `data`.

* Reorganized parameters in help file and added details.

* New parameter `showlegendsum` to show summary in legend nodes.

* New parameter `font` to specify font to use.

* Extended wildcard matching in variable specifications and summary specifications to match `this*that` expressions and `this#that` expressions.


## Bug fixes

* Codes for less-than-or-equal and greater-than-or-equal were removed due to rendering issues with DiagrammeRsvg.

* Fixed bug when using `showlegend=TRUE` with `pattern=TRUE` or with `check.is.na=TRUE`.

* Fix by trafficonese for tabPanels.


# vtree 5.0.0

## Changes

* Variable labels are shown differently: no longer boldface, and also larger (24 point instead of 18 point). (The metrics for boldface font seemed to be not quite right, which sometimes resulted in truncated text.)

* Revamped legends.

* New `sortfill` parameter. Specifying `sortfill=TRUE` fills nodes with gradient colors
in sorted order according to the node count.

* `vtree` return value now has an `info` attribute,
which is a nested list that gives the count for each node

* When no summary codes for statistics are specified,
a default summary is shown, depending on the type of variable.

* New variable prefix `i:` for "intersections" of variables

* New variable prefixes `any:` and `all:`

* New summary codes `%range%`, `%freq%`, and `%freqpct%`.

* New variant summary codes (ending in x to disregard missing values,
ending in _ to put on separate lines).

* New REDCap variable prefix `r:` and suffix `@`

* When `vars` is not specified, only the root node is shown. (Previously vtree would try to use as many variables as possible.)

* Added more informative error messages.

* Added additional color palettes.

* The old `stem:` and `rc:` variable specification and summary prefixes
for REDCap checkbox variables are now deprecated.

* The `lsplitwidth` parameter is now deprecated; `vsplitwidth` (for splitting variable names) should be used instead.

* `splitwidth` now applies to legend nodes as well as ordinary nodes.

## Bug fixes

* Fixed issue with Shiny where vtree not visible immediately.

* Fixed problem with summaries when three or fewer observations are present.

* Fixed bug with HTML conversion of labels in legends.

* Fixed problem with DOT script when there is no root node.

* Fixed bug with variable names showing up white when `fillcolor="white"`.

* Fixed bug with pattern tables when `prunesmaller` was specified.



# vtree 4.0.0

## Changes

* New parameter, `just`, to specify text justification.

* New parameter, `arrowhead`, to specify the type of arrowhead
(or `none`).

* Added summary variable specification `!=`

* Sebastian Gatscha added Shiny functionality
and the svg-pan-zoom JavaScript library.
The following functions were added:
`init_js`, `inlineCssSetup`, `renderVtree`, `use_svgzoom`, `vtreeOutput`.

* New function `svtree` uses Shiny functionality.

* Modified outputs of `vtree` to make it work better.

* Thanks to `knitr::asis_output()`,
calling `vtree` from R Markdown when generating a PNG file has been simplified.

* Specifying `showvarinnode=TRUE` shows variable name in each node.

* When no variable list is specified,
`vtree` reports included variables, excluded discrete variables, 
other excluded variables.

## Bug fixes

* Fixed a bug with missing value nodes.

* Fixed problem with return value when running chunk from R Studio.


# vtree 3.0.0

## Changes

* Extensive revision of vignette.

## Bug fixes

* Fixed a bug that occurred when `pattern=TRUE` and a single variable was specified.

* Fixed a bug that occurred when `showvarnames=FALSE` but `labelvar` was set.

## New features

* The `keep` parameter will not prune missing value nodes when `vp=TRUE`.

* There is a new summary code, `%sum%`, to show the sum of values.

* `prunesmaller` parameter to remove nodes with small counts.

* In variable specifications, `*` matches multiple variable name endings and `#` matches variable names ending with numeric digits.

* When called while knitting an R Markdown document, PNG files are now automatically generated and embedded.

* `pngknit=FALSE` disables PNG generation while knitting. Returns an `htmlwidget` instead.

* The new function `VennTable` reformats a pattern table output by `vtree` for indicator (0/1) variables.

* The new function `build.data.frame` makes it easy to create a data frame for use with vtree.

* If no variable names are provided, `vtree` will use, in order, all of the variables in the data frame.

* Multi-variable calculations like `x/y` can be used in the argument of the `summary` parameter.


# vtree 2.0.0

## Changes

* `check.is.na` now  uses `pattern=TRUE` by default.

* Changed the meaning of the `%node%` summary code,
which had previously (and confusingly!) been used to specify the *variable* 
in whose nodes summary information should be shown.
Now `%node=n%` specifies node `n` and `%var=v%` specifies variable `v`.
  
* Additional text produced by `text` or `ttext` or `summary`
now starts on the *same line* as the number of observations and percentage.
In order to put this text on a new line, it must begin with `\n`.

* When `seq=TRUE` the first-level nodes are ordered from least frequent sequence to most frequent sequence.
(The same applies for the new `pattern` parameter.)

* Extensive revisions to the vignette.

## New features

* `pattern` parameter for unordered sequences of variables

* `ptable` parameter to generate pattern tables

* Variable specification modifiers: `stem:` for REDCap checkboxes, `is.na:`,
`variable=value`, `variable<value`, `variable>value`, `tri:`

* `choicechecklist` parameter: When REDCap checklists are specified using the `stem:` syntax, 
automatically extract the names of choices and use them as variable names.

* `%pct%` summary code to show percentage (not frequency)

* `%listlines%` summary code to separate items with a new line (unlike %list% which uses commas).

* Summary variable specification modifiers:
`variable=value`, `variable<value`, `variable>value`

* `tlabelnode` parameter: targeted node labels

* `ttext` parameter: targeted text

* New function `crosstabToCases` to convert crosstabulated data to a data frame of cases

* `runsummary` parameter to control which nodes show summaries depending on node contents

* `retain` parameter to specify additional variables that need to be available to execute the functions in runsummary

* `varminwidth`, `varminheight` parameters to control node sizes for specified variables

* `varlabelloc` parameter to control vertical justification for nodes of specified variables


# vtree 1.0.0 

This is a major release that incorporates several new features.


## New default features

* Automatic variable coloring using gradients.

* More compact display (reduced node separation and tighter margins).

* The color of the root node can be specified using the `rootfillcolor` parameter.

* All missing-value nodes are colored white by default.
This can be changes using the `NAfillcolor` parameter.

* By default, the root node now has no title.

* **NOTE** Display from version 0.1.4 can be obtained by specifying `plain=TRUE`.
This sets:

    * variable-specific shades of blue

    * greater node separation
  
    * wider margins
  
    * missing-value nodes have the same color (shade of blue) as all other nodes at that level
  

## New optional features

* `showlevels` is now deprecated. Use `showvarnames` instead.

* Legends, with marginal frequencies and percentages.

* To put node labels on the same line as frequencies and percentages, specify `sameline=TRUE`.

* A new parameter, `squeeze` takes a value between 0 and 1,
and controls how the tightly-packed the display is.

* "Sequences" of variables can be displayed by specifying `seq=TRUE`.

* Additional `summary` codes: 
`%node=n%` to show summary information only in specified node.
`%trunc=n%` to truncate the summary to the first *n* characters.

* Settings for multi-way intersections can be specified with `Venn=TRUE`.

* `graphattr`, `nodeattr`, and `edgeattr`
parameters allow additional Graphviz attributes to be set.

* Other new parameters: `palette`, `gradient`, `revgradient`, `singlecolor`,
`showpct`, `showlpct`, `showcount`, `varnamepointsize`, `lsplitwidth`,
`margin`, `showempty`, `prunelone`



# vtree 0.1.4 (initial release version)
