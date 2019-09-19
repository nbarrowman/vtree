# vtree 2.2.3

## Changes

## Bug fixes

* Fixed a bug that occurred when `pattern=TRUE` and a single variable was specified.

## New features

* There is a new summary code, `%sum%`, to show the sum of values.

* `prunesmaller` parameter to remove nodes with small counts.

* In variable specifications, `*` matches multiple variable name endings and `#` matches variable names ending with numeric digits.

* When called while knitting an R Markdown document, PNG files are now automatically generated and embedded.

* `pngknit=FALSE` disables PNG generation while knitting. Returns an `htmlwidget` instead.

* The new function `VennTable` reformats a "pattern table" output by `vtree` for indicator (0/1) variables.

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
