# docs

* Massive documentation enhancements
  * detailed docs and examples for `pipes` system
  * detailed docs and examples for `slice` and `slice_artifact`
  * added two vignettes: one for slicing and one for pipes

# pipes

## New features

* Adds `pipes` system to the packaged, focused on automation of data manipulation for engeneering
  features

# shapeshiftr 0.3

## New features

* Subsetting of `slice_artifacts` can now be done by integer positions, on top of by variable name
  of index string
* Adds `names` and `names<-` methods for `slice_artifact` objects

## Misc

* Attribute `L` now has names of variables in the slice
* Method `merge.slice_artifact` and function `combine_features` now keep attribute `L` pertaining 
  to existing variables, from original `slice_artifact` instead of `NA`
* Method `[.slice_artifact` now keeps `L` only on subsetted variables

# shapeshiftr 0.2

## Misc

* Changes how multithreaded is handled in the package. There is now a single underlying cluster used by all functions that iterate over a collection. This makes the repetitive argument `threads` in every function unnecessary and allows for cleaner calls

# shapeshiftr 0.1

This package aims at providing a simple and efficient interface for manipulation and 
transformation of data for training forecasting models. This is effectively done through 
"slicing" operations, which transform standard tabular time series data into data.frames usable 
for model fitting/forecasting, as well as diverse transforms.
