# shapeshiftr 0.2

## Misc

* Changes how multithreaded is handled in the package. There is now a single underlying cluster used by all functions that iterate over a collection. This makes the repetitive argument `threads` in every function unnecessary and allows for cleaner calls

# shapeshiftr 0.1

This package aims at providing a simple and efficient interface for manipulation and 
transformation of data for training forecasting models. This is effectively done through 
"slicing" operations, which transform standard tabular time series data into data.frames usable 
for model fitting/forecasting, as well as diverse transforms.
