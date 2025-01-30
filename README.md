
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shapeshiftr

<!-- badges: start -->

[![R-CMD-check](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

Package `shapeshiftr` aims at providing a simple and efficient interface
for manipulation and transformation of data for training forecasting
models. This is effectively done through “slicing” operations, which
transform standard tabular time series data into data.frames usable for
model fitting/forecasting, as well as diverse transforms (such as
wavelets).

## Installation

You can install the development version of shapeshiftr from
[GitHub](https://github.com/lkhenayfis/shapeshiftr) with:

``` r
# install.packages("devtools")
devtools::install_github("lkhenayfis/shapeshiftr")
```

## Example

This is a basic example that demonstrates how `shapeshiftr` is meant to
aid in data manipulation in it’s canonical use case.

Suppose there are two datasets:

  - `obs`, containing observed data of explanatory variables “X1”, “X2”
    and target variable “Y”

<!-- end list -->

``` r
obs  <- simple_dt_date[, .(date, X1, X2, Y)]
print(head(obs))
#>          date X1  X2   Y
#> 1: 2025-01-02 16 -18 132
#> 2: 2025-01-03  5 -17 122
#> 3: 2025-01-04 12 -16 138
#> 4: 2025-01-05 15 -19 137
#> 5: 2025-01-06  9  -7 125
#> 6: 2025-01-07 19  -6 131
```

  - `pred`, containing predicted variables of the regressors “X1” and
    “X2” (i.e. numerical weather forecasts)

<!-- end list -->

``` r
pred <- keyed_dt_date[, .(date, target_date, X1, X2)]
print(head(pred, 10))
#>           date target_date X1  X2
#>  1: 2025-01-02  2025-01-02 15 -14
#>  2: 2025-01-02  2025-01-03 12 -12
#>  3: 2025-01-02  2025-01-04  3 -16
#>  4: 2025-01-02  2025-01-05 20 -13
#>  5: 2025-01-02  2025-01-06  5  -9
#>  6: 2025-01-03  2025-01-03 14  -6
#>  7: 2025-01-03  2025-01-04  4  -7
#>  8: 2025-01-03  2025-01-05 19  -8
#>  9: 2025-01-03  2025-01-06 13  -5
#> 10: 2025-01-03  2025-01-07 18 -17
```

The goal is to forecast “Y” two days ahead using 3 lags of “X1” and
“X2”, plus their forecasted values two days ahead. To build this
dataset, we use

``` r
lag_X <- slice(obs, "date", variables = c("X1", "X2"), L = -3:-1,
    names = c("lag_X1", "lag_X2"))
lead_X <- slice(pred, "date", "target_date", variables = c("X1", "X2"), L = 0:2,
    names = c("lead_X1", "lead_X2"))
target <- slice(obs, "date", variables = "Y", L = 2)

X <- merge(lag_X, lead_X)
training_data <- merge(X, target)

dt <- as.data.table(training_data)
```

The resulting data.table is

``` r
print(head(dt, 10))
#>          index lag_X1_1 lag_X1_2 lag_X1_3 lag_X2_1 lag_X2_2 lag_X2_3 lead_X1_1
#>  1: 2025-01-03       NA       NA       16       NA       NA      -18        14
#>  2: 2025-01-04       NA       16        5       NA      -18      -17        16
#>  3: 2025-01-05       16        5       12      -18      -17      -16        10
#>  4: 2025-01-06        5       12       15      -17      -16      -19        15
#>  5: 2025-01-07       12       15        9      -16      -19       -7        14
#>  6: 2025-01-08       15        9       19      -19       -7       -6        16
#>  7: 2025-01-09        9       19        6       -7       -6      -13        10
#>  8: 2025-01-10       19        6        4       -6      -13      -10        15
#>  9: 2025-01-11        6        4        2      -13      -10       -2        14
#> 10: 2025-01-12        4        2        7      -10       -2       -1        16
#>     lead_X1_2 lead_X1_3 lead_X2_1 lead_X2_2 lead_X2_3 Y_1
#>  1:         4        19        -6        -7        -8 137
#>  2:         7         2        -2       -15        -4 125
#>  3:         9         8       -10       -20       -18 131
#>  4:        12         3       -14       -12       -16 123
#>  5:         4        19        -6        -7        -8 128
#>  6:         7         2        -2       -15        -4 126
#>  7:         9         8       -10       -20       -18 139
#>  8:        12         3       -14       -12       -16 136
#>  9:         4        19        -6        -7        -8 124
#> 10:         7         2        -2       -15        -4 127
```
