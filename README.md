
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shapeshiftr

<!-- badges: start -->

[![R-CMD-check](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lkhenayfis/shapeshiftr/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/lkhenayfis/shapeshiftr/graph/badge.svg?token=JW6LQ7TO2U)](https://codecov.io/gh/lkhenayfis/shapeshiftr)
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

## Main functionalities

### Pipes

The `shapeshiftr` package provides a flexible pipe system for defining
data transformations through closure generators. Pipes allow you to
define transformation workflows that can be parsed and evaluated on
data.

#### Simple transformation pipes

A pipe transformation is defined by a closure generator - a function
that returns another function. Here’s a simple example that computes
summary statistics:

``` r
# Define a closure generator for summary statistics
gen_closure_summary <- function(...) function(x) summary(x)

# Define a pipe that applies this transformation to mtcars
raw_pipe <- list(
    on = "mtcars",
    transforms = list(
        list(fun = "gen_closure_summary")
    )
)

# Parse the pipe to generate the closure
parsed_pipe <- parse_single_pipe(raw_pipe)

# Evaluate the pipe on the data
result <- eval_single_pipe(parsed_pipe, env = list(mtcars = mtcars))
result
#>       mpg             cyl             disp             hp       
#>  Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
#>  1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
#>  Median :19.20   Median :6.000   Median :196.3   Median :123.0  
#>  Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
#>  3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
#>  Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
#>       drat             wt             qsec             vs        
#>  Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
#>  1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
#>  Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
#>  Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
#>  3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
#>  Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
#>        am              gear            carb      
#>  Min.   :0.0000   Min.   :3.000   Min.   :1.000  
#>  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
#>  Median :0.0000   Median :4.000   Median :2.000  
#>  Mean   :0.4062   Mean   :3.688   Mean   :2.812  
#>  3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
#>  Max.   :1.0000   Max.   :5.000   Max.   :8.000
```

#### Pipes with computed parameters

More sophisticated pipes can perform computations and use the results as
parameters in the returned closure. This example shows a scaling
transformation that centers data to zero mean and scales to unit
variance:

``` r
# Define a closure generator that computes scaling parameters
gen_closure_scale <- function(x, ...) {
    # Compute mean and sd from the training data x (happens once during parsing)
    means <- colMeans(x, na.rm = TRUE)
    sds <- apply(x, 2, sd, na.rm = TRUE)
    
    # Return a closure that uses these pre-computed parameters
    function(x) {
        as.data.frame(scale(x, center = means, scale = sds))
    }
}

# Define a pipe that scales the data
raw_pipe <- list(
    on = "mtcars",
    transforms = list(
        list(fun = "gen_closure_scale")
    )
)

# Parse and evaluate
parsed_pipe <- parse_single_pipe(raw_pipe, env = list(mtcars = mtcars), enclos = environment())
scaled_data <- eval_single_pipe(parsed_pipe, env = list(mtcars = mtcars), enclos = environment())
head(scaled_data)
#>                          mpg        cyl        disp         hp       drat
#> Mazda RX4          0.1508848 -0.1049878 -0.57061982 -0.5350928  0.5675137
#> Mazda RX4 Wag      0.1508848 -0.1049878 -0.57061982 -0.5350928  0.5675137
#> Datsun 710         0.4495434 -1.2248578 -0.99018209 -0.7830405  0.4739996
#> Hornet 4 Drive     0.2172534 -0.1049878  0.22009369 -0.5350928 -0.9661175
#> Hornet Sportabout -0.2307345  1.0148821  1.04308123  0.4129422 -0.8351978
#> Valiant           -0.3302874 -0.1049878 -0.04616698 -0.6080186 -1.5646078
#>                             wt       qsec         vs         am       gear
#> Mazda RX4         -0.610399567 -0.7771651 -0.8680278  1.1899014  0.4235542
#> Mazda RX4 Wag     -0.349785269 -0.4637808 -0.8680278  1.1899014  0.4235542
#> Datsun 710        -0.917004624  0.4260068  1.1160357  1.1899014  0.4235542
#> Hornet 4 Drive    -0.002299538  0.8904872  1.1160357 -0.8141431 -0.9318192
#> Hornet Sportabout  0.227654255 -0.4637808 -0.8680278 -0.8141431 -0.9318192
#> Valiant            0.248094592  1.3269868  1.1160357 -0.8141431 -0.9318192
#>                         carb
#> Mazda RX4          0.7352031
#> Mazda RX4 Wag      0.7352031
#> Datsun 710        -1.1221521
#> Hornet 4 Drive    -1.1221521
#> Hornet Sportabout -0.5030337
#> Valiant           -1.1221521
```

#### Chaining multiple transformations

Pipes can chain multiple transformations together. Each transformation
receives the output of the previous one:

``` r
# Define a filter closure generator
gen_closure_filter <- function(by, value, ...) {
    function(x) x[x[[by]] == value, ]
}

# Combine filtering and summary
raw_pipe <- list(
    on = "mtcars",
    transforms = list(
        list(fun = "gen_closure_filter", by = "cyl", value = 6),
        list(fun = "gen_closure_summary")
    )
)

# Parse and evaluate
parsed_pipe <- parse_single_pipe(raw_pipe, env = list(mtcars = mtcars), enclos = environment())
result <- eval_single_pipe(parsed_pipe, env = list(mtcars = mtcars), enclos = environment())
result
#>       mpg             cyl         disp             hp             drat      
#>  Min.   :17.80   Min.   :6   Min.   :145.0   Min.   :105.0   Min.   :2.760  
#>  1st Qu.:18.65   1st Qu.:6   1st Qu.:160.0   1st Qu.:110.0   1st Qu.:3.350  
#>  Median :19.70   Median :6   Median :167.6   Median :110.0   Median :3.900  
#>  Mean   :19.74   Mean   :6   Mean   :183.3   Mean   :122.3   Mean   :3.586  
#>  3rd Qu.:21.00   3rd Qu.:6   3rd Qu.:196.3   3rd Qu.:123.0   3rd Qu.:3.910  
#>  Max.   :21.40   Max.   :6   Max.   :258.0   Max.   :175.0   Max.   :3.920  
#>        wt             qsec             vs               am        
#>  Min.   :2.620   Min.   :15.50   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:2.822   1st Qu.:16.74   1st Qu.:0.0000   1st Qu.:0.0000  
#>  Median :3.215   Median :18.30   Median :1.0000   Median :0.0000  
#>  Mean   :3.117   Mean   :17.98   Mean   :0.5714   Mean   :0.4286  
#>  3rd Qu.:3.440   3rd Qu.:19.17   3rd Qu.:1.0000   3rd Qu.:1.0000  
#>  Max.   :3.460   Max.   :20.22   Max.   :1.0000   Max.   :1.0000  
#>       gear            carb      
#>  Min.   :3.000   Min.   :1.000  
#>  1st Qu.:3.500   1st Qu.:2.500  
#>  Median :4.000   Median :4.000  
#>  Mean   :3.857   Mean   :3.429  
#>  3rd Qu.:4.000   3rd Qu.:4.000  
#>  Max.   :5.000   Max.   :6.000
```

### Slicing

This is a basic example that demonstrates how `shapeshiftr` is meant to
aid in data manipulation in it’s canonical use case.

Suppose there are two datasets:

  - `obs`, containing observed data of explanatory variables “X1”, “X2”
    and target variable “Y”

<!-- end list -->

``` r
obs  <- simple_dt_date[, .(date, X1, X2, Y)]
print(head(obs))
#>          date    X1    X2     Y
#>        <Date> <int> <int> <int>
#> 1: 2025-01-02    16   -18   132
#> 2: 2025-01-03     5   -17   122
#> 3: 2025-01-04    12   -16   138
#> 4: 2025-01-05    15   -19   137
#> 5: 2025-01-06     9    -7   125
#> 6: 2025-01-07    19    -6   131
```

  - `pred`, containing predicted variables of the regressors “X1” and
    “X2” (i.e. numerical weather forecasts)

<!-- end list -->

``` r
pred <- keyed_dt_date[, .(date, target_date, X1, X2)]
print(head(pred, 10))
#>           date target_date    X1    X2
#>         <Date>      <Date> <int> <int>
#>  1: 2025-01-02  2025-01-02    15   -14
#>  2: 2025-01-02  2025-01-03    12   -12
#>  3: 2025-01-02  2025-01-04     3   -16
#>  4: 2025-01-02  2025-01-05    20   -13
#>  5: 2025-01-02  2025-01-06     5    -9
#>  6: 2025-01-03  2025-01-03    14    -6
#>  7: 2025-01-03  2025-01-04     4    -7
#>  8: 2025-01-03  2025-01-05    19    -8
#>  9: 2025-01-03  2025-01-06    13    -5
#> 10: 2025-01-03  2025-01-07    18   -17
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
#>         <Date>    <int>    <int>    <int>    <int>    <int>    <int>     <int>
#>  1: 2025-01-02       NA       NA       NA       NA       NA       NA        15
#>  2: 2025-01-03       NA       NA       16       NA       NA      -18        14
#>  3: 2025-01-04       NA       16        5       NA      -18      -17        16
#>  4: 2025-01-05       16        5       12      -18      -17      -16        10
#>  5: 2025-01-06        5       12       15      -17      -16      -19        15
#>  6: 2025-01-07       12       15        9      -16      -19       -7        14
#>  7: 2025-01-08       15        9       19      -19       -7       -6        16
#>  8: 2025-01-09        9       19        6       -7       -6      -13        10
#>  9: 2025-01-10       19        6        4       -6      -13      -10        15
#> 10: 2025-01-11        6        4        2      -13      -10       -2        14
#>     lead_X1_2 lead_X1_3 lead_X2_1 lead_X2_2 lead_X2_3   Y_1
#>         <int>     <int>     <int>     <int>     <int> <int>
#>  1:        12         3       -14       -12       -16   138
#>  2:         4        19        -6        -7        -8   137
#>  3:         7         2        -2       -15        -4   125
#>  4:         9         8       -10       -20       -18   131
#>  5:        12         3       -14       -12       -16   123
#>  6:         4        19        -6        -7        -8   128
#>  7:         7         2        -2       -15        -4   126
#>  8:         9         8       -10       -20       -18   139
#>  9:        12         3       -14       -12       -16   136
#> 10:         4        19        -6        -7        -8   124
```

## Full Example: Training and Inference Consistency

One of the most powerful features of the pipe system is its ability to
maintain transformation consistency between training and inference. When
training a forecasting model, you often need to apply transformations
(like scaling) to your data. During inference, you must apply the *exact
same* transformations using the *same parameters* computed from the
training data.

This example demonstrates how to:

1.  Create pipes that scale data then perform slicing
2.  Parse and evaluate pipes on training data (computing scaling
    parameters)
3.  Re-use the same parsed pipes on test data (applying the same scaling
    parameters)

<!-- end list -->

``` r
# Split the data into training (first half) and test (second half)
n <- nrow(obs)
train_obs <- obs[1:floor(n / 2)]
test_obs <- obs[(floor(n / 2) + 1):n]
```

Now we define closure generators for scaling and slicing:

``` r
# Closure generator that computes scaling parameters and returns a function
# that applies those parameters
gen_closure_scale_cols <- function(x, cols, ...) {
    # Compute scaling parameters from x (training data) - happens ONCE during parsing
    means <- colMeans(x[, cols, with = FALSE], na.rm = TRUE)
    sds <- sapply(x[, cols, with = FALSE], sd, na.rm = TRUE)

    # Return a closure that uses these pre-computed, fixed parameters
    function(x) {
        result <- copy(x)
        for (col in cols) {
            result[[col]] <- (x[[col]] - means[col]) / sds[col]
        }
        result
    }
}

# Closure generator for slicing
gen_closure_slice_simple <- function(x, index_var, variables, L, ...) {
    # The returned closure performs slicing
    # x is the training data but we don't use it here - slicing doesn't need "training"
    function(x) {
        result <- slice(x, index_var, variables = variables, L = L)
        as.data.table(result)
    }
}
```

Define the pipe that first scales, then slices:

``` r
# Create a pipe that scales X1, X2, Y, then creates lagged features
pipe_def <- list(
    on = "data",
    transforms = list(
        # First: scale the data
        list(
            fun = "gen_closure_scale_cols",
            cols = c("X1", "X2", "Y")
        ),
        # Then: create lagged features
        list(
            fun = "gen_closure_slice_simple",
            index_var = "date",
            variables = c("X1", "X2", "Y"),
            L = -2:0
        )
    )
)
```

**Step 1: Parse and evaluate on training data**

``` r
# Parse the pipe using TRAINING data - this computes scaling parameters from training data
parsed_pipe_train <- parse_single_pipe(pipe_def,
    env = list(data = train_obs),
    enclos = environment())

# Evaluate on training data
train_result <- eval_single_pipe(parsed_pipe_train,
    env = list(data = train_obs),
    enclos = environment())

cat("Training data after scaling and slicing:\n")
#> Training data after scaling and slicing:
print(head(train_result, 5))
#>         index       X1_1       X1_2        X1_3       X2_1       X2_2
#>        <Date>      <num>      <num>       <num>      <num>      <num>
#> 1: 2025-01-02         NA         NA  1.13629727         NA         NA
#> 2: 2025-01-03         NA  1.1362973 -0.78666734         NA -1.0638039
#> 3: 2025-01-04  1.1362973 -0.7866673  0.43703741 -1.0638039 -0.9139724
#> 4: 2025-01-05 -0.7866673  0.4370374  0.96148230 -0.9139724 -0.7641408
#> 5: 2025-01-06  0.4370374  0.9614823 -0.08740748 -0.7641408 -1.2136354
#>          X2_3        Y_1        Y_2        Y_3
#>         <num>      <num>      <num>      <num>
#> 1: -1.0638039         NA         NA  0.3017183
#> 2: -0.9139724         NA  0.3017183 -1.2862725
#> 3: -0.7641408  0.3017183 -1.2862725  1.2545127
#> 4: -1.2136354 -1.2862725  1.2545127  1.0957136
#> 5:  0.5843430  1.2545127  1.0957136 -0.8098753
print(tail(train_result, 5))
#>         index        X1_1        X1_2       X1_3       X2_1       X2_2
#>        <Date>       <num>       <num>      <num>      <num>      <num>
#> 1: 2025-01-07  0.96148230 -0.08740748  1.6607422 -1.2136354  0.5843430
#> 2: 2025-01-08 -0.08740748  1.66074216 -0.6118524  0.5843430  0.7341745
#> 3: 2025-01-09  1.66074216 -0.61185237 -0.9614823  0.7341745 -0.3146462
#> 4: 2025-01-10 -0.61185237 -0.96148230 -1.3111122 -0.3146462  0.1348484
#> 5: 2025-01-11 -0.96148230 -1.31111223 -0.4370374  0.1348484  1.3335007
#>          X2_3        Y_1        Y_2        Y_3
#>         <num>      <num>      <num>      <num>
#> 1:  0.7341745  1.0957136 -0.8098753  0.1429192
#> 2: -0.3146462 -0.8098753  0.1429192 -1.1274735
#> 3:  0.1348484  0.1429192 -1.1274735 -0.3334781
#> 4:  1.3335007 -1.1274735 -0.3334781 -0.6510762
#> 5:  1.4833322 -0.3334781 -0.6510762  1.4133118
```

**Step 2: Evaluate the SAME parsed pipe on test data**

The key insight: we use `parsed_pipe_train` (which contains scaling
parameters from training) on test data:

``` r
# Evaluate the SAME parsed pipe on test data
# This applies the scaling parameters learned from TRAINING data
test_result <- eval_single_pipe(parsed_pipe_train,
    env = list(data = test_obs),
    enclos = environment())

cat("\nTest data after applying TRAINING scaling and slicing:\n")
#> 
#> Test data after applying TRAINING scaling and slicing:
print(head(test_result, 5))
#>         index       X1_1       X1_2       X1_3       X2_1       X2_2       X2_3
#>        <Date>      <num>      <num>      <num>      <num>      <num>      <num>
#> 1: 2025-01-12         NA         NA 0.78666734         NA         NA -0.4644778
#> 2: 2025-01-13         NA 0.78666734 0.08740748         NA -0.4644778 -0.1648147
#> 3: 2025-01-14 0.78666734 0.08740748 0.26222245 -0.4644778 -0.1648147  0.4345114
#> 4: 2025-01-15 0.08740748 0.26222245 1.83555712 -0.1648147  0.4345114 -0.6143093
#> 5: 2025-01-16 0.26222245 1.83555712 0.61185237  0.4345114 -0.6143093  0.2846799
#>           Y_1        Y_2        Y_3
#>         <num>      <num>      <num>
#> 1:         NA         NA  0.9369146
#> 2:         NA  0.9369146 -0.9686744
#> 3:  0.9369146 -0.9686744 -0.4922771
#> 4: -0.9686744 -0.4922771  0.4605173
#> 5: -0.4922771  0.4605173  0.6193164
print(tail(test_result, 5))
#>         index       X1_1       X1_2       X1_3       X2_1       X2_2
#>        <Date>      <num>      <num>      <num>      <num>      <num>
#> 1: 2025-01-17  1.8355571  0.6118524 -0.2622224 -0.6143093  0.2846799
#> 2: 2025-01-18  0.6118524 -0.2622224  1.3111122  0.2846799  1.0338376
#> 3: 2025-01-19 -0.2622224  1.3111122 -1.4859272  1.0338376  0.8840061
#> 4: 2025-01-20  1.3111122 -1.4859272  1.4859272  0.8840061  1.1836691
#> 5: 2025-01-21 -1.4859272  1.4859272 -1.1362973  1.1836691 -1.3634670
#>           X2_3        Y_1        Y_2         Y_3
#>          <num>      <num>      <num>       <num>
#> 1:  1.03383759  0.4605173  0.6193164 -1.60387070
#> 2:  0.88400605  0.6193164 -1.6038707  1.57211089
#> 3:  1.18366912 -1.6038707  1.5721109  0.77811549
#> 4: -1.36346696  1.5721109  0.7781155 -0.17467899
#> 5: -0.01498315  0.7781155 -0.1746790 -0.01587991
```

This workflow demonstrates the core value proposition of the pipe
system: **transformation consistency**. By parsing pipes on training
data and reusing them on test/inference data, you ensure that all
transformations use the same parameters, which is critical for model
performance and avoiding data leakage.
