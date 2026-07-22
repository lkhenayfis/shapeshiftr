
#' Generate Temporal Slices From Data
#' 
#' Operates on a time-indexed data.frame-like to generate appropriate regressors and target variable
#' 
#' @details
#' ## Simple vs Keyed Slicing
#' 
#' **Simple slicing** occurs when \code{walk_on == slice_on} (the default).
#' This creates a standard sliding window where the index used for centering
#' is the same as the index used for extracting lag/lead values. Use simple
#' slicing for typical autoregressive feature engineering.
#' 
#' **Keyed slicing** occurs when \code{walk_on != slice_on}. This is essential
#' for forecast datasets with dual temporal structure (e.g., forecast execution
#' date and target date). The function groups by \code{walk_on} values and
#' extracts features based on \code{slice_on} values within each group.
#' 
#' For example, the included data.table \code{keyed_dt_date}
#' 
#' |      date  | target_date | X1  |  X2  | X3  |   Y |
#' |      ---   | ---         | --- |  --- | --- | --- |
#' | 2025-01-02 | 2025-01-03  | 16  | -18  | 66  | 132 |
#' | 2025-01-02 | 2025-01-04  |  5  | -17  | 55  | 122 |
#' | 2025-01-02 | 2025-01-05  | 12  | -16  | 68  | 138 |
#' | 2025-01-02 | 2025-01-06  | 15  | -19  | 69  | 137 |
#' | 2025-01-02 | 2025-01-07  |  9  |  -7  | 70  | 125 |
#' 
#' If \code{walk_on = "date"} and \code{slice_on = "target_date"}, slices will be generated for
#' values in \code{"date"}, but the actual subset will be based on \code{target_date}.
#' 
#' ## Lag and Lead Sign Convention
#' 
#' * **Negative L values**: Lags (past values). L = -1 means "1 period ago"
#' * **Zero**: Current period. L = 0 means "same period as walk_on"
#' * **Positive L values**: Leads (future values). L = 1 means "1 period ahead"
#' 
#' The "period" unit is determined by temporal resolution of \code{walk_on}
#' (e.g., days for Date class, datetime intervals for POSIXct).
#' 
#' ## Temporal Resolution
#' 
#' Temporal resolution is automatically detected from \code{walk_on} column class:
#' * **Date**: Daily resolution
#' * **POSIXct/POSIXlt**: Datetime resolution (hours, minutes, seconds)
#' 
#' The \code{step} parameter controls stride through \code{walk_on}.
#' Can be integer (number of periods) or string ("2 hours", "1 day").
#' 
#' ## Variable Selection
#' 
#' Argument \code{variables} defines which columns in \code{data} are to be sliced. If it is empty,
#' all variables except \code{walk_on} and \code{key_by} are used. This argument also accepts
#' duplicated values. This is intended to facilitate different forms of slicing a single variable
#' in the data. One example would be if one is extracting both target future values and explanatory
#' past values for fitting an autoregressive model.
#' 
#' ## Lag Structure Specification
#' 
#' Lag and lead times, passed through argument \code{L}, are differentiated by their sign. Negative
#' values are interpreted as lags and positive ones as leads. The canonical way to pass this
#' argument is a names list indicating lags/leads for each variable. If unamed, it is assumed
#' \code{L} is in the same order as \code{variables}. If \code{L} is a single integer or vector of
#' integers, it is assumed all variables should be sliced equally.
#' 
#' @param data the data.frame-like object on which to operate
#' @param variables names of variables for extraction in slicing
#' @param walk_on name of the column to use as index for centering each slice. See Details
#' @param slice_on name of the column to use as reference for lead/lag slice search. See Details
#' @param L lag/lead times of the variables that must be extracted. See Details
#' @param start where the slicing will start. Can be an integer, in which case it is interpreted as
#'     a line number, or a date-like object, in which case it is interpreted literally
#' @param step step size to walk through \code{walk_on}. Can be an integer, in which case it is
#'     interpreted as number of intervals in the temporal resolution of \code{walk_on}, or a string
#'     of the form "2 hours". See \code{\link{difftime}} for which time units are available
#' @param names naming for each sliced variable; by default this is the same as \code{variables} or,
#'     if there are duplicates, appends \code{_X} where X is an increasing integer
#' @param ... sem funcao, apenas para consistencia entre metodos
#' 
#' @seealso 
#' * \code{\link{new_slice_artifact}} for details on the returned slice_artifact object structure
#' * \code{\link{merge.slice_artifact}} for combining multiple slice artifacts
#' * \code{\link{as.data.table.slice_artifact}} for converting to wide/long format
#' 
#' @return An \code{slice_artifact} object with the results, see \code{\link{new_slice_artifact}}
#'     for more details
#' 
#' @examples
#' # Example 1: Simple lag features for autoregressive forecasting ----
#' # Create lag features to predict tomorrow's Y using past Y values
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Create 1, 2, 3-day lags of Y for autoregressive model
#' # L = c(-1, -2, -3): negative values indicate lags (past values)
#' y_lags <- slice(
#'     data = simple_dt_date,
#'     variables = "Y",
#'     walk_on = "date",
#'     L = c(-1, -2, -3)
#' )
#' 
#' # Inspect structure: slice_artifact contains list of lag values
#' print(y_lags)
#' str(y_lags, max.level = 2)
#' 
#' # Note: walk_on == slice_on (default) creates standard sliding window
#' # This is "simple slicing" - centering and slicing on same index
#' 
#' # Example 2: Keyed slicing for forecast data integration ----
#' # When data has forecast execution date AND target date, use keyed slicing
#' 
#' data(keyed_dt_date)
#' 
#' # keyed_dt_date structure:
#' # - date: when forecast was executed
#' # - target_date: what date the forecast targets
#' # - X1, X2, X3, Y: predictor and response variables
#' 
#' # Create lag features aligned to TARGET dates, grouped by execution date
#' # walk_on = "date": group by forecast execution date
#' # slice_on = "target_date": slice based on target dates
#' keyed_lags <- slice(
#'     data = keyed_dt_date,
#'     variables = "X1",
#'     walk_on = "date",
#'     slice_on = "target_date",
#'     L = c(-1, -2)
#' )
#' 
#' print(keyed_lags)
#' 
#' # This is "keyed slicing" - walk on one index, slice on another
#' # Critical for integrating observed data (target_date) with forecast context (date)
#' 
#' # Example 3: Multi-variable slicing with different lag structures ----
#' # Different predictors may need different lag depths
#' 
#' # Scenario: X1 volatile (1-2 day lags), X2 slow-moving (1-5 day lags)
#' multi_lags <- slice(
#'     data = simple_dt_date,
#'     variables = c("X1", "X2"),
#'     walk_on = "date",
#'     L = list(
#'         X1 = c(-1, -2),
#'         X2 = c(-1, -3, -5, -7)
#'     )
#' )
#' 
#' print(multi_lags)
#' str(multi_lags, max.level = 2)
#' 
#' # List specification of L allows flexible lag patterns
#' # Names in L must match variables
#' # Can also include lead times: L = c(-2, -1, 0, 1, 2) for centered window
#' 
#' @export

slice <- function(data, L = -1, start = 1, step = 1, ...) UseMethod("slice")

#' @rdname slice
#'
#' @export

slice.default <- function(data, L = -1, start = 1, step = 1, variables = NULL,
    walk_on = NULL, slice_on = walk_on, names = NULL, ...) {

    params <- parse_slice_args(data, variables, walk_on, slice_on, L, start, step, names)
    out <- do_slices(data, params)

    return(out)
}

#' @rdname slice
#'
#' @export

slice.ts <- function(data, L = -1, start = 1, step = 1, ...) {
    slice.default(ts2dt(data), "value", "time", L = L, start = start, step = step)
}

#' @rdname slice
#'
#' @export

slice.mts <- function(data, L = -1, start = 1, step = 1, variables = NULL, names = NULL, ...) {
    slice.default(ts2dt(data), variables, "time", L = L, start = start, step = step, names = names)
}

# SLICING INTERNALS --------------------------------------------------------------------------------

do_slices <- function(data, params) {

    # truque bem gambiarrado
    # lapply subsets por uma primitiva em C que quebra a classe e atributos especiais de um vetor,
    # como a classe int_time
    # as.list() faria a mesma coisa, entao e necessario dar essa volta para que os slice_times
    # virem uma lista de int_times que efetivamente preservam sua classe e atributos
    st <- params$slice_times
    st <- lapply(seq_along(st), function(i) st[i])
    slices <- run_loop(st, function(i) do_single_slice(data, i, params))
    slices <- Reduce(c, slices)

    return(slices)
}

#' @keywords internal

do_single_slice <- function(data, index, params) UseMethod("do_single_slice", params)

#' @keywords internal

do_single_slice.simple_slice_params <- function(data, index, params) {

    time_indexes <- lapply(params$L, function(l) index + l)
    slice <- mapply(params$variables, time_indexes, FUN = function(v, t) {
        rows <- match(t, data[[params$slice_on]])
        list(data[rows][[v]])
    }, SIMPLIFY = FALSE)

    new <- new_slice_artifact(slice, index, params$L)
    names(new) <- params$names

    return(new)
}

#' @keywords internal

do_single_slice.keyed_slice_params <- function(data, index, params) {
    do_single_slice.simple_slice_params(data[get(params$walk_on) == index], index, params)
}

# HELPERS ------------------------------------------------------------------------------------------

#' @keywords internal

ts2dt <- function(x) UseMethod("ts2dt")

#' @keywords internal

ts2dt.ts <- function(x) {
    data.table(time = int_time(x), value = as.numeric(x))
}

#' @keywords internal

ts2dt.mts <- function(x) {
    cbind(data.table(time = int_time(x)), as.matrix(x))
}
