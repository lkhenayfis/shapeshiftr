#' Example Datasets
#' 
#' Sample \code{data.table}s for testing and example code in this package
#' 
#' There are four datasets included:
#' 
#' \itemize{
#' \item simple_dt_date
#' \item simple_dt_datetime
#' \item keyed_dt_date
#' \item keyed_dt_datetime
#' }
#' 
#' In the following, all values are merely illustrative and have no bearing on the actual datasets.
#' 
#' The ones prefixed \code{simple} have a single column of time index, which can be of either type
#' \code{Date} or \code{POSIXct}, as indicaded by the suffixes. \code{simple_dt_date} looks like
#' 
#' |      date  | X1  |  X2  | X3  |   Y |
#' |      ---   | --- |  --- | --- | --- |
#' | 2025-01-02 | 16  | -18  | 66  | 132 |
#' | 2025-01-03 |  5  | -17  | 55  | 122 |
#' | 2025-01-04 | 12  | -16  | 68  | 138 |
#' | 2025-01-05 | 15  | -19  | 69  | 137 |
#' | 2025-01-06 |  9  |  -7  | 70  | 125 |
#' 
#' \code{simple_dt_datetime} is similar, except instead of column \code{date}, there is a column
#' named \code{datetime} of hour by hour times.
#' 
#' Keyed datasets are very similar, except for having an aditional time index column. For example,
#' \code{keyed_dt_date} looks like
#' 
#' |      date  | target_date | X1  |  X2  | X3  |   Y |
#' |      ---   | ---         | --- |  --- | --- | --- |
#' | 2025-01-02 | 2025-01-03  | 16  | -18  | 66  | 132 |
#' | 2025-01-02 | 2025-01-04  |  5  | -17  | 55  | 122 |
#' | 2025-01-02 | 2025-01-05  | 12  | -16  | 68  | 138 |
#' | 2025-01-02 | 2025-01-06  | 15  | -19  | 69  | 137 |
#' | 2025-01-02 | 2025-01-07  |  9  |  -7  | 70  | 125 |
#' 
#' With \code{keyed_dt_datetime} being analogous.
#' 
#' @name example_datasets

#' @rdname example_datasets
"simple_dt_date"

#' @rdname example_datasets
"simple_dt_datetime"

#' @rdname example_datasets
"keyed_dt_date"

#' @rdname example_datasets
"keyed_dt_datetime"