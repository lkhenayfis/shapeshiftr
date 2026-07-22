
# SLICE PARAMETERS ---------------------------------------------------------------------------------

#' @keywords internal

parse_slice_args <- function(data, variables, walk_on, slice_on, L, start, step, names) {

    walk_on <- parse_index_column(data, walk_on)
    slice_on <- parse_index_column(data, slice_on)
    variables <- parse_variables(data, c(walk_on, slice_on), variables)
    names <- parse_names(names, variables)
    L <- parse_laglead_times(data, slice_on, L, variables)
    slice_times <- parse_slice_times(data, walk_on, start, step)

    params <- new_slice_parameters(variables, walk_on, slice_on, L, slice_times, names)

    return(params)
}

#' @keywords internal

new_slice_parameters <- function(variables, walk_on, slice_on, L, slice_times, names) {
    new <- list(
        variables = variables,
        walk_on = walk_on,
        slice_on = slice_on,
        L = L,
        slice_times = slice_times,
        names = names
    )

    new_class <- ifelse(walk_on == slice_on, "simple_slice_params", "keyed_slice_params")
    class(new) <- c(new_class, "list")

    return(new)
}

# PARSE UTILS --------------------------------------------------------------------------------------

#' @keywords internal

parse_index_column <- function(data, col) {
    if (is.null(col)) col <- guess_index_col(data)
    if (!(col %in% colnames(data))) stop(sprintf("Column '%s' not found in 'data'", col))
    return(col)
}

#' @keywords internal

guess_index_col <- function(data) {
    candidates <- lapply(data, inherits, what = c("Date", "POSIXct", "POSIXlt"))
    candidates <- sapply(candidates, any)
    guess <- head(which(candidates), 1)
    if (length(guess) == 0) stop("Couldn't guess 'walk_on' column -- provide this argument")
    guess <- names(data)[guess]
    return(guess)
}

#' @keywords internal

parse_variables <- function(data, index_cols, variables) {
    if (is.null(variables)) {
        variables <- setdiff(names(data), index_cols)
    } else if (!(all(variables %in% colnames(data)))) {
        stop("Some columns in 'variables' not found in 'data'")
    }
    return(variables)
}

#' @keywords internal

parse_names <- function(names, variables) {
    if (!is.null(names)) {
        if (length(names) != length(variables)) stop("'names' must have same length as 'variables'")
        return(names)
    }
    ord <- split(seq_along(variables), variables)
    names <- split(variables, variables)
    names <- lapply(names, function(x) if (length(x) > 1) paste0(x, "_", seq_len(length(x))) else x)
    names <- unname(unlist(names))
    names <- names[order(unlist(ord))]
    return(names)
}

#' @keywords internal

guess_sample_freq <- function(data, column) {

    freq <- diff(unique(data[[column]]))
    freq <- freq[(freq > 0)]
    freq <- min(freq)

    time_type <- class(data[1][[column]])[1]
    unit <- switch(time_type,
        "Date" = "days",
        "POSIXlt" = "secs",
        "POSIXct" = "secs")

    freq <- as.numeric(freq, units = unit)

    if (time_type == "Date") {
        if (freq >= 28 && freq <= 31) {
            unit <- "months"
            freq <- 1
        } else if (freq >= 365 && freq <= 366) {
            unit <- "years"
            freq <- 1
        }
    }

    attr(freq, "unit") <- unit

    return(freq)
}

#' @keywords internal

parse_laglead_times <- function(data, slice_on, L, variables) {

    sample_freq <- guess_sample_freq(data, slice_on)

    M <- length(variables)
    L_is_list <- is.list(L)

    if (L_is_list) {
        if (length(L) != M) stop("'L' is a list but has less elements than 'variables'")
    } else {
        L <- lapply(seq_len(M), function(i) L)
    }

    L <- lapply(L, function(l) l * as.numeric(sample_freq))

    if (is.null(names(L))) names(L) <- variables else L <- L[variables]

    return(L)
}

#' @keywords internal

parse_start_time <- function(data, walk_on, start) UseMethod("parse_start_time", start)

#' @keywords internal

parse_start_time.default <- function(data, walk_on, start) return(start)

#' @keywords internal

parse_start_time.character <- function(data, walk_on, start) {
    example <- data[[walk_on]][1]
    class_walk_on <- class(example)
    if (class_walk_on[1] == "Date") {
        start <- as.Date(start)
    } else if (class_walk_on[1] == "POSIXct") {
        start <- as.POSIXct(start, tz = attr(example, "tzone"))
    }

    return(start)
}

#' @keywords internal

parse_start_time.numeric <- function(data, walk_on, start) {
    sample_freq <- guess_sample_freq(data, walk_on)
    start <- data[[walk_on]][1] + as.numeric(sample_freq) * (start - 1)
    return(start)
}

#' @keywords internal

parse_step_time <- function(data, walk_on, step) UseMethod("parse_step_time", step)

#' @keywords internal

parse_step_time.character <- function(data, walk_on, step) {
    sample_freq <- guess_sample_freq(data, walk_on)
    step  <- strsplit(step, " ")[[1]]
    units <- c("secs", "mins", "hours", "days", "weeks")
    resol <- units[grep(step[2], units)]
    tdiff <- as.difftime(as.numeric(step[1]), units = resol)
    step <- as.numeric(tdiff, units = attr(sample_freq, "unit"))

    return(step)
}

#' @keywords internal

parse_step_time.numeric <- function(data, walk_on, step) {
    sample_freq <- guess_sample_freq(data, walk_on)
    step <- as.numeric(sample_freq) * step
    return(step)
}

#' @keywords internal

parse_slice_times <- function(data, walk_on, start, step) {
    step  <- parse_step_time(data, walk_on, step)
    start <- parse_start_time(data, walk_on, start)

    out <- seq(start, tail(data[[walk_on]], 1), by = step)

    return(out)
}
