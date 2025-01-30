
# SLICING UTILS ------------------------------------------------------------------------------------

do_single_slice <- function(data, current_index, slice_on, variables, L, names) {
    lst <- extract_lagleads(data, current_index, slice_on, variables, L)
    names(lst) <- names

    new_slice_artifact(lst, current_index, L)
}

extract_lagleads <- function(data, current_index, slice_on, variables, L) {

    time_indexes <- lapply(L, function(l) current_index + l)
    extracted <- mapply(variables, time_indexes, FUN = function(v, t) {
        rows <- match(t, data[[slice_on]])
        list(data[rows][[v]])
    }, SIMPLIFY = FALSE)

    return(extracted)
}

# PARSE UTILS --------------------------------------------------------------------------------------

check_index_column <- function(data, walk_on) {
    if (!(walk_on %in% colnames(data))) stop("Column 'walk_on' not found in 'data'")
}

parse_variables <- function(data, walk_on, key_by, variables) {
    if (!missing("variables")) {
        if (!(all(variables %in% colnames(data)))) {
            stop("Some columns in 'variables' not found in 'data'")
        }
    } else {
        variables <- colnames(data)
        variables <- variables[!(variables %in% c(walk_on, key_by))]
    }

    return(variables)
}

guess_sample_freq <- function(data, column) {

    freq <- max(diff(data[[column]]))

    time_type <- class(data[1][[column]])[1]
    unit <- switch(time_type,
        "Date" = "days",
        "POSIXlt" = "secs",
        "POSIXct" = "secs")

    freq <- as.numeric(freq, units = unit)
    attr(freq, "unit") <- unit

    return(freq)
}

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

    return(L)
}

parse_start_time <- function(data, walk_on, start) UseMethod("parse_start_time", start)

parse_start_time.default <- function(data, walk_on, start) return(start)

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

parse_start_time.numeric <- function(data, walk_on, start) {
    sample_freq <- guess_sample_freq(data, walk_on)
    start <- data[[walk_on]][1] + as.numeric(sample_freq) * (start - 1)
    return(start)
}

parse_step_time <- function(data, walk_on, step) UseMethod("parse_step_time", step)

parse_step_time.character <- function(data, walk_on, step) {
    sample_freq <- guess_sample_freq(data, walk_on)
    step  <- strsplit(step, " ")[[1]]
    units <- c("secs", "mins", "hours", "days", "weeks")
    resol <- units[grep(step[2], units)]
    tdiff <- as.difftime(as.numeric(step[1]), units = resol)
    step <- as.numeric(tdiff, units = attr(sample_freq, "unit"))

    return(step)
}

parse_step_time.numeric <- function(data, walk_on, step) {
    sample_freq <- guess_sample_freq(data, walk_on)
    step <- as.numeric(sample_freq) * step
    return(step)
}

parse_slice_times <- function(data, walk_on, start, step) {
    step  <- parse_step_time(data, walk_on, step)
    start <- parse_start_time(data, walk_on, start)

    out <- seq(start, tail(data[[walk_on]], 1), by = step)

    return(out)
}
