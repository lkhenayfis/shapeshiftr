
# SLICING UTILS ------------------------------------------------------------------------------------

do_single_slice <- function(data, current_index, delta_on, variables, L, names) {
    out <- extract_lagleads(data, current_index, delta_on, variables, L)
    names(out) <- names
    return(out)
}

extract_lagleads <- function(data, current_index, delta_on, variables, L) {

    time_indexes <- lapply(L, function(l) current_index + l)
    extracted <- mapply(variables, time_indexes, FUN = function(v, t) {
        rows <- match(t, data[[delta_on]])
        data[rows][[v]]
    }, SIMPLIFY = FALSE)

    return(extracted)
}

# PARSE UTILS --------------------------------------------------------------------------------------

check_index_column <- function(data, index_by) {
    if (!(index_by %in% colnames(data))) stop("Column 'index_by' not found in 'data'")
}

parse_variables <- function(data, index_by, key_by, variables) {
    if (!missing("variables")) {
        if (!(all(variables %in% colnames(data)))) {
            stop("Some columns in 'variables' not found in 'data'")
        }
    } else {
        variables <- colnames(data)
        variables <- variables[!(variables %in% c(index_by, key_by))]
    }

    return(variables)
}

parse_laglead_times <- function(L, variables, sample_freq) {

    M <- length(variables)
    L_is_list <- is.list(L)

    if (L_is_list) {
        if (length(L) != M) stop("'L' is a list but has less elements than 'variables'")
    } else {
        L <- lapply(seq_len(M), function(i) L)
    }

    L <- lapply(L, function(l) l * sample_freq)

    return(L)
}

parse_start_time <- function(data, index_by, start, sample_freq) UseMethod("parse_start_time", start)

parse_start_time.default <- function(data, index_by, start, sample_freq) return(start)

parse_start_time.numeric <- function(data, index_by, start, sample_freq) {
    start <- data[[index_by]][1] + sample_freq * (start - 1)
    return(start)
}

parse_step_time <- function(data, index_by, step, sample_freq) UseMethod("parse_step_time", step)

parse_step_time.character <- function(data, index_by, step, sample_freq) {
    step  <- strsplit(step, " ")[[1]]
    units <- c("secs", "mins", "hours", "days", "weeks")
    resol <- units[grep(step[2], units)]
    tdiff <- as.difftime(as.numeric(step[1]), units = resol)
    step <- as.numeric(tdiff, units = attr(sample_freq, "unit"))

    return(step)
}

parse_step_time.numeric <- function(data, index_by, step, sample_freq) {
    step <- sample_freq * step
    return(step)
}

parse_slice_times <- function(data, index_by, start, step, sample_freq) {
    step  <- parse_step_time(data, index_by, step, sample_freq)
    start <- parse_start_time(data, index_by, start, sample_freq)

    out <- seq(start, tail(data[[index_by]], 1), by = step)

    return(out)
}

guess_sample_freq <- function(data, delta_on) {
    freq <- diff(head(data[[delta_on]], 2))

    time_type <- class(data[1][[delta_on]])[1]
    unit <- switch(time_type,
        "Date" = "days",
        "POSIXlt" = "secs",
        "POSIXct" = "secs")

    freq <- as.numeric(freq, units = unit)
    attr(freq, "unit") <- unit

    return(freq)
}