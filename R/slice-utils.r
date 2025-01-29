
# SLICING UTILS ------------------------------------------------------------------------------------

do_single_slice <- function(data, index_by, current_index, variables, L) {
    out <- extract_lagleads(data, current_index, delta_on, variables, L)
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

parse_start_time <- function(data, L, start) {

    max_lag  <- abs(min(unlist(L)))
    max_lead <- abs(max(unlist(L)))
    if (start < (max_lag + 1)) {
        start <- max_lag + 1
        warning("'start' is less than the maximum lags -- forcing to maximum lag + 1")
    } else if (start > (nrow(data) - max_lead)) {
        start <- nrow(data) - max_lead
        warning("'start' is greater than the (nrow(data) - max lead) -- forcing to nrow(data) - max lead")
    }

    return(start)
}

guess_sample_freq <- function(data, delta_on) {
    freq <- diff(head(data[[delta_on]], 2))

    time_type <- class(data[1][[delta_on]])[1]
    unit <- switch(time_type,
        "Date" = "days",
        "POSIXlt" = "secs",
        "POSIXct" = "secs")

    freq <- as.numeric(freq, units = unit)

    return(freq)
}