
# SLICING UTILS ------------------------------------------------------------------------------------

extract_lags <- function(data, target, regressors, L, center) {
    vars <- c(target, regressors)
    out <- lapply(seq_along(vars), function(i) {
        L_i <- L[[i]]
        var <- vars[i]
        list(data[center - L_i][[var]])
    })
    names(out) <- paste0("lagged_", vars)

    return(out)
}

extract_leads <- function(data, target, H, center) {
    out <- list(list(data[center + H][[target]]))
    names(out) <- paste0("lead_", target)

    return(out)
}

do_single_slice <- function(data, index_by, target, regressors, L, H, center) {
    lags  <- extract_lags(data, target, regressors, L, center)
    leads <- extract_leads(data, target, H, center)

    out <- c(lags, leads)

    return(out)
}

# PARSE UTILS --------------------------------------------------------------------------------------

check_column_names <- function(data, index_by, target, regressors) {
    if (!(index_by %in% colnames(data))) stop("Column 'index_by' not found in 'data'")
    if (!(target %in% colnames(data))) stop("Column 'target' not found in 'data'")
    if (!(all(regressors %in% colnames(data)))) stop("Some 'regressors' not found in 'data'")
}

parse_lag_times <- function(L, num_regressors) {

    L_is_integer <- length(L) == 1 & !is.list(L)
    L_is_vector  <- length(L) > 1 & !is.list(L)
    L_is_list    <- is.list(L)

    if (L_is_integer) {
        L <- lapply(seq(num_regressors + 1), function(x) seq(L))
    } else if (L_is_vector) {
        L <- lapply(seq(num_regressors + 1), function(x) L)
    } else if (L_is_list) {
        if (length(L) != (num_regressors + 1)) stop("'L' is a list but of wrong size")
    } else {
        stop("'L' is of unsopported format")
    }

    L <- lapply(L, function(l) sort(l, TRUE))

    return(L)
}

parse_lead_times <- function(H) {
    H_is_integer <- length(H) == 1
    H_is_vector  <- length(H) > 1

    if (H_is_integer) {
        H <- seq(H)
    }else {
        stop("'H' is of unsupported format")
    }

    return(H)
}

check_zero_lead_lag <- function(L, H) {
    if ((0 %in% unlist(H)) && (0 %in% unlist(L))) {
        warning("Both 'L' and 'H' contain 0 -- this is likely a mistake")
    }
}

recenter_lag_lead_indexes <- function(L, H) {
    center <- max(unlist(L)) + 1

    L <- lapply(L, function(l) center - l)
    H <- center + H

    out <- list(L, H)

    return(out)
}

parse_start_time <- function(start, L) {

    if (start < (max(unlist(L)) + 1)) {
        start <- max(unlist(L)) + 1
        warning("'start' is less than the maximum number of lags -- forcing to maximum lag + 1")
    }

    return(start)
}