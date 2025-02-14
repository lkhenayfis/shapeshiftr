
#' Discrete Wavelet Transform Method For \code{slice_artifact} Objects
#' 
#' Extends the \code{\link[wavelets]{dwt}} function to operate on \code{slice_artifact}
#' 
#' @param X a \code{slice_artifact} object
#' @param variables variables on which to perform the discrete wavelet transform
#' @param ... remaining arguments of \code{\link[wavelets]{dwt}}
#' 
#' @import wavelets
#' 
#' @rdname dwt
#' 
#' @export

dwt.slice_artifact <- function(X, variables, ...) {
    if (missing("variables")) variables <- names(X)

    X <- na.exclude(X)
    wav_list <- lapply(X[variables], build_wavelets_single_variable, ...)

    new_slice_artifact(wav_list, attr(X, "index"), NA)
}

format_coef <- function(wt, slot) {
    coef <- slot(wt, slot)
    names <- lapply(names(coef), function(s) paste0(s, "_", seq_len(nrow(coef[[s]]))))
    coef <- lapply(coef, c)
    coef <- do.call(c, coef)
    names(coef) <- do.call(c, names)
    return(coef)
}

build_wavelets_single_variable <- function(l, ...) {
    l_wt <- run_loop(l, function(v) dwt(as.numeric(v), ...))
    ws <- lapply(l_wt, format_coef, "W")
    vs <- lapply(l_wt, format_coef, "V")
    wavs <- mapply(c, ws, vs, SIMPLIFY = FALSE)
    return(wavs)
}

#' @rdname dwt
#' 
#' @export

dwt.default <- function(X, ...) {
    wavelets::dwt(X, ...)
}

#' @rdname dwt
#' 
#' @export

dwt <- function(X, ...) UseMethod("dwt")
