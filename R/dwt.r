
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

format_ws <- function(wt) {
    ws <- wt@W
    names <- lapply(names(ws), function(s) paste0(s, "_", seq_len(nrow(ws[[s]]))))
    ws <- lapply(ws, c)
    ws <- do.call(c, ws)
    names(ws) <- do.call(c, names)
    return(ws)
}

format_vs <- function(wt) {
    vs <- wt@V
    names <- lapply(names(vs), function(s) paste0(s, "_", seq_len(nrow(vs[[s]]))))
    vs <- lapply(vs, c)
    vs <- do.call(c, vs)
    names(vs) <- do.call(c, names)
    return(vs)
}

build_wavelets_single_variable <- function(l, ...) {
    l_wt <- lapply(l, function(v) dwt(as.numeric(v), ...))
    ws <- lapply(l_wt, format_ws)
    vs <- lapply(l_wt, format_vs)
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
