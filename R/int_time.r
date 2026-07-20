# INT_TIME -----------------------------------------------------------------------------------------

int_time <- function(x, ...) UseMethod("int_time")

int_time.ts <- function(x, ...) {
    times <- as.numeric(time(x))
    freq <- frequency(x)
    start <- start(x)

    int_time(times, freq)
}

int_time.numeric <- function(x, frequency) {
    if (frequency == 1) return(new_int_time(x, frequency, 0, x))

    seasons <- (x %% 1) * frequency + 1
    extra <- ceiling(log10(frequency))

    part1 <- floor(x) * 10^extra

    new_int_time(part1 + seasons, frequency, extra, x)
}

new_int_time <- function(x, frequency, extra, inner_times) {
    structure(x,
        frequency = frequency, extra = extra, inner_times = inner_times,
        class = c("int_time", "numeric"))
}

# METODOS ------------------------------------------------------------------------------------------

#' @export

`[.int_time` <- function(x, i, ...) {
    out1 <- unclass(x)[i]
    out2 <- attr(x, "inner_times")[i]
    new_int_time(out1, attr(x, "freq"), attr(x, "extra"), out2)
}

#' @export

`[[.int_time` <- function(x, i, ...) `[.int_time`(x, i, ...)

#' @export

`+.int_time` <- function(e1, e2) {
    aux <- attr(e1, "inner_times") + e2 / attr(e1, "frequency")
    int_time(aux, frequency = attr(e1, "frequency"))
}

#' @export

seq.int_time <- function(from, to, by, length.out, ...) {
    by <- by / attr(from, "frequency")

    time_from <- decode_int_time(from)

    if (exists("to")) {
        time_to <- decode_int_time(to)
        aux <- seq(time_from, time_to, by)
    } else if (exists("length.out")) {
        aux <- seq(time_from, by = by, length.out = length.out)
    }

    int_time(aux, attr(from, "frequency"))
}

decode_int_time <- function(x) {
    freq  <- attr(x, "frequency")
    extra <- 10^attr(x, "extra")

    if (freq == 1) return(as.numeric(x))

    part1 <- floor(as.numeric(x) / extra)
    part2 <- (as.numeric(x) %% extra - 1) / freq

    return(part1 + part2)
}
