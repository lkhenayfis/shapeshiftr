
.onLoad <- function(libname, pkgname) {
    threads <- as.numeric(Sys.getenv("SHAPESHIFTR_THREADS", 0))
    parse_threads(threads)
}

parse_threads <- function(threads) {
    has_parallel <- requireNamespace("parallel", quietly = TRUE)
    is_multi <- threads > 1
    is_linux <- Sys.info()["sysname"] == "Linux"

    if (!has_parallel) {
        stop("Package 'parallel' is not installed -- parallel >= 4.3 is required for parallel execution")
    }
    if (!is_multi) return(structure(list(), post_hook = function(cl) NULL))
    if (!is_linux) stop("Multithreading is only supported on Linux platforms")

    cl <- parallel::makeCluster(threads, "FORK")
    attr(cl, "post_hook") <- function(cl) parallel::stopCluster(cl)

    assign(".SHAPESHIFTR_CLUSTER", cl, asNamespace("shapeshiftr"))
}

get_cluster <- function() get(".SHAPESHIFTR_CLUSTER", envir = asNamespace("shapeshiftr"))