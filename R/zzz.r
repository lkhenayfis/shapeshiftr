
.onLoad <- function(libname, pkgname) {
    threads <- as.numeric(Sys.getenv("SHAPESHIFTR_THREADS", 0))
    set_up_cluster(threads)

    msg <- generate_msg(threads)
    packageStartupMessage(msg)
}

.onUnload <- function(libname, pkgname) {
    cl <- get_cluster()
    run_post_hook(cl)
}

set_up_cluster <- function(threads) {
    has_parallel <- requireNamespace("parallel", quietly = TRUE)
    is_multi <- threads > 1
    is_linux <- Sys.info()["sysname"] == "Linux"

    if (!has_parallel) {
        stop("Package 'parallel' is not installed -- parallel >= 4.3 is required for parallel execution")
    }
    if (!is_linux) stop("Multithreading is only supported on Linux platforms")

    if (!is_multi) {
        cl <- structure(list(), post_hook = function(cl) NULL)
    } else {
        cl <- parallel::makeCluster(threads, "FORK")
        attr(cl, "post_hook") <- function(cl) parallel::stopCluster(cl)
    }

    assign(".SHAPESHIFTR_CLUSTER", cl, asNamespace("shapeshiftr"))
}

generate_msg <- function(threads) {
    if (threads > 1) {
        msg <- paste0("'shapeshifter' is running multithreated with ", threads, " threads")
    } else {
        msg <- paste0("'shapeshifter' is running singlethreaded")
    }

    msg <- paste0(msg, " -- See ?shapeshiftr_threads for more details")
    return(msg)
}