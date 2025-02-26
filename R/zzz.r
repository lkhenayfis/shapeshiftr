
generate_msg <- function(threads) {
    if (threads > 1) {
        msg <- paste0("'shapeshifter' is running multithreated with ", threads, " threads")
    } else {
        msg <- paste0("'shapeshifter' is running singlethreaded")
    }

    msg <- paste0(msg, " -- See ?shapeshiftr_threads for more details")
    return(msg)
}

.onLoad <- function(libname, pkgname) {
    threads <- as.numeric(Sys.getenv("SHAPESHIFTR_THREADS", 0))
    set_up_cluster(threads)
}

.onAttach <- function(libname, pkgname) {
    threads <- as.numeric(Sys.getenv("SHAPESHIFTR_THREADS", 0))
    msg <- generate_msg(threads)
    packageStartupMessage(msg)
}

.onUnload <- function(libname, pkgname) {
    cl <- get_cluster()
    run_post_hook(cl)
}
