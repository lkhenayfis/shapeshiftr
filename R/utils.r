
#' Configures Internal Package Cluster
#' 
#' Sets up a FORK cluster of \code{threads} threads, if running on linux platforms
#' 
#' This is an internal function, should not be called directly by the user
#' 
#' @param threads number of cluster threads
#' 
#' @return has no explicit return. Assings a FORK cluster to name \code{.SHAPESHIFTR_CLUSTER} in the
#'     package's namespace, if code{threads > 1}, or a NULL object otherwise

set_up_cluster <- function(threads) {
    has_parallel <- requireNamespace("parallel", quietly = TRUE)
    is_multi <- threads > 1
    is_linux <- Sys.info()["sysname"] == "Linux"

    if (!has_parallel) {
        stop("Package 'parallel' is not installed -- parallel >= 4.3 is required for parallel execution")
    }
    if (is_multi && !is_linux) stop("Multithreading is only supported on Linux platforms")

    if (!is_multi) {
        cl <- structure(list(), post_hook = function(cl) NULL)
    } else {
        cl <- parallel::makeCluster(threads, "FORK")
        attr(cl, "post_hook") <- function(cl) parallel::stopCluster(cl)
    }

    assign(".SHAPESHIFTR_CLUSTER", cl, asNamespace("shapeshiftr"))
}

#' Treatment Of Cluster On Package Unload
#' 
#' If \code{cl} is a cluster, stops it
#' 
#' @param cl cluster or NULL
#' 
#' @return no explicit return, only stops \code{cl} if it is a cluster, or does nothing otherwise

run_post_hook <- function(cl) {
    hook <- attr(cl, "post_hook")
    hook(cl)
}

#' Gets Internal \code{shapeshiftr} Cluster
#' 
#' Internal function for accessing package dedicated cluster
#' 
#' @return object of type \code{cluster}

get_cluster <- function() get(".SHAPESHIFTR_CLUSTER", envir = asNamespace("shapeshiftr"))

#' Updates Cluster With Contextual Information
#' 
#' Exports names in current calling environment to cluster \code{cl}
#' 
#' Given that the cluster is initialized when the package is loaded, it needs to receive, at each
#' parallel function execution, the objects necessary for computation. \code{refresh_cluster} looks
#' for all names in an environment \code{envir} and exports them to cluster \code{cl}
#' 
#' Since this is an internal function, it is not meant to be directly called by the user. Normally
#' it is seen in functions as \code{refresh_cluster(cl, parent.frame())}, i.e., refreshing with the
#' calling environment.
#' 
#' @param cl an object of type \code{cluster}
#' @param envir environment from which to export names
#' 
#' @return nothing, only has side effect of exporting names in \code{envir} to \code{cl}

refresh_cluster <- function(cl, envir) UseMethod("refresh_cluster")

#' @rdname refresh_cluster

refresh_cluster.default <- function(cl, envir) invisible(NULL)

#' @rdname refresh_cluster

refresh_cluster.cluster <- function(cl, envir) {
    vars <- ls(envir = envir)
    parallel::clusterExport(cl, vars, envir)
}

#' Internal Loop Executor
#' 
#' Simple wrapper to run loops, either single or multithreaded
#' 
#' @param X iterable collection
#' @param fun to be applied
#' 
#' @return list with results of applying \code{fun} to each element of \code{X}

run_loop <- function(X, fun) {
    cl <- get_cluster()
    refresh_cluster(cl, parent.frame())
    inner_run(cl, X, fun)
}

#' Generic Loop Execution
#' 
#' Simple generic with methods for either single or multithreaded
#' 
#' @param cl either an object of type \code{cluster} or NULL
#' @param X iterable collection
#' @param fun to be applied
#' 
#' @return list with results of applying \code{fun} to each element of \code{X}

inner_run <- function(cl, X, fun) UseMethod("inner_run")

#' @rdname inner_run

inner_run.default <- function(cl, X, fun) lapply(X, fun)

#' @rdname inner_run

inner_run.cluster <- function(cl, X, fun) parallel::parLapply(cl, X, fun)
