
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
#' @param function to be applied
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
#' @param function to be applied
#' 
#' @return list with results of applying \code{fun} to each element of \code{X}

inner_run <- function(cl, X, fun) UseMethod("inner_run")

inner_run.default <- function(cl, X, fun) lapply(X, fun)

inner_run.cluster <- function(cl, X, fun) parallel::parLapply(cl, X, fun)
