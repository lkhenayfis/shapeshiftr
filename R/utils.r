
get_cluster <- function() get(".SHAPESHIFTR_CLUSTER", envir = asNamespace("shapeshiftr"))

refresh_cluster <- function(cl, envir) UseMethod("refresh_cluster")

refresh_cluster.default <- function(cl, envir) invisible(NULL)

refresh_cluster.cluster <- function(cl, envir) {
    vars <- ls(envir = envir)
    parallel::clusterExport(cl, vars, envir)
}

run_loop <- function(X, fun) {
    cl <- get_cluster()
    refresh_cluster(cl, parent.frame())
    inner_run(cl, X, fun)
}

inner_run <- function(cl, X, fun) UseMethod("inner_run")

inner_run.default <- function(cl, X, fun) lapply(X, fun)

inner_run.cluster <- function(cl, X, fun) parallel::parLapply(cl, X, fun)

run_post_hook <- function(cl) {
    hook <- attr(cl, "post_hook")
    hook(cl)
}
