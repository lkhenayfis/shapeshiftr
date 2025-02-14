
inner_run <- function(cl, X, fun) UseMethod("inner_run")

inner_run.default <- function(cl, X, fun) lapply(X, fun)

inner_run.cluster <- function(cl, X, fun) parallel::parLapply(cl, X, fun)

run_post_hook <- function(cl) {
    hook <- attr(cl, "post_hook")
    hook(cl)
}
