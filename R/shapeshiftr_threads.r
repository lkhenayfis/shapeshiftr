#' Multithreading Functionality In \code{shapeshiftr}
#' 
#' How to set up and configure \code{shapeshiftr} to run multithreaded
#' 
#' Some functions in \code{shapeshiftr} can take a long time to run, especially in very large 
#' datasets. This is usually the case of \code{slicing}, which must be executed as a serial loop
#' over the lines of a \code{data.frame}-like object. To overcome the high computational cost, this
#' package offers multithreading functionality in a universal way, that is, multithreading is either
#' active or not whithout any fine grain control.
#' 
#' By default every function will be run singlethreaded. To enable multithreading, first of all, two
#' conditions must be met:
#' 
#' \itemize{
#' \item{multithreading is only allowed in Linux based systems}
#' \item{package \code{parallel} must be available in version >= 4.3}
#' }
#' 
#' Should these conditions be met, when \code{shapeshiftr} is loaded it will look for an environment
#' variable named \code{"SHAPESHIFTR_THREADS"}. If it exists and is an integer larger than 1, a
#' cluster will be initiated and reserved within the package's namespace. After this, every single
#' function in the package that loops over data will use the cluster created; if no cluster exists
#' they simply run singlethreaded as usual.
#' 
#' Note that the environment variable \code{"SHAPESHIFTR_THREADS"} must exist WHEN THE PACKAGE IS
#' LOADED. Thus, to configure multithreading, it is recommended the user define this varaible in a
#' \code{.Renviron} file in the projects root. The second option, which works but is not
#' recommended, is to set the environment variable with \code{\link[base]{Sys.setenv}}.
#' 
#' @name shapeshiftr_threads

NULL