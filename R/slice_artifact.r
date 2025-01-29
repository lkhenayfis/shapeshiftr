
#' Slice Artifact Inner Constructor
#' 
#' Builds an object of class \code{slice_artifact}

new_slice_artifact <- function(list, index, L) {
    class(list) <- c("slice_artifact", "list")
    attr(list, "index") <- index
    attr(list, "L") <- L

    return(list)
}

# METHODS ------------------------------------------------------------------------------------------

#' Concatenates \code{slice_artifact} Objects
#' 
#' Internal function; concatenates multiple objectes of this class into one \code{slice_artifact}
#' 
#' This function is not intended to be directly called by the user. It is only deployed in specific
#' conditions within the package and does not have all the necessary sanity checks for external use.
#' 
#' Mainly \code{c.slice_artifact} is intended to combine individual artifacts returned by 
#' \code{do_single_slice}, which are artifacts concerning a single time index, into the complete
#' one for the whole original data
#' 
#' @export

c.slice_artifact <- function(...) {
    n <- ...length()
    if (n > 2) {
        c(..1, do.call(c, list(...)[-1]))
    } else {
        c_two_slices(..1, ..2)
    }
}

match_slice_attr <- function(s1, s2) {
    attr1 <- attributes(s1)[c("L")]
    attr2 <- attributes(s2)[c("L")]

    same_attrs <- identical(attr1, attr2)
    same_names <- identical(names(s1), names(s2))

    same_attrs && same_names
}

c_two_slices <- function(s1, s2) {
    if (!match_slice_attr(s1, s2)) {
        stop("Slices are not eligible for concatenation -- maybe ?merge.slice_atifact")
    }

    list <- mapply("c", s1, s2, SIMPLIFY = FALSE)
    indexes <- c(attr(s1, "index"), attr(s2, "index"))

    new_slice_artifact(list, indexes, attr(s1, "L"))
}