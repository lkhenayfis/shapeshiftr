
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

#' Subsetting Of \code{slice_artifact}
#' 
#' Internal function; subsets \code{slice_artifact} either by variable or index
#' 
#' This function is not intended to be directly called by the user. It is only deployed in specific
#' conditions within the package and does not have all the necessary sanity checks for external use.
#' 
#' @param x object of class \code{slice_artifact}
#' @param i variables for subsetting; if missing returns all variables
#' @param j indexes for subsetting; if missing returns all indexes
#' 
#' @export

`[.slice_artifact` <- function(x, i, j, ...) {
    if (missing("i")) i <- names(x)
    if (missing("j")) j <- attr(x, "index")

    L <- attr(x, "L")

    x <- unclass(x)
    list_i <- x[i]

    indexes_keep <- attr(x, "index") %in% j
    list_i_j <- lapply(list_i, function(l) l[indexes_keep])

    new_slice_artifact(list_i_j, j, L)
}

#' Merges \code{slice_artifact} Objects
#' 
#' Internal function; merges objectes by common index into a single \code{slice_artifact}
#' 
#' This function is not intended to be directly called by the user. It is only deployed in specific
#' conditions within the package and does not have all the necessary sanity checks for external use.
#' 
#' Contrary to \code{c.slice_artifact}, this function is meant to combine artifacts from different
#' data, or different slicing parameters, into a unified artifact object. This will match indexes
#' in all elements for merging and ONLY USE indexes which are common to all. There is no \code{all}
#' argument like in data.frame merging.
#' 
#' @export

merge.slice_artifact <- function(x, y, ...) {
    matched_indexes <- matched_slice_indexes(x, y)
    x <- x[, matched_indexes]
    y <- y[, matched_indexes]

    inner_merge_slices(x, y)
}

matched_slice_indexes <- function(s1, s2) {
    index_s1 <- attr(s1, "index")
    index_s2 <- attr(s2, "index")

    match_s1_s2 <- match(index_s1, index_s2)
    matched_indexes <- index_s1[!is.na(match_s1_s2)]

    return(matched_indexes)
}

inner_merge_slices <- function(s1, s2) {

    attr_s1 <- attributes(s1)
    attributes(s1) <- NULL
    names(s1)      <- attr_s1$names

    attr_s2 <- attributes(s2)
    attributes(s2) <- NULL
    names(s2)      <- attr_s2$names

    list <- c(s1, s2)

    new_slice_artifact(list, attr_s1$index, NA)
}

#' Remove \code{NA} From Slices
#' 
#' Removes all indexes where there is at least one feature with \code{NA}s

na.exclude.slice_artifact <- function(object, ...) {
    indexes <- attr(object, "index")
    has_na  <- sapply(object, function(f) sapply(f, function(v) any(is.na(v))))
    has_na  <- rowSums(has_na) > 0

    indexes_keep <- indexes[!has_na]

    object[, indexes_keep]
}

#' Combines Slice Two Features Into One
#' 
#' Concatenates two features of a slice into a single one
#' 
#' @param slice a \code{slice_artifact} object
#' @param feature1,feature2 the features which will be combined into one
#' @param return.all boolean indicating if the returned object should contain all remaining features
#'     along the merged one

combine_features <- function(slice, feature1, feature2, return.all = FALSE) {

    all_features <- names(slice)

    comb_list <- slice[c(feature1, feature2)]
    comb_list <- list(mapply(c, slice[[feature1]], slice[[feature2]], SIMPLIFY = FALSE))
    names(comb_list) <- paste0(feature1, "_c_", feature2)

    if (return.all) {
        all <- slice[!(names(slice) %in% c(feature1, feature2))]
        all <- c(unclass(all), unclass(comb_list))

        new_slice_artifact(all, attr(slice, "index"), NA)
    } else {
        new_slice_artifact(comb_list, attr(slice, "index"), NA)
    }
}

#' Converts \code{slice_artifact} To A \code{data.table}
#' 
#' Transforms an object of class \code{slice_artifact} to a \code{data.table}

as.data.table.slice_artifact <- function(x, ...) {
    features <- lapply(x, unlist)
    features <- lapply(features, matrix, ncol = length(x))
    features <- lapply(names(features), function(s) {
        m <- features[[s]]
        colnames(m) <- paste0(s, "_", seq_len(ncol(m)))
        m
    })
    features <- Reduce(cbind, features)

    index <- attr(x, "index")

    dt <- data.table(index = index)
    dt <- cbind(dt, as.data.table(features))

    return(dt)
}