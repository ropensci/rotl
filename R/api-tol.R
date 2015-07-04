##' @importFrom jsonlite unbox
##' @importFrom httr content
## Summary information about the OpenTree Tree of Life
.tol_about <- function(study_list=FALSE, ...) {
    if (!is.logical(study_list)) {
        stop("Argument \'study_list\' must be of class \"logical\"")
    }
    q <- list(study_list=jsonlite::unbox(study_list))
    res <- otl_POST(path="tree_of_life/about", body=q, ...)
    cont <- httr::content(res)
    return(invisible(cont))
}


##' @importFrom httr content
## Get the MRCA of a set of nodes
.tol_mrca <- function(ott_ids=NULL, ...) {
    if (is.null(ott_ids)) {
        stop("Must supply at least some \'ott_ids\'.")
    }

    if (!is.null(ott_ids)) {
        if (!all(sapply(ott_ids, check_numeric))) {
            stop("all ", sQuote("ott_ids"), " must look like numbers.")
        }
        q <- list(ott_ids = ott_ids)
    }
    res <- otl_POST(path="tree_of_life/mrca", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a subtree from the OpenTree Tree of Life
.tol_subtree <- function(ott_id=NULL, tree_id=NULL, ...) {
    if (is.null(ott_id)) {
        stop("One \'ott_id\' must be provided")
    }
    if (!is.null(tree_id)) {
        warning("\'tree_id\' is currently ignored as the OTL only supports one version of tree of life.")
    }
    if (!is.null(ott_id)) {
        if (length(ott_id) > 1) stop("A subtree can only be inferred from a single ", sQuote("ott_id"), ".")
        if (!check_numeric(ott_id)) stop(sQuote("ott_id"), " needs to look like a number.")
        q <- list(ott_id = jsonlite::unbox(ott_id))
    }
    res <- otl_POST(path="tree_of_life/subtree", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom httr content
## Get an induced subtree from the OpenTree Tree of Life from a set of nodes
.tol_induced_subtree <- function(ott_ids=NULL, ...) {
    if (is.null(ott_ids)) {
        stop("At least two valid ", sQuote("ott_ids"), " must be provided")
    }

    if (!is.null(ott_ids)) {
        if (any(is.na(ott_ids))) {
        	stop("NAs are not allowed for argument \'ott_ids\'")
        }
        if (!all(sapply(ott_ids, check_numeric))) {
            stop(sQuote("ott_ids"), " must look like numbers")
        }
    }

    q <- list(ott_ids  = ott_ids)

    res <- otl_POST("tree_of_life/induced_subtree", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}
