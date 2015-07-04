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
.tol_mrca <- function(ott_ids=NULL, node_ids=NULL, ...) {
    if (is.null(ott_ids) && is.null(node_ids)) {
        stop("Must supply at least one of \'ott_ids\' or \'node_ids\'.")
    }

    if (is.null(node_ids) && !is.null(ott_ids)) {
        if (!all(sapply(ott_ids, check_numeric))) {
            stop("all ", sQuote("ott_ids"), " must look like numbers.")
        }
        q <- list(ott_ids = ott_ids)
    }
    if (!is.null(node_ids) && is.null(ott_ids)) {
        q <- list(node_ids = node_ids)
    }
    if (!is.null(node_ids) && !is.null(ott_ids)) {
        q <- list(ott_ids = ott_ids,
                  node_ids = node_ids)
    }
    res <- otl_POST(path="tree_of_life/mrca", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a subtree from the OpenTree Tree of Life
.tol_subtree <- function(node_id=NULL, ott_id=NULL, tree_id=NULL, ...) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only node_id OR ott_id")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a \'node_id\' OR \'ott_id\'")
    }
    if (!is.null(tree_id)) {
        warning("\'tree_id\' is currently ignored as the OTL only supports one version of tree of life.")
    }
    if (!is.null(ott_id)) {
        if (length(ott_id) > 1) stop("A subtree can only be inferred from a single ", sQuote("ott_id"), ".")
        if (!check_numeric(ott_id)) stop(sQuote("ott_id"), " needs to look like a number.")
        q <- list(ott_id = jsonlite::unbox(ott_id))
    }
    if (!is.null(node_id)) {
        if (length(node_id) > 1) stop("A subtree can only be inferred from a single ", sQuote("node_id"), ".")
        if (!check_numeric(node_id)) stop(sQuote("node_id"), " needs to look like a number.")
        q <- list(node_id = jsonlite::unbox(node_id))
    }
    res <- otl_POST(path="tree_of_life/subtree", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom httr content
## Get an induced subtree from the OpenTree Tree of Life from a set of nodes
.tol_induced_subtree <- function(node_ids=NULL, ott_ids=NULL, ...) {
    if (is.null(node_ids) && is.null(ott_ids)) {
        stop("Must supply \'node_ids\' and/or \'ott_ids\'")
    }

    if (!is.null(ott_ids)) {
        if (any(is.na(ott_ids))) {
        	stop("NAs are not allowed for argument \'ott_ids\'")
        }
        if (!all(sapply(ott_ids, check_numeric))) {
            stop(sQuote("ott_ids"), " must look like numbers")
        }
    }
    if (!is.null(node_ids)) {
        if (any(is.na(node_ids))) {
        	stop("NAs are not allowed for argument \'node_ids\'")
        }
        if (!all(sapply(node_ids, check_numeric))) {
            stop(sQuote("node_ids"), " must look like numbers")
        }
    }

    if (is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids  = ott_ids)
    if (!is.null(node_ids) && is.null(ott_ids)) q <- list(node_ids = node_ids)
    if (!is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    res <- otl_POST("tree_of_life/induced_subtree", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}
