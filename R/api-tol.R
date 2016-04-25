##' @importFrom jsonlite unbox
##' @importFrom httr content
##' @importFrom assertthat is.flag
## Summary information about the OpenTree Tree of Life
.tol_about <- function(include_source_list=FALSE, ...) {
    if (!assertthat::is.flag(include_source_list)) {
        stop("Argument ", sQuote("include_ource_list"), " must be of class ", sQuote("logical"))
    }
    q <- list(include_source_list=jsonlite::unbox(include_source_list))
    res <- otl_POST(path="tree_of_life/about", body=q, ...)
    res
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get summary information about a node in the OpenTree Tree of Life
.tol_node_info <- function(ott_id=NULL, node_id=NULL, include_lineage=FALSE, ...) {
    if (!is.logical(include_lineage)) {
        stop("Argument \'include_lineage\' must be of class \"logical\"")
    }
    if (is.null(ott_id) && is.null(node_id)) {
        stop("Must provide either ", sQuote("ott_id"), " or ", sQuote("node_id"))
    }
    if (!is.null(ott_id) && !is.null(node_id)) {
        stop("Must provide either ", sQuote("ott_id"), " or ", sQuote("node_id"), ", not both.")
    }
    if (!is.null(ott_id)) {
        ott_id <- check_ott_ids(ott_id)
        if (length(ott_id) != 1)
            stop("Please provide a single ", sQuote("ott_id"), call. = FALSE)
        q <- list(ott_id=jsonlite::unbox(ott_id), include_lineage=jsonlite::unbox(include_lineage))
    } else {
        if (!check_valid_node_id(node_id)) {
            stop("Argument ", sQuote("node_id"), " must look like \'ott123\' or \'mrcaott123ott456\'.")
        }
        q <- list(node_id=jsonlite::unbox(node_id), include_lineage=jsonlite::unbox(include_lineage))
    }
    res <- otl_POST(path="tree_of_life/node_info", body=q, ...)
    res
}


##' @importFrom httr content
## Get the MRCA of a set of nodes
.tol_mrca <- function(ott_ids=NULL, node_ids=NULL, ...) {
    if (is.null(ott_ids) && is.null(node_ids)) {
        stop("Must provide ", sQuote("ott_ids"), " or ", sQuote("node_ids"), " (or both).")
    }
    q <- list()
    if (!is.null(ott_ids)) {
        ott_ids <- check_ott_ids(ott_ids)
        q$ott_ids <- ott_ids
    }
    if (!is.null(node_ids)) {
        check_node_ids(node_ids)
        q$node_ids <- node_ids
    }
    res <- otl_POST(path="tree_of_life/mrca", body=q, ...)
    res
}


# ignoring 'include_lineage' for subtree below. arguson only

##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a subtree from the OpenTree Tree of Life
.tol_subtree <- function(ott_id=NULL, node_id=NULL, label_format=NULL, ...) {
    if (is.null(ott_id) && is.null(node_id)) {
        stop("Must provide either ", sQuote("ott_id"), " or ", sQuote("node_id"))
    }
    if (!is.null(ott_id) && !is.null(node_id)) {
        stop("Must provide either ", sQuote("ott_id"), " or ", sQuote("node_id"), ", not both.")
    }
    if (!is.null(ott_id)) {
        ott_id <- check_ott_ids(ott_id)
        if (length(ott_id) !=  1)
            stop("Please provide a single ", sQuote("ott_id"))
        q <- list(ott_id=jsonlite::unbox(ott_id))
    } else {
        if (!check_valid_node_id(node_id)) {
            stop("Argument ", sQuote("node_id"), " must look like \'ott123\' or \'mrcaott123ott456\'.")
        }
        q <- list(node_id=jsonlite::unbox(node_id))
    }
    if (!is.null(label_format)) {
        if (!check_label_format(label_format)) {
            stop(sQuote("label_format"), " must be one of: ", sQuote("name"), ", ",
                 sQuote("id"), ", or ", sQuote("name_and_id"))
        }
        q$label_format <- jsonlite::unbox(label_format)
    }
    res <- otl_POST(path="tree_of_life/subtree", body=q, ...)
    res
}


##' @importFrom httr content
## Get an induced subtree from the OpenTree Tree of Life from a set of nodes
.tol_induced_subtree <- function(ott_ids=NULL, node_ids=NULL, label_format=NULL, ...) {
    if (is.null(ott_ids) && is.null(node_ids)) {
        stop("Must provide ", sQuote("ott_ids"), " or ", sQuote("node_ids"), " (or both).")
    }
    q <- list()
    if (!is.null(label_format)) {
        if (!check_label_format(label_format)) {
            stop(sQuote("label_format"), " must be one of: ", sQuote("name"), ", ",
                 sQuote("id"), ", or ", sQuote("name_and_id"))
        }
        q$label_format <- jsonlite::unbox(label_format)
    }
    if (!is.null(ott_ids)) {
        ott_ids <- check_ott_ids(ott_ids)
        q$ott_ids <- ott_ids
    }
    if (!is.null(node_ids)) {
        check_node_ids(node_ids)
        q$node_ids <- node_ids
    }
    if ((length(ott_ids) + length(node_ids)) < 2) {
        stop("At least two valid ", sQuote("ott_ids"), " or ", sQuote("node_ids"), " must be provided.")
    }
    res <- otl_POST("tree_of_life/induced_subtree", body=q, ...)
    res
}
