##' @importFrom httr content
## Summary information about the Graph of Life
.gol_about <- function(...) {
    res <- otl_POST(path="graph/about", body=list(), ...)
    cont <- httr::content(res)
    if (length(cont) < 1) {
        warning("Nothing returned")
    }
    return(cont)
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a source tree from the Graph of Life
.gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL, ...) {
    if (is.null(study_id)) {
        stop("Must supply an \'study_id\' argument")
    } else if (!is.character(study_id)) {
        stop("Argument \'study_id\' must be of class \"character\"")
    }
    if (is.null(tree_id)) {
        stop("Must supply a \'tree_id\' argument")
    } else if (!is.character(tree_id)) {
        stop("Argument \'tree_id\' must be of class \"character\"")
    }
    if (is.null(git_sha)) {
        stop("Must supply a \'git_sha\' argument")
    } else if (!is.character(git_sha)) {
        stop("Argument \'git_sha\' must be of class \"character\"")
    }

    q <- list(study_id=jsonlite::unbox(study_id), tree_id=jsonlite::unbox(tree_id),
        git_sha=jsonlite::unbox(git_sha))
    res <- otl_POST(path="graph/source_tree", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}

##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get summary information about a node in the Graph of Life
.gol_node_info <- function(ott_id=NULL, include_lineage=FALSE, ...) {
    if (!is.logical(include_lineage)) {
        stop("Argument \'include_lineage\' must be of class \"logical\"")
    }
    if (is.null(ott_id)) {
        stop(sQuote("ott_id"), " needs to be provided")
    } else {
        if (!check_numeric(ott_id)) {
            stop("Argument \'ott_id\' must look like a number.")
        }
        q <- list(ott_id=jsonlite::unbox(ott_id), include_lineage=jsonlite::unbox(include_lineage))
    }
    res <- otl_POST(path="graph/node_info", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}
