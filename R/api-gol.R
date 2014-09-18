## Summary information about the Graph of Life
.gol_about <- function() {
    res <- otl_POST(path="graph/about", body=list())
    cont <- httr::content(res)
    if (length(cont) < 1) {
        warning("Nothing returned")
    }
    return(cont)
}


## Get a source tree from the Graph of Life
.gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL) {
    if (any(is.null(c(study_id, tree_id, git_sha)))) {
    	    stop("Must supply all arguments: \'study_id\', \'tree_id\', \'git_sha\'")
    }
    q <- list(study_id=jsonlite::unbox(study_id), tree_id=jsonlite::unbox(tree_id),
        git_sha=jsonlite::unbox(git_sha))
    res <- otl_POST(path="graph/source_tree", body=q)
    cont <- httr::content(res)
    return(cont)
}


## Get summary information about a node in the Graph of Life
gol_node_info <- function(node_id=NULL, ott_id=NULL, include_lineage=FALSE) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only \'node_id\' OR \'ott_id\'")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a \'node_id\' OR \'ott_id\'")
    }
    if (!is.logical(include_lineage)) {
		stop("Argument \'include_lineage\' should be logical")
	}
    if (!is.null(ott_id)) {
        q <- list(ott_id=jsonlite::unbox(ott_id), include_lineage=jsonlite::unbox(include_lineage))
    } else {
    	    q <- list(node_id=jsonlite::unbox(node_id), include_lineage=jsonlite::unbox(include_lineage))
    }
    res <- otl_POST(path="graph/node_info", body=q)
    cont <- httr::content(res)
    return(cont)
}
