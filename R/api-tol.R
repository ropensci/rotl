## Summary information about the OpenTree Tree of Life
.tol_about <- function(study_list=FALSE) {
    if (!is.logical(study_list)) {
        stop("Argument \'study_list\' must be of class \"logical\"")
    }
    q <- list(study_list=jsonlite::unbox(study_list))
    res <- otl_POST(path="tree_of_life/about", body=q)
    cont <- httr::content(res)
    return(invisible(cont))
}


## Get the MRCA of a set of nodes
.tol_mrca <- function(ott_ids=NULL, node_ids=NULL) {
    if (!is.null(node_ids) && !is.null(ott_ids)) {
        stop("Use only node_id OR ott_id")
    }
    if (!is.null(node_ids) && !is.numeric(node_ids)) {
    	    stop("Argument \'node_ids\' must be of class \"numeric\"")
    }
    if (!is.null(ott_ids) && !is.numeric(ott_ids)) {
    	stop("Argument \'ott_ids\' must be of class \"numeric\"")
    }
    if (is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids)
    if (!is.null(node_ids) && is.null(ott_ids)) q <- list(node_ids = node_ids)
    if (!is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    res <- otl_POST(path="tree_of_life/mrca", body=q)
    cont <- httr::content(res)
    return(cont)
}


## Get a subtree from the OpenTree Tree of Life
.tol_subtree <- function(node_id=NULL, ott_id=NULL, tree_id=NULL) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only node_id OR ott_id")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a \'node_id\' OR \'ott_id\'")
    }
    if (!is.null(tree_id)) {
        stop("\'tree_id\' is currently ignored")
    }
    if (!is.null(ott_id)) {
        if (!is.numeric(ott_id)) {
            stop("Argument \'ott_id\' must be of class \"numeric\"")
        }
        q <- list(ott_id = jsonlite::unbox(ott_id))
    }
    if (!is.null(node_id)) {
    	    if (!is.numeric(node_id)) {
            stop("Argument \'node_id\' must be of class \"numeric\"")
        }
        q <- list(node_id = jsonlite::unbox(node_id))
    }
    res <- otl_POST(path="tree_of_life/subtree", body=q)
    cont <- httr::content(res)
    return(cont)
}


## Get an induced subtree from the OpenTree Tree of Life from a set of nodes
.tol_induced_subtree <- function(node_ids=NULL, ott_ids=NULL) {
    if (is.null(node_ids) && is.null(ott_ids)) {
        stop("Must supply \'node_ids\' and/or \'ott_ids\'")
    }
    
    if (!is.null(ott_ids)) {
        if (!is.numeric(ott_ids)) {
        	stop("Argument \'ott_ids\' must be of class \"numeric\"")
        }
        if (any(is.na(ott_ids))) {
        	stop("NAs are not allowed for argument \'ott_ids\'")
        }
    }
    if (!is.null(node_ids)) {
        if (!is.numeric(node_ids)) {
        	stop("Argument \'node_ids\' must be of class \"numeric\"")
        }
        if (any(is.na(node_ids))) {
        	stop("NAs are not allowed for argument \'node_ids\'")
        }
    }
    
    if (is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids  = ott_ids)
    if (!is.null(node_ids) && is.null(ott_ids)) q <- list(node_ids = node_ids)
    if (!is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    res <- otl_POST("tree_of_life/induced_subtree", body=q)
    cont <- httr::content(res)
    return(cont)
}
