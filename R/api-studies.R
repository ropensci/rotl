## Return a list of studies from the OpenTree docstore that match a given properties
.studies_find_studies <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE) {
    if (!is.logical(verbose)) stop("Argument \'verbose\' should be logical")
    if (!is.logical(exact)) stop("Argument \'exact\' should be logical")

    req_body <- list()
    if (!is.null(property)) {
        	if (!is.character(property)) {
            stop("Argument \'property\' must be of class \"character\"")
        }
        req_body$property <- jsonlite::unbox(property)
    } else {
    	    stop("Must supply a \'property\' argument")
    }
    if (!is.null(value)) {
    	    if (!is.character(value)) {
            stop("Argument \'value\' must be of class \"character\"")
        }
        req_body$value <- jsonlite::unbox(value)
    } else {
    	    stop("Must supply a \'value\' argument")
    }
    res <- otl_POST(path="studies/find_studies/", body=c(req_body,
                                                  jsonlite::unbox(verbose),
                                                  jsonlite::unbox(exact)))
    cont <- httr::content(res)
    return(cont)
}


## Return a list of trees from the OpenTree docstore that match a given properties
.studies_find_trees <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE) {
    req_body <- list()
    if (!is.null(property)) {
    	    if (!is.character(property)) {
            stop("Argument \'property\' must be of class \"character\"")
        }
        req_body$property <- jsonlite::unbox(property)
    } else {
    	    stop("Must supply a \'property\' argument")
    }
    if (!is.null(value)) {
    	    if (!is.character(value)) {
            stop("Argument \'value\' must be of class \"character\"")
        }
        req_body$value <- jsonlite::unbox(value)
    } else {
    	    stop("Must supply a \'value\' argument")
    }
    if (!is.logical(verbose)) {
        stop("Argument \'verbose\' must be of class \"logical\"")
    }
    if (!is.logical(exact)) {
        stop("Argument \'exact\' must be of class \"logical\"")
    }
    res <- otl_POST(path="studies/find_trees/", body=c(req_body,
                                                jsonlite::unbox(verbose),
                                                jsonlite::unbox(exact)))
    cont <- httr::content(res)
    return(cont)
}


## Return a list of properties that can be used to search studies and trees
.studies_properties <- function() {
    res <- otl_POST(path="studies/properties/", body=list())
    cont <- httr::content(res)
    return(cont)
}


## Get a study from the OpenTree docstore
.get_study <- function(study_id=NULL, format=c("", "nexus", "newick", "nexml", "json")) {
    if (is.null(study_id)) {
    	    stop("Must supply a \'study_id\' argument")
    } else if (!is.character(study_id)) {
        stop("Argument \'study_id\' must be of class \"character\"")
    }
    format <- match.arg(format)
    res <- otl_GET(path=paste("study", paste0(study_id, otl_formats(format)), sep="/"))
    cont <- httr::content(res)
    return(cont)
}


## Get a tree in a study from the OpenTree docstore
.get_study_tree <- function(study_id=NULL, tree_id=NULL, format=c("nexus", "newick", "json")) {
    if (is.null(study_id)) {
    	    stop("Must supply a \'study_id\' argument")
    } else if (!is.character(study_id)) {
        stop("Argument \'study_id\' must be of class \"character\"")
    }
    if (is.null(tree)) {
        stop("Must supply a \'tree\' argument")
    } else if (!is.character(tree)) {
        stop("Argument \'tree\' must be of class \"character\"")
    }
    format <- match.arg(format)
    tree_file <- paste(tree, otl_formats(format), sep="")
    res <- otl_GET(path=paste("study", study, "tree", tree_file, sep="/"))
    cont <- httr::content(res)
    return(cont)
}

.get_study_meta <- function(study_id){
    httr::content(otl_GET(path= paste("study", study_id, "meta", sep="/")))
}


.get_study_subtree <- function(study, tree, subtree_id,
                               format=c("newick", "nexus", "nexml", "json")) {
    format <- match.arg(format)
    format <- otl_formats(format)
    url_stem <- paste("study", study, "tree", paste0(tree, format), sep="/")
    res <- otl_GET(path=paste(url_stem, "?subtree_id=", subtree_id, sep=""))
    httr::content(res)
}
