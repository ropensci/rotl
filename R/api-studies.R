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
.get_study <- function(study=NULL, format=c("", "nexus", "newick", "nexml", "json")) {
    if (is.null(study)) {
    	    stop("Must supply a \'study\' argument")
    } else if (!is.character(study)) {
        stop("Argument \'study\' must be of class \"character\"")
    }
    format <- match.arg(format)
    res <- otl_GET(path=paste("study", paste0(study, otl_formats(format)), sep="/"))
    cont <- httr::content(res)
    return(cont)
}


## Get a tree in a study from the OpenTree docstore
.get_study_tree <- function(study=NULL, tree=NULL, format=c("nexus", "newick", "json")) {
    if (is.null(study)) {
    	    stop("Must supply a \'study\' argument")
    } else if (!is.character(study)) {
        stop("Argument \'study\' must be of class \"character\"")
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

.get_study_meta <- function(study){
    httr::content(otl_GET(path= paste("study", study, "meta", sep="/")))
}
