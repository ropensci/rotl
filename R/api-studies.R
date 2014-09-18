## Return a list of studies from the OpenTree docstore that match a given properties
.studies_find_studies <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE) {
    if (!is.logical(verbose)) stop("Argument \'verbose\' should be logical")
    if (!is.logical(exact)) stop("Argument \'exact\' should be logical")
    
    req_body <- list()
    if (!is.null(property)) {
        req_body$property <- jsonlite::unbox(property)
    } else {
    	stop("Argument \'property\' must be specified")
    }
    if (!is.null(value)) {
        req_body$value <- jsonlite::unbox(value)
    } else {
    	stop("Argument \'value\' must be specified")
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
        req_body$property <- jsonlite::unbox(property)
    } else {
    	stop("Argument \'property\' must be specified")
    }
    if (!is.null(value)) {
        req_body$value <- jsonlite::unbox(value)
    } else {
    	stop("Argument \'value\' must be specified")
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
        stop("\'study\' must be specified")
    }
    format <- match.arg(format)
    res <- otl_GET(path=paste("study", paste0(study, otl_formats(format)), sep="/"))
    cont <- httr::content(res)
    return(cont)
}


## Get a tree in a study from the OpenTree docstore
.get_study_tree <- function(study=NULL, tree=NULL, format=c("", "nexus", "newick", "json")) {
    if (is.null(study)) {
        stop("\'study\' must be specified")
    }
    if (is.null(tree)) {
        stop("\'tree\' must be specified")
    }
    format <- match.arg(format)
    tree_file <- paste(tree, otl_formats(format), sep="")
    res <- otl_GET(path=paste("study", study, "tree", tree_file, sep="/"))
    cont <- httr::content(res)
    return(cont)
}











