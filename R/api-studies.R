##' @importFrom jsonlite unbox
##' @importFrom httr content
## Return a list of studies from the OpenTree docstore that match a given properties
.studies_find_studies <- function(property = NULL, value = NULL, verbose = FALSE,
                                  exact = FALSE, ...) {
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
    req_body$verbose <- jsonlite::unbox(verbose)
    req_body$exact <- jsonlite::unbox(exact)
    res <- otl_POST(path="studies/find_studies/",
                    body=req_body,
                    ...)
    res
}

##' @importFrom jsonlite unbox
##' @importFrom httr content
## Return a list of trees from the OpenTree docstore that match a given properties
.studies_find_trees <- function(property=NULL, value=NULL, verbose=FALSE,
                                exact=FALSE, ...) {
    if (!is.logical(verbose)) {
        stop("Argument \'verbose\' must be of class \"logical\"")
    }
    if (!is.logical(exact)) {
        stop("Argument \'exact\' must be of class \"logical\"")
    }
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

    res <- otl_POST(path="studies/find_trees/",
                    body=c(req_body,
                           jsonlite::unbox(verbose),
                           jsonlite::unbox(exact)), ...)
    res
}


##' @importFrom httr content
## Return a list of properties that can be used to search studies and trees
.studies_properties <- function() {
    res <- otl_POST(path="studies/properties/", body=list())
    res
}


##' @importFrom httr content
## Get a study from the OpenTree docstore
.get_study <- function(study_id = NULL, format = c("", "nexus", "newick", "nexml", "json"),
                       ...) {
    if (is.null(study_id)) {
        stop("Must supply a \'study_id\' argument")
    } else if (!is.character(study_id)) {
        stop("Argument \'study_id\' must be of class \"character\"")
    }
    format <- match.arg(format)
    res <- otl_GET(path=paste("study",
                              paste0(study_id, otl_formats(format)), sep="/"),
                   ...)
    res
}


##' @importFrom httr content
## Get a tree in a study from the OpenTree docstore
.get_study_tree <- function(study_id=NULL, tree_id=NULL, format=c("json", "newick", "nexus"),
                            tip_label = c("ot:originallabel", "ot:ottid", "ot:otttaxonname"),
                            ...) {
    if (is.null(study_id)) {
        stop("Must supply a \'study_id\' argument")
    } else if (!is.character(study_id)) {
        stop("Argument \'study_id\' must be of class \"character\"")
    }
    if (is.null(tree_id)) {
        stop("Must supply a \'tree\' argument")
    } else if (!is.character(tree_id)) {
        stop("Argument \'tree\' must be of class \"character\"")
    }
    format <- match.arg(format)
    tip_label <- match.arg(tip_label)
    tip_label <- paste0("/?tip_label=", tip_label)
    tree_file <- paste0(tree_id, otl_formats(format), tip_label)
    res <- otl_GET(path=paste("study", study_id, "tree", tree_file, sep="/"), ...)
    res
}

##' @importFrom httr content
.get_study_meta <- function(study_id, ...) {
    otl_GET(path= paste("study", study_id, "meta", sep="/"), ...)
}


##' @importFrom httr content
.get_study_subtree <- function(study_id, tree_id, subtree_id,
                               format=c("newick", "nexus", "nexml", "json"), ...) {
    if (is.null(study_id)) {
        stop("Must supply a \'study_id\' argument")
    } else if (!is.character(study_id)) {
        stop("Argument \'study_id\' must be of class \"character\"")
    }
    if (is.null(tree_id)) {
        stop("Must supply a \'tree\' argument")
    } else if (!is.character(tree_id)) {
        stop("Argument \'tree\' must be of class \"character\"")
    }
    if (is.null(subtree_id)) {
        stop("Must supply a \'subtree\' argument")
    } else if (!is.character(subtree_id)) {
        stop("Argument \'subtree\' must be of class \"character\"")
    }
    format <- match.arg(format)
    format <- otl_formats(format)
    url_stem <- paste("study", study_id, "tree", paste0(tree_id, format), sep="/")
    res <- otl_GET(path=paste(url_stem, "?subtree_id=", subtree_id, sep=""), ...)
    res
}

### Let's not worry about those for now, as their results could be
### obtained using get_study_tree

get_study_otu <- function(study_id, otu=NULL, ...) {
    otl_GET(path=paste("study", study_id, "otu", otu, sep="/"), ...)
}

get_study_otus <- function(study_id, otus, ...) {
    otl_GET(path=paste("study", study_id, "otu", otus, sep="/"), ...)
}

get_study_otumap <- function(study_id, ...) {
    otl_GET(path=paste("study", study_id,"otumap", sep="/"))
}
