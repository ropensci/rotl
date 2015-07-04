##' @importFrom jsonlite unbox
##' @importFrom httr content
## Match taxon names
.tnrs_match_names <- function(names=NULL, context_name=NULL, do_approximate_matching=TRUE,
                         ids=NULL, include_deprecated=FALSE, include_dubious=FALSE, ...) {
    if (is.null(names)) {
        stop("Must supply a \'names\' argument")
    } else if (!is.character(names)) {
        stop("Argument \'names\' must be of class \"character\"")
    }
    if (!is.null(ids)) {
        if (length(ids) != length(names)) {
            stop("Arguments \'ids\' and \'names\' must be of the same length")
        } else if (!is.character(ids)) {
            stop("Argument \'ids\' must be of class \"character\"")
        }
    }
    if (!is.logical(do_approximate_matching)) {
        stop("Argument \'do_approximate_matching\' must be of class \"logical\"")
    }
    if (!is.logical(include_deprecated)) {
            stop("Argument \'include_deprecated\' must be of class \"logical\"")
    }
    if (!is.logical(include_dubious)) {
        stop("Argument \'include_dubious\' must be of class \"logical\"")
    }
    if (!is.null(context_name)){
        if(!is.character(context_name)) {
            stop("Argument \'context_name\' must be of class \"character\"")
        }
        context_name <- jsonlite::unbox(context_name)
    }


    q <- list(names = names, context_name = context_name,
              do_approximate_matching = jsonlite::unbox(do_approximate_matching),
              ids = ids, include_deprecated = jsonlite::unbox(include_deprecated),
              include_dubious = jsonlite::unbox(include_dubious))
    toKeep <- sapply(q, is.null)
    q <- q[!toKeep]

    res <- otl_POST("tnrs/match_names", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom httr content
## Get OpenTree TNRS contexts
.tnrs_contexts <- function(...) {
    res <- otl_POST("tnrs/contexts", body=list(), ...)
    cont <- httr::content(res)
    return(cont)
}


## Infer taxonomic context from a set of names
.tnrs_infer_context <- function(names=NULL, ...) {
    if (is.null(names)) {
        stop("Must supply a \'names\' argument")
    } else if (!is.character(names)) {
        stop("Argument \'names\' must be of class \"character\"")
    }
    q <- list(names=names)
    res <- otl_POST("tnrs/infer_context", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}
