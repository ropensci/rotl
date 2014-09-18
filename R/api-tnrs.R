## Match taxon names
.tnrs_match_names <- function(taxon_names=NULL, context_name=NULL, do_approximate_matching=TRUE,
                             ids=NULL, include_deprecated=FALSE, include_dubious=FALSE) {
    if (is.null(taxon_names)) {
        stop("\'names\' must be specified")
    }
    if (!is.null(ids)) {
        if (length(ids) != length(taxon_names))
            stop("\'ids\' and \'taxon_names\' must be of the same length.")
    }
    if (!is.character(taxon_names)) stop("\'taxon_names\' must be a character vector")
    if (!is.logical(do_approximate_matching)) stop("Argument \'do_approximate_matching\' should be logical")
    if (!is.logical(include_deprecated)) stop("Argument \'include_deprecated\' should be logical")
    if (!is.logical(include_dubious)) stop("Argument \'include_dubious\' should be logical")

    q <- list(names = taxon_names, context_name = context_name,
              do_approximate_matching = jsonlite::unbox(do_approximate_matching),
              ids = ids, include_deprecated = jsonlite::unbox(include_deprecated),
              include_dubious = jsonlite::unbox(include_dubious))
    toKeep <- sapply(q, is.null)
    q <- q[!toKeep]

    res <- otl_POST("tnrs/match_names", q)
    cont <- httr::content(res)
    return(cont)
}


## Get OpenTree TNRS contexts
.tnrs_contexts <- function() {
    res <- otl_POST("tnrs/contexts", list())
    cont <- httr::content(res)
    return(cont)
}


## Infer taxonomic context from a set of names
.tnrs_infer_context <- function(taxon_names=NULL) {
    if (is.null(taxon_names)) {
        stop("\'taxon_names\' must be specified")
    }
    q <- list(names=taxon_names)
    res <- otl_POST("tnrs/infer_context", q)
    cont <- httr::content(res)
    return(cont)
}

