##' @importFrom jsonlite unbox
##' @importFrom httr content
##' @importFrom assertthat is.string is.flag
## Match taxon names
.tnrs_match_names <- function(names=NULL, context_name=NULL,
                              do_approximate_matching=TRUE,
                              ids=NULL, include_suppressed=FALSE, ...) {

    if (is.null(names)) {
        stop("You must supply a ", sQuote("names"), " argument")
    } else if (!is.character(names)) {
        stop("Argument ", sQuote("names"), " must be of class ",
             sQuote("character"))
    }
    if (!is.null(ids)) {
        if (length(ids) != length(names)) {
            stop("Arguments ", sQuote("ids"), " and ",
                 sQuote("names"), " must be of the same length")
        } else if (!is.character(ids)) {
            stop("Argument ", sQuote("ids"), " must be of class ",
                 sQuote("character"))
        }
    }
    if (!assertthat::is.flag(do_approximate_matching)) {
        stop("Argument ", sQuote("do_approximate_matching"),
             " must be of class ",
             sQuote("logical"))
    }
    if (!assertthat::is.flag(include_suppressed)) {
        stop("Argument ", sQuote("include_deprecated"), " must be of class ",
             sQuote("logical"))
    }
    if (!is.null(context_name)){
        if(!assertthat::is.string(context_name)) {
            stop("Argument ", sQuote("context_name"), " must be of class ",
                 sQuote("character"))
        }
        context_name <- jsonlite::unbox(context_name)
    }

    q <- list(names = names, context_name = context_name,
              do_approximate_matching = jsonlite::unbox(do_approximate_matching),
              ids = ids, include_suppressed = jsonlite::unbox(include_suppressed))
    toKeep <- sapply(q, is.null)
    q <- q[!toKeep]

    res <- otl_POST("tnrs/match_names", body=q, ...)
    res
}


##' @importFrom httr content
## Get OpenTree TNRS contexts
.tnrs_contexts <- function(...) {
    res <- otl_POST("tnrs/contexts", body=list(), ...)
    res
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
    res
}
