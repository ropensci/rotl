##' @importFrom httr content
## Summary information about the OpenTree Taxaonomy (OTT)
.taxonomy_about <- function(...) {
    res <- otl_POST(path="/taxonomy/about", body=list(), ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Information about an OpenTree Taxonomy (OTT) taxon
.taxonomy_taxon <- function(ott_id=NULL, ...) {
    if (is.null(ott_id)) {
        stop("Must supply an \'ott_id\' argument")
    } else if (length(ott_id) > 1) {
        stop("Must only supply one \'ott_id\' argument")
    } else if (!check_numeric(ott_id)) {
        stop("Argument \'ott_id\' must look like a number.")
    }
    q <- list(ott_id=jsonlite::unbox(ott_id))
    res <- otl_POST(path="/taxonomy/taxon", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a subtree from the OpenTree Taxonomy (OTT) taxonomic tree
.taxonomy_subtree <- function(ott_id=NULL, ...) {
    if (is.null(ott_id)) {
        stop("Must supply an \'ott_id\' argument")
    } else if (length(ott_id) > 1) {
        stop("Must only supply one \'ott_id\' argument")
    } else if (!check_numeric(ott_id)) {
        stop("Argument \'ott_id\' must look like a number.")
    }
    q <- list(ott_id=jsonlite::unbox(ott_id))
    res <- otl_POST(path="/taxonomy/subtree", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}


##' @importFrom httr content
## Get the least inclusive common ancestor (LICA) from nodes in the OpenTree Taxonomy (OTT)
.taxonomy_lica <- function (ott_ids = NULL, ...) {
    if (is.null(ott_ids)) {
        stop("Must supply an \'ott_ids\' argument")
    } else if (!all(sapply(ott_ids, check_numeric))) {
        stop("Argument \'ott_ids\' must look like a number.")
    }
    q <- list(ott_ids=ott_ids)
    res <- otl_POST(path="/taxonomy/lica", body=q, ...)
    cont <- httr::content(res)
    return(cont)
}
