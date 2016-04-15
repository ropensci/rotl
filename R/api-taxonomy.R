##' @importFrom httr content
## Summary information about the OpenTree Taxaonomy (OTT)
.taxonomy_about <- function(...) {
    res <- otl_POST(path="/taxonomy/about", body=list(), ...)
    res
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
##' @importFrom assertthat is.flag
##' @importFrom assertthat assert_that
## Information about an OpenTree Taxonomy (OTT) taxon
.taxonomy_taxon <- function(ott_id=NULL, include_lineage = FALSE,
                            list_terminal_descendants = FALSE, ...) {
    if (is.null(ott_id)) {
        stop("Must supply an \'ott_id\' argument")
    } else if (length(ott_id) > 1) {
        stop("Must only supply one \'ott_id\' argument")
    } else if (!check_numeric(ott_id)) {
        stop("Argument \'ott_id\' must look like a number.")
    }
    assertthat::assert_that(assertthat::is.flag(include_lineage))
    assertthat::assert_that(assertthat::is.flag(list_terminal_descendants))
    q <- list(ott_id=jsonlite::unbox(ott_id),
              include_lineage = jsonlite::unbox(include_lineage),
              list_terminal_descendants = jsonlite::unbox(list_terminal_descendants))
    res <- otl_POST(path="/taxonomy/taxon", body=q, ...)
    res
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a subtree from the OpenTree Taxonomy (OTT) taxonomic tree
.taxonomy_subtree <- function(ott_id=NULL, label_format=NULL, ...) {
    if (is.null(ott_id)) {
        stop("Must supply an \'ott_id\' argument")
    } else if (length(ott_id) > 1) {
        stop("Must only supply one \'ott_id\' argument")
    } else if (!check_numeric(ott_id)) {
        stop("Argument \'ott_id\' must look like a number.")
    }
    q <- list(ott_id=jsonlite::unbox(ott_id))
    if (!is.null(label_format)) {
        if (!check_label_format(label_format)) {
            stop(sQuote("label_format"), " must be one of: ", sQuote("name"), ", ",
                 sQuote("id"), ", or ", sQuote("name_and_id"))
        }
        q$label_format <- jsonlite::unbox(label_format)
    }
    res <- otl_POST(path="/taxonomy/subtree", body=q, ...)
    res
}


##' @importFrom httr content
## Get the most recent common ancestor (MRCA) from nodes in the OpenTree Taxonomy (OTT)
.taxonomy_mrca <- function (ott_ids = NULL, ...) {
    if (is.null(ott_ids)) {
        stop("Must supply an \'ott_ids\' argument")
    } else if (!all(sapply(ott_ids, check_numeric))) {
        stop("Argument \'ott_ids\' must look like a number.")
    }
    q <- list(ott_ids=ott_ids)
    res <- otl_POST(path="/taxonomy/mrca", body=q, ...)
    res
}
