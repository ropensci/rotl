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
.taxonomy_taxon_info <- function(ott_id=NULL,
                                 include_children = FALSE,
                                 include_lineage = FALSE,
                                 include_terminal_descendants = FALSE,
                                  ...) {
    ott_id <- check_ott_ids(ott_id)

    if (length(ott_id) > 1) {
        stop("Must only supply one ", sQuote("ott_id"), " argument")
    }

    assertthat::assert_that(assertthat::is.flag(include_children))
    assertthat::assert_that(assertthat::is.flag(include_lineage))
    assertthat::assert_that(assertthat::is.flag(include_terminal_descendants))
    q <- list(ott_id=jsonlite::unbox(ott_id),
              include_children = jsonlite::unbox(include_children),
              include_lineage = jsonlite::unbox(include_lineage),
              include_terminal_descendants = jsonlite::unbox(include_terminal_descendants))
    res <- otl_POST(path="/taxonomy/taxon_info", body=q, ...)
    res
}


##' @importFrom jsonlite unbox
##' @importFrom httr content
## Get a subtree from the OpenTree Taxonomy (OTT) taxonomic tree
.taxonomy_subtree <- function(ott_id=NULL, label_format=NULL, ...) {
    ott_id <- check_ott_ids(ott_id)

    if (length(ott_id) > 1) {
        stop("Must only supply one ", sQuote("ott_id"), " argument")
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

    ott_ids <- check_ott_ids(ott_ids)
    q <- list(ott_ids=ott_ids)
    res <- otl_POST(path="/taxonomy/mrca", body=q, ...)
    res
}
