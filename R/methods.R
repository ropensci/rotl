############################################################################
## methods                                                                ##
############################################################################

##' Methods for dealing with objects returned by functions dealing
##' with the Taxonomy and the Taxonomic Name Resolution Services APIs.
##'
##' This is the page for the generic methods. See the help pages for
##' \code{\link{taxonomy_taxon_info}}, \code{\link{taxonomy_mrca}}, and
##' \code{\link{tnrs_match_names}} for more information.
##'
##' @title Methods for Taxonomy
##' @param tax an object returned by \code{\link{taxonomy_taxon_info}},
##'     \code{\link{taxonomy_mrca}}, or \code{\link{tnrs_match_names}}
##' @param ... additional arguments (see
##'     \code{\link{tnrs_match_names}})
##' @rdname taxonomy-methods
##' @export

tax_rank <- function(tax) { UseMethod("tax_rank") }

##' @export
##' @rdname taxonomy-methods
ott_id <- function(tax, ...) { UseMethod("ott_id") }

##' @export
##' @rdname taxonomy-methods
synonyms <- function(tax, ...) { UseMethod("synonyms") }

##' @export
tax_sources <- function(tax, ...) UseMethod("tax_sources")

##' @export
is_suppressed <- function(tax, ...) UseMethod("is_suppressed")

##' @export
unique_name <- function(tax, ...) UseMethod("unique_name")

##' @export
tax_name <- function(tax, ...) UseMethod("tax_name")
