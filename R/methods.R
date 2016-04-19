############################################################################
## methods                                                                ##
############################################################################

##' Methods for dealing with objects containing taxonomic information
##' (Taxonomy, TNRS endpoints)
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

tax_rank <- function(tax, ...) { UseMethod("tax_rank") }

##' @export
##' @rdname taxonomy-methods
ott_id <- function(tax, ...) { UseMethod("ott_id") }

##' @export
##' @rdname taxonomy-methods
synonyms <- function(tax, ...) { UseMethod("synonyms") }

##' @export
##' @rdname taxonomy-methods
tax_sources <- function(tax, ...) UseMethod("tax_sources")

##' @export
##' @rdname taxonomy-methods
is_suppressed <- function(tax, ...) UseMethod("is_suppressed")

##' @export
##' @rdname taxonomy-methods
unique_name <- function(tax, ...) UseMethod("unique_name")

##' @export
##' @rdname taxonomy-methods
tax_name <- function(tax, ...) UseMethod("tax_name")

### flags -----------------------------------------------------------------------

##' @export
##' @rdname match_names-methods
flags <- function(tax, ...) UseMethod("flags")

###

##' Retrieve the detailed information for the list of studies used in
##' the Tree of Life.
##'
##' @title List of studies used in the Tree of Life
##'
##' @details This function takes the object resulting from
##'     \code{tol_about(study_list = TRUE)}, \code{tol_mrca()},
##'     \code{tol_node_info()}, and returns a data frame listing the
##'     \code{tree_id}, \code{study_id} and \code{git_sha} for the
##'     studies currently included in the Tree of Life.
##'
##' @param tax a list containing a \code{source_id_map} slot.
##' @param ... additional arguments (currently unused)
##'
##' @return a data frame
##' @export
source_list <- function(tax, ...) UseMethod("source_list")


##' Extract the lineage information (higher taxonomy) from an object
##' returned by \code{\link{taxonomy_taxon_info}}.
##'
##' The object passed to this function must have been created using
##' the argument \code{include_lineage=TRUE}.
##'
##' @title Lineage of a taxon
##' @param tax an object created by \code{\link{taxonomy_taxon_info}}.
##' @param ... additional arguments (currently unused).
##' @return A list with one slot per taxon that contains a data frame
##'     with 3 columns: the taxonomy rank, the name, and unique name
##'     for all taxa included in the lineage of the taxon up to the
##'     root of the tree.
##' @rdname tax_lineage
##' @export
tax_lineage <- function(tax, ...) UseMethod("tax_lineage")

##' @export
##' @rdname tol_node_info
tol_lineage <- function(tax, ...) UseMethod("tol_lineage")
