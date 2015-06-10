##' Summary information about the OpenTree Taxaonomy (OTT)
##'
##' Return information about the taxonomy, including version.
##' @title Taxonomy about
##' @return Some JSON
##' @examples
##' taxonomy_about()
##' @export
taxonomy_about <- function (...) {
    res <- .taxonomy_about(...)
    return(res)
}


##' Taxon Info
##'
##' Given an ott id, return information about the specified taxon.
##' @title taxon
##' @param ott_id The OTT id of the taxon of interest. Not optional.
##' @return Some JSON
##' @examples
##' req <- taxonomy_taxon(ott_id=515698)
##' @export
taxonomy_taxon <- function (ott_id=NULL, ...) {
    res <- .taxonomy_taxon(ott_id = ott_id)
    return(res)
}


##' Taxonomic Subtree
##'
##' Given an ott id, return complete taxonomy subtree descended from specified taxon.
##' @title taxon
##' @param ott_id The OTT id of the taxon of interest.  Not optional.
##' @param output_format
##' @param file
##' @return Some JSON
##' @examples
##' req <- taxonomy_subtree(ott_id=515698)
##' @export
taxonomy_subtree <- function (ott_id=NULL,
                              output_format = c("taxa_all", "newick", "phylo", "raw", "taxa_species", "taxa_internal"),
                              file, ...) {
    output_format <- match.arg(output_format)
    res <- .taxonomy_subtree(ott_id = ott_id, ...)
    if (!missing(file) && !identical(output_format, "newick"))
        warning("'file' argument is ignored, you can only write newick tree strings to a file.")
    if (identical(output_format, "raw")) {
        return(res)
    } else if (identical(output_format, "newick")) {
        res <- res$subtree
        if (!missing(file)) {
            cat(res, file = file)
            return(file.exists(file))
        }
    } else { ## in all other cases, need to convert to phylo object
        res <- phylo_from_otl(res)
        if (identical(output_format, "taxa_internal")) {
            res <- res$node.label
        } else if (identical(output_format, "taxa_species")) {
            res <- res$tip.label
        } else if (identical(output_format, "taxa_all")) {
            res <- c(res$tip.label, res$node.label)
        }
    }
    return(res)
}


##' Taxonomic LICA
##'
##' Given a set of ott ids, get the taxon that is the least inclusive common ancestor
##' (the LICA) of all the identified taxa. A taxonomic LICA is analogous to a most recent
##' common ancestor (MRCA) in a phylogenetic tree.
##' @title lica
##' @param ott_ids The vector of ott ids for the taxa whose LICA is to be found. Not optional.
##' @return Some JSON
##' @examples
##' req <- taxonomy_lica(ott_ids=c(515698,590452,409712,643717))
##' @export
taxonomy_lica <- function (ott_ids=NULL, ...) {
	res <- .taxonomy_lica(ott_ids = ott_ids, ...)
	return(res)
}
