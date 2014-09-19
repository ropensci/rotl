##' Summary information about the OpenTree Taxaonomy (OTT)
##' 
##' Return information about the taxonomy, including version.
##' @title Taxonomy about
##' @return Some JSON
##' @examples
##' res <- taxonomy_about()
##' @export
taxonomy_about <- function () {
    res <- .taxonomy_about()
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
taxonomy_taxon <- function (ott_id=NULL) {
    res <- .taxonomy_taxon(ott_id)
    return(res)
}


##' Taxonomic Subtree
##' 
##' Given an ott id, return complete taxonomy subtree descended from specified taxon.
##' @title taxon
##' @param ott_id The OTT id of the taxon of interest.  Not optional.
##' @return Some JSON
##' @examples
##' req <- taxonomy_subtree(ott_id=515698)
##' @export
taxonomy_subtree <- function (ott_id=NULL) {
	res <- .taxonomy_subtree(ott_id)
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
taxonomy_lica <- function (ott_ids=NULL) {
	res <- .taxonomy_lica(ott_ids)
	return(res)
}
