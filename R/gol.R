## *** NOTE: the development server has a newer tree graph DB ***
otl_url <- function() { "http://devapi.opentreeoflife.org" }


##' Basic information about the graph
##'
##' Returns summary information about the entire graph database, including identifiers for the
##' taxonomy and source trees used to build it.
##' @title Information about the tree of life
##' @return Some JSON
##' @export
gol_about <- function(study_list=FALSE) {
	if (!is.logical(study_list)) {
		stop("Argument study_list should be logical")
	}
    otl_POST(path="graph/about", body=list())
}


##' Returns a reconstructed source tree
##'
##' source tree
##' @title Get reconstructed source tree
##' @param study_id The study identifier. Will typically include a prefix ("pg_" or "ot_").
##' @param tree_id The tree identifier for a given study.
##' @param git_sha The git SHA identifying a particular source version.
##' @param format The name of the return format. The only currently supported format is newick.
##' @return a newick tree
##' @example "study_id"="pg_420", "tree_id"="522", "git_sha"="a2c48df995ddc9fd208986c3d4225112550c8452"
##' @export
gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL) {
    if (any(is.null(c(study_id, tree_id, git_sha)))) {
    	    stop("Must supply all arguments: study_id, tree_id, git_sha")
    }
    q <- list(study_id=study_id, tree_id=tree_id, git_sha=git_sha)
    
    otl_POST(path="graph/source_tree", body=q)
}


##' Summary information about a node in the graph
##'
##' node info
##' @title Get summary information about a node in the graph
##' @param ott_id The OpenTree taxonomic identifier.
##' @param node_id The idenitifer of the node in the graph.
##' @param include_lineage Whether to return the lineage of the node from the synthetic tree.
##' @return a list of information about the node
##' @example ott_id=81461
##' @export
gol_node_info <- function(node_id=NULL, ott_id=NULL, include_lineage=FALSE) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only node_id OR ott_id")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a node_id OR ott_id")
    }
    if (!is.logical(include_lineage)) {
		stop("Argument include_lineage should be logical")
	}
    if (!is.null(ott_id)) {
        q <- list(ott_id = ott_id, include_lineage=include_lineage)
    } else {
    	q <- list(node_id = node_id, include_lineage=include_lineage)
    }
    otl_POST(path="graph/node_info", body=q)
}
