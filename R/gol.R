## *** NOTE: the development server has a newer tree graph DB ***
otl_url <- function() { "http://devapi.opentreeoflife.org" }


##' Basic information about the graph
##'
##' Returns summary information about the entire graph database, including identifiers for the
##' taxonomy and source trees used to build it.
##' @title Information about the tree of life
##' @return Some JSON
##' @examples
##' req <- gol_about()
##' @export
gol_about <- function() {
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
##' @examples
##' req <- gol_source_tree(study_id="pg_420", tree_id="522", git_sha="a2c48df995ddc9fd208986c3d4225112550c8452")
##' @export
gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL) {
    if (any(is.null(c(study_id, tree_id, git_sha)))) {
    	    stop("Must supply all arguments: \'study_id\', \'tree_id\', \'git_sha\'")
    }
    q <- list(study_id=jsonlite::unbox(study_id), tree_id=jsonlite::unbox(tree_id), 
        git_sha=jsonlite::unbox(git_sha))
    
    otl_POST(path="graph/source_tree", body=q)
}


##' Summary information about a queried node, including 1) whether it is in the graph DB,
##' 2) whether it is in the synthetic tree, 3) supporting study sources, 4) number of 
##' descendant tip taxa, etc.
##'
##' node info
##' @title Get summary information about a node in the graph
##' @param ott_id The OpenTree taxonomic identifier.
##' @param node_id The idenitifer of the node in the graph.
##' @param include_lineage Boolean. Whether to return the lineage of the node from the synthetic tree.
##' Default = FALSE.
##' @return A list of information about the node
##' @examples
##' req <- gol_node_info(ott_id=81461)
##' @export
gol_node_info <- function(node_id=NULL, ott_id=NULL, include_lineage=FALSE) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only \'node_id\' OR \'ott_id\'")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a \'node_id\' OR \'ott_id\'")
    }
    if (!is.logical(include_lineage)) {
		stop("Argument \'include_lineage\' should be logical")
	}
    if (!is.null(ott_id)) {
        q <- list(ott_id=jsonlite::unbox(ott_id), include_lineage=jsonlite::unbox(include_lineage))
    } else {
    	    q <- list(node_id=jsonlite::unbox(node_id), include_lineage=jsonlite::unbox(include_lineage))
    }
    otl_POST(path="graph/node_info", body=q)
}
