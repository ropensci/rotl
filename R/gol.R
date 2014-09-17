## *** NOTE: the development server has a newer tree graph DB (JWB) ***
otl_url <- function() { "http://devapi.opentreeoflife.org" }


##' @title Information about the graph of life
##' @description Basic information about the graph
##' @details Returns summary information about the entire graph database, including identifiers for the taxonomy and source trees used to build it.
##' @return A list of graph attributes:
##' \itemize{
##'	\item {graph_num_source_trees} {The number of unique source trees in the graph.}
##'	\item {graph_taxonomy_version} {The version of the taxonomy used to initialize the graph.}
##'	\item {graph_num_tips} {The number of terminal (tip) taxa in the graph.}
##'	\item {graph_root_name} {The taxonomic name of the root node of the graph.}
##'	\item {graph_root_node_id} {The node ID of the root node of the graph.}
##'	\item {graph_root_ott_id} {The OpenTree ID (ottID) of the root node of the graph.}
##' }
##' @examples
##' res <- gol_about()
##' @export
gol_about <- function() {
    res <- otl_POST(path="graph/about", body=list())
    cont <- httr::content(res)
    if (length(cont) < 1) {
        warning("Nothing returned")
    }
    return(cont)
}



## TODO: get a smaller example tree ***


##' @title Get reconstructed source tree
##' @description Returns a reconstructed source tree from the graph DB.
##' @details Reconstructs a source tree given identifiers: \code{study_id}, \code{tree_id}, and \code{git_sha}. The tree may differ from the original source tree in 2 ways: 1) it may contain fewer taxa (as duplicate taxa are pruned on tree ingestion), and 2) OpenTree Taxonomy (OTT) IDs are applied to all named internal nodes and as a suffix to all terminal node names.
##' @param study_id The study identifier. Will typically include a prefix ("pg_" or "ot_").
##' @param tree_id The tree identifier for a given study.
##' @param git_sha The git SHA identifying a particular source version.
##' @param format The name of the return format. The only currently supported format is newick.
##' @return a tree of class \code{"synth_sources"}
##' @examples
##' res <- gol_source_tree(study_id="pg_420", tree_id="522", git_sha="a2c48df995ddc9fd208986c3d4225112550c8452")
##' @export
gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL) {
    if (any(is.null(c(study_id, tree_id, git_sha)))) {
    	    stop("Must supply all arguments: \'study_id\', \'tree_id\', \'git_sha\'")
    }
    q <- list(study_id=jsonlite::unbox(study_id), tree_id=jsonlite::unbox(tree_id), 
        git_sha=jsonlite::unbox(git_sha))
    
    res <- otl_POST(path="graph/source_tree", body=q)
    cont <- httr::content(res)
    phy <- collapse.singles(phytools::read.newick(text=(cont)[["newick"]])); # required b/c of "knuckles"
    return(phy)
}


##' @title Node info
##' @description Get summary information about a node in the graph
##' @details Summary information about a queried node, including 1) whether it is in the graph DB,
##' 2) whether it is in the synthetic tree, 3) supporting study sources, 4) number of 
##' descendant tip taxa, 5) graph node ID, and 6) taxonomic information (if it is a named
##' node in the graph), including: name, rank, OpenTree Taxonomy (OTT) ID, and source taxonomy
##' IDs.
##' @param ott_id The OpenTree taxonomic identifier.
##' @param node_id The idenitifer of the node in the graph.
##' @param include_lineage Boolean. Whether to return the lineage of the node from the synthetic tree. Optional; default = FALSE.
##' @return A list of summary information about the queried node.
##' \itemize{
##'	\item {in_graph} {Boolean. Whether the queried node is present in the graph.}
##'	\item {node_id} {Numeric. The node ID of the queried node in the graph.}
##'	\item {in_synth_tree} {Boolean. Whether the queried node is present in the synthetic tree.}
##'	\item {rank} {String. The taxonomic rank of the queried node (if it is a named node).}
##'	\item {name} {String. The taxonomic name of the queried node (if it is a named node).}
##'	\item {ott_id} {Numeric. The OpenTree Taxonomy (OTT) ID of the queried node (if it is a named node).}
##'	\item {num_tips} {Numeric. The number of taxonomic tip descendants.}
##'	\item {num_synth_children} {Numeric . The number of synthetic tree tip descendants.}
##'	\item {tax_source} {String. Source taxonomy IDs (if it is a named node), e.g. "ncbi:9242,gbif:5289,irmng:104628".}
##'	\item {synth_sources} {A list of supporting synthesis source trees, each with:}
##' \itemize{
##' \item {study_id} {The study identifier. Will typically include a prefix ("pg_" or "ot_").}
##' \item {tree_id} {The tree identifier for a given study.}
##' \item {git_sha} {The git SHA identifying a particular source version.}
##' }
##'	\item {tree_sources} {A list of supporting source trees in the graph. May differ from \code{"synth_sources"}, if trees are in the graph, but were not used in constructing the synthetic tree. Each source has:}
##' \itemize{
##' \item {study_id} {The study identifier. Will typically include a prefix ("pg_" or "ot_").}
##' \item {tree_id} {The tree identifier for a given study.}
##' \item {git_sha} {The git SHA identifying a particular source version.}
##' }
##' }
##' @examples
##' res <- gol_node_info(ott_id=81461)
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
    res <- otl_POST(path="graph/node_info", body=q)
    cont <- httr::content(res)
    return(cont)
}


