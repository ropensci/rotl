##' Graph of Life
##'
##' Basic information about the graph
##'
##' @title Information about the graph of life
##' @details Returns summary information about the entire graph
##' database, including identifiers for the taxonomy and source trees
##' used to build it.
##' @return An invisible list of graph attributes:
##' \itemize{
##'	\item {graph_num_source_trees} {The number of unique source trees in the graph.}
##'	\item {graph_taxonomy_version} {The version of the taxonomy used to initialize the graph.}
##'	\item {graph_num_tips} {The number of terminal (tip) taxa in the graph.}
##'	\item {graph_root_name} {The taxonomic name of the root node of the graph.}
##'	\item {graph_root_node_id} {The node ID of the root node of the graph.}
##'	\item {graph_root_ott_id} {The OpenTree Taxonomy ID (ottID) of the root node of the graph.}
##' }
##' @examples
##' res <- gol_about()
##' @export
gol_about <- function() {
    res <- structure(.gol_about(), class = "gol")
    res
}


print.gol <- function(res) {
    cat("\nOpenTree Synthetic Tree Graph.\n\n")
    cat("\tTaxonomy version: ", res$graph_taxonomy_version, "\n", sep="")
    cat("\tNumber of terminal taxa: ", res$graph_num_tips, "\n", sep="")
    cat("\tNumber of source trees: ", res$graph_num_source_trees, "\n", sep="")
    cat("\tGraph root taxon: ", res$graph_root_name, "\n", sep="")
    cat("\tGraph root ott_id: ", res$graph_root_ott_id, "\n", sep="")
    cat("\tGraph root node_id: ", res$graph_root_node_id, "\n", sep="")
}



##' Returns a reconstructed source tree from the graph database.
##'
##' @title Get reconstructed source tree
##' @details Reconstructs a source tree given identifiers:
##' \code{study_id}, \code{tree_id}, and \code{git_sha}.
##'
##' The tree may differ from the original source tree in 2 ways: 1) it
##' may contain fewer taxa (as duplicate taxa are pruned on tree
##' ingestion), and 2) OpenTree Taxonomy IDs (ottIDs) are applied to
##' all named internal nodes and as a suffix to all terminal node
##' names.
##' @param study_id String. The study identifier. Will typically
##' include a prefix ("pg_" or "ot_").
##' @param tree_id String. The tree identifier for a given study.
##' @param git_sha String. The git SHA identifying a particular source
##' version.
##' @param format The name of the return format. The only currently
##' supported format is newick.
##' @param parser The newick parser to use. Defaults to
##' \code{"rncl"}. The alternative is \code{"phytools"}. \code{"rncl"}
##' is faster.
##' @return a tree of class \code{"phylo"}
##' @examples
##'\dontrun{
##' res <- gol_source_tree(study_id="ot_121", git_sha="a2c48df995ddc9fd208986c3d4225112550c8452", tree_id="7")
##'}
## @export ## not exported for now as users probably don't care/need it
gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL, parser="rncl") {
    res <- .gol_source_tree(study_id, tree_id, git_sha)
    # required b/c of "knuckles"
    phy <- phylo_from_otl(res, parser)
    return(phy)
}


##' Get summary information about a node in the graph
##'
##' @title Node info
##' @details Summary information about a queried node, including 1) whether it is in the graph DB,
##' 2) whether it is in the synthetic tree, 3) supporting study sources, 4) number of
##' descendant tip taxa, 5) graph node ID, and 6) taxonomic information (if it is a named
##' node in the graph), including: name, rank, OpenTree Taxonomy ID (ottID), and source taxonomy
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
##'	\item {ott_id} {Numeric. The OpenTree Taxonomy ID (ottID) of the queried node (if it is a named node).}
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
    res <- .gol_node_info(node_id, ott_id, include_lineage)
    return(res)
}
