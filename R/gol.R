##' Graph of Life
##'
##' Basic information about the graph
##'
##' @title Information about the graph of life
##' @details Returns summary information about the entire graph
##'     database, including identifiers for the taxonomy and source
##'     trees used to build it.
##' @param ... additional arguments to customize the API call (see
##'     ?rotl for more information)
##' @return An invisible list of graph attributes:
##'     \itemize{
##'
##'     \item {graph_num_source_trees} {The number of unique source
##'     trees in the graph.}
##'
##'     \item {graph_taxonomy_version} {The version of the taxonomy
##'     used to initialize the graph.}
##'
##'     \item {graph_num_tips} {The number of terminal (tip) taxa in
##'     the graph.}
##'
##'     \item {graph_root_name} {The taxonomic name of the root node
##'     of the graph.}
##'
##'     \item {graph_root_node_id} {The node ID of the root node of
##'     the graph.}
##'
##'     \item {graph_root_ott_id} {The OpenTree Taxonomy ID (ottID) of
##'     the root node of the graph.}
##' }
##' @examples
##'   \dontrun{
##'     res <- gol_about()
##'   }
##' @export
gol_about <- function(...) {
    res <- structure(.gol_about(...), class = "gol")
    res
}


##' @export
print.gol <- function(x, ...) {
    cat("\nOpenTree Synthetic Tree Graph.\n\n")
    cat("\tTaxonomy version: ", x$graph_taxonomy_version, "\n", sep="")
    cat("\tNumber of terminal taxa: ", x$graph_num_tips, "\n", sep="")
    cat("\tNumber of source trees: ", x$graph_num_source_trees, "\n", sep="")
    cat("\tGraph root taxon: ", x$graph_root_name, "\n", sep="")
    cat("\tGraph root ott_id: ", x$graph_root_ott_id, "\n", sep="")
    cat("\tGraph root node_id: ", x$graph_root_node_id, "\n", sep="")
}



## Returns a reconstructed source tree from the graph database.
##
## Reconstructs a source tree given identifiers:
## \code{study_id}, \code{tree_id}, and \code{git_sha}.
##
## The tree may differ from the original source tree in 2 ways: 1) it
## may contain fewer taxa (as duplicate taxa are pruned on tree
## ingestion), and 2) OpenTree Taxonomy IDs (ottIDs) are applied to
## all named internal nodes and as a suffix to all terminal node
## names.
##
## @title Get a reconstructed source tree
## @param study_id String. The study identifier. Will typically
##     include a prefix ("pg_" or "ot_").
## @param tree_id String. The tree identifier for a given study.
## @param git_sha String. The git SHA identifying a particular source
##     version.
## @param ... additional arguments to customize the API call (see
##     \code{\link{rotl}} for more information)
## @return a tree of class \code{"phylo"}
## @examples
##\dontrun{
## ## This example is broken as it returns a single taxa that rncl can't deal with
## res <- gol_source_tree(study_id="pg_420", git_sha="a2c48df995ddc9fd208986c3d4225112550c8452",
##                        tree_id="522")
##}
## @export ## not exported for now as users probably don't care/need it
## gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL, ...) {
##     res <- .gol_source_tree(study_id = study_id, tree_id = tree_id,
##                             git_sha = git_sha, ...)
##     phy <- phylo_from_otl(res)
##     return(phy)
## }


##' Get summary information about a node in the graph database
##'
##' @title Node info
##' @details Summary information about a queried node, including 1)
##'     whether it is in the graph database, 2) whether it is in the
##'     synthetic tree, 3) supporting study sources, 4) number of
##'     descendant tip taxa, 5) graph node ID, and 6) taxonomic
##'     information (if it is a named node in the graph), including:
##'     name, rank, OpenTree Taxonomy ID (ottID), and source taxonomy
##'     IDs.
##' @param ott_id The OpenTree taxonomic identifier.
##' @param include_lineage Boolean. Whether to return the lineage of
##'     the node from the synthetic tree. Optional; default = FALSE.
##' @param ... additional arguments to customize the API call (see
##'     ?rotl for more information)
##' @return \code{gol_node_info} returns a list of summary information
##'     about the queried node.
##'
##' \itemize{
##'
##'     \item {tree_id} {The tree identifier for a given study.}
##'
##'     \item {num_synth_tips} {Numeric. The number of synthetic
##'            tree tip descendants.}
##'
##'     \item {name} {String. The taxonomic name of the queried node
##'        (if it is a named node).}
##'
##'     \item {rank} {String. The taxonomic rank of the queried node
##'     (if it is a named node).}
##'
##'    \item {ott_id} {Numeric. The OpenTree Taxonomy ID (ottID) of
##'     the queried node (if it is a named node).}
##'
##'    \item {num_tips} {Numeric. The number of taxonomic tip
##'     descendants.}
##'
##'     \item {tree_sources} {A list of supporting source
##'     trees in the graph. May differ from \code{"synth_sources"}, if
##'     trees are in the graph, but were not used in constructing the
##'     synthetic tree. Each source has:}
##'
##'     \itemize{
##'
##'         \item {study_id} {The study identifier. Will typically include
##'         a prefix ("pg_" or "ot_").}
##'
##'         \item {tree_id} {The tree identifier for a given study.}
##'
##'         \item {git_sha} {The git SHA identifying a particular source
##'     version.}
##'
##'     }
##'
##'	\item {tax_source} {String. Source taxonomy IDs (if it is a
##'     named node), e.g. "ncbi:9242,gbif:5289,irmng:104628".}
##'
##'    \item {synth_sources} {A list of supporting synthesis source
##'     trees, each with:}
##'
##'        \itemize{
##'
##'           \item {git_sha} {The git SHA identifying a particular source
##'            version.}
##'
##'           \item {tree_id} {The tree id associated with the study id used.}
##'
##'          \item {study_id} {The study identifier. Will typically include
##'          a prefix ("pg_" or "ot_").}
##'
##'
##'        }
##'
##'    \item {node_id} { The identifier for the node used by the neo4j
##'       database. These identifiers are not persistent and shouldn't be
##'       used.}
##'
##'   \item {in_synth_tree} {Boolean. Whether the \code{ott_id} is
##'       included in the synthetic tree.}
##'
##'
##' }
##'
##'     \code{tax_rank} and \code{ott_id} return vectors (character,
##'     and numeric respectively).
##'
##' @examples
##' \dontrun{
##' birds <- gol_node_info(ott_id=81461)
##' synth_sources(birds)
##' tax_rank(birds)
##' ott_id(birds)
##' }
##' @export
gol_node_info <- function(ott_id=NULL, include_lineage=FALSE, ...) {
    res <- .gol_node_info(ott_id = ott_id, include_lineage = include_lineage,
                          ...)
    class(res) <- "gol_node"
    return(res)
}


##' @export
##' @param tax an object returned by \code{gol_node_info}.
##' @rdname gol_node_info
tax_rank.gol_node <- function(tax) {
    tax[["rank"]]
}

##' @export
##' @rdname gol_node_info
ott_id.gol_node <- function(tax, ...) {
    tax[["ott_id"]]
}

##' @export
##' @rdname gol_node_info
synth_sources <- function(tax) UseMethod("synth_sources")


##' @export
##' @rdname gol_node_info
synth_sources.gol_node <- function(tax) {
    tt <- lapply(tax$synth_sources, function(x) {
        c(x["study_id"], x["tree_id"], x["git_sha"])
    })
    tt <- do.call("rbind", tt)
    as.data.frame(tt, stringsAsFactors = FALSE)
}
