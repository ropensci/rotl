
##' @title Information about the tree of life
##' @description Basic information about the tree
##' @details Summary information about the current draft tree of life,
##' including information about the list of trees and the taxonomy
##' used to build it.
##' @param study_list Boolean. Whether to return the list of source studies. Optional; default = FALSE.
##' @return A list of synthetic tree summary statistics:
##' \itemize{
##'	\item {tree_id} {The name identifier of the synthetic tree.}
##'	\item {date} {The date that the synthetic tree was constructed.}
##'	\item {taxonomy_version} {The version of the taxonomy used to initialize the graph.}
##'	\item {num_source_studies} {The number of unique source trees used in the synthetic tree.}
##'	\item {num_tips} {The number of terminal (tip) taxa in the synthetic tree.}
##'	\item {root_taxon_name} {The taxonomic name of the root node of the synthetic tree.}
##'	\item {root_node_id} {The node ID of the root node of the synthetic tree.}
##'	\item {root_ott_id} {The OpenTree Taxonomy ID (ottID) of the root node of the synthetic tree.}
##' }
##' @examples
##' res <- tol_about()
##' @author Francois Michonneau
##' @export
tol_about <- function(study_list=FALSE) {
	if (!is.logical(study_list)) {
		stop("Argument \'study_list\' should be logical")
	}
	q <- list(study_list=jsonlite::unbox(study_list))
    res <- otl_POST(path="tree_of_life/about", body=q)
    cont <- httr::content(res)
    return(cont)
}


##' @title get MRCA
##' @description Most recent common ancestor of nodes
##' @details Return the most recent common ancestor of a set of nodes in the synthetic tree.
##' @param ott_ids
##' @param node_ids
##' @return the MRCA
##' @author Francois Michonneau
##' @examples
##' test1 <- tol_mrca(ott_ids=c(412129, 536234))
##' test2 <- tol_mrca(ott_ids=c(415255), node_ids=c(341556))
##' @export
tol_mrca <- function(ott_ids=NULL, node_ids=NULL) {
    if (!is.null(node_ids) && !is.null(ott_ids)) {
        stop("Use only node_id OR ott_id")
    }
    if (is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids)
    if (!is.null(node_ids) && is.null(ott_ids)) q <- list(node_ids = node_ids)
    if (!is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    res <- otl_POST(path="tree_of_life/mrca", body=q)
    cont <- httr::content(res)

}

##' Extract subtree from a node or an OTT
##'
##' Return a complete subtree of the draft tree descended from some
##' specified node. The node to use as the start node may be specified
##' using either a node id or an ott id, but not both. If the
##' specified node is not in the synthetic tree (or is entirely absent
##' from the graph), an error will be returned.
##' @title Extract subtree
##' @return something
##' @author Francois Michonneau
##' @param node_id The node id of the node in the tree that should
##' serve as the root of the tree returned. This argument may not be
##' used in combination with ott_id.
##' @param ott_id The ott id of the node in the tree that should serve
##' as the root of the tree returned. This argument may not be used in
##' combination with node_id.
##' @param tree_id The identifier for the synthesis tree. We currently
##' only support a single draft tree in the db at a time, so this
##' argument is superfluous and may be safely ignored.
##' @return a tree of class \code{"phylo"}
##' @examples
##' res <- tol_subtree(ott_id=81461)
##' @export
tol_subtree <- function(node_id=NULL, ott_id=NULL, tree_id=NULL) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only node_id OR ott_id")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a \'node_id\' OR \'ott_id\'")
    }
    if (!is.null(tree_id)) {
        stop("\'tree_id\' is currently ignored")
    }
    if (is.null(node_id) && !is.null(ott_id)) {
        q <- list(ott_id = jsonlite::unbox(ott_id))
    }
    if (!is.null(node_id) && is.null(ott_id)) {
        q <- list(node_id = jsonlite::unbox(node_id))
    }
    res <- otl_POST(path="tree_of_life/subtree", body=q)
    cont <- httr::content(res)

    #phy <- collapse.singles(read.tree(text=(cont)[["newick"]])); # required b/c of "knuckles"
    phy <- collapse.singles(phytools::read.newick(text=(cont)[["newick"]])); # required b/c of "knuckles"

    return(phy)
}

##' Extract induced subtree
##'
##' Return a tree with tips corresponding to the nodes identified in
##' the input set(s), that is consistent with topology of the current
##' draft tree. This tree is equivalent to the minimal subtree induced
##' on the draft tree by the set of identified nodes. Any combination
##' of node ids and ott ids may be used as input. Nodes or ott ids
##' that do not correspond to any found nodes in the graph, or which
##' are in the graph but are absent from the synthetic tree, will be
##' identified in the output (but will of course not be present in the
##' resulting induced tree). Branch lengths in the result may be
##' arbitrary, and the leaf labels of the tree may either be taxonomic
##' names or (for nodes not corresponding directly to named taxa) node
##' ids.
##' @title induced subtree
##' @param node_ids Node ids indicating nodes to be used as tips in
##' the induced tree
##' @param ott_ids OTT ids indicating nodes to be used as tips in the
##' induced tree
##' @return a tree of class \code{"phylo"}
##' @author Francois Michonneau
##' @examples
##' res <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458))
##' @export
tol_induced_subtree <- function(node_ids=NULL, ott_ids=NULL) {
    if (is.null(node_ids) && is.null(ott_ids)) {
    	stop("Must supply \'node_ids\' and/or \'ott_ids\'")
    }
    if (any(is.na(node_ids)) || any(is.na(ott_ids))) {
        stop("NA are not allowed")
    }
    if (is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids  = ott_ids)
    if (!is.null(node_ids) && is.null(ott_ids)) q <- list(node_ids = node_ids)
    if (!is.null(node_ids) && !is.null(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    res <- otl_POST("tree_of_life/induced_subtree", body=q)
    cont <- httr::content(res)

    #phy <- collapse.singles(read.tree(text=(cont)[["subtree"]])); # required b/c of "knuckles"
    phy <- collapse.singles(phytools::read.newick(text=(cont)[["subtree"]])); # required b/c of "knuckles"

    return(phy)
}
