##' Basic information about the tree
##'
##' Summary information about the current draft tree of life,
##' including information about the list of trees and the taxonomy
##' used to build it.
##' @title Information about the tree of life
##' @param study_list Boolean. Whether to return the list of source studies. Default = FALSE.
##' @return Some JSON
##' @author Francois Michonneau
##' @export
tol_about <- function(study_list=FALSE) {
	if (!is.logical(study_list)) {
		stop("Argument \'study_list\' should be logical")
	}
	q <- list(study_list=jsonlite::unbox(study_list))
    otl_POST(path="tree_of_life/about", body=q)
}

##' Reurns the MRCA
##'
##' Most recent common ancestor
##' @title get MRCA
##' @param ott_ids
##' @param node_ids
##' @return the MRCA
##' @author Francois Michonneau
##' @examples
##' test1 <- tol_mrca(ott_ids=list("ott_ids" = c(412129, 536234)))
##' test2 <- tol_mrca(ott_ids=list("ott_ids" = c(415255)), node_ids=c(341556))
##' @export
tol_mrca <- function(ott_ids, node_ids) {
    if (missing(node_ids) && !missing(ott_ids)) q <- list(ott_ids = ott_ids)
    if (!missing(node_ids) && missing(ott_ids)) q <- list(node_ids = node_ids)
    if (!missing(node_ids) && !missing(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    otl_POST(path="tree_of_life/mrca", body=q)
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
##' @export
tol_subtree <- function(node_id, ott_id, tree_id) {
    if (!missing(node_id) && !missing(ott_id)) {
        stop("Use only node_id OR ott_id")
    }
    if (!missing(tree_id)) {
        stop("tree_id is currently ignored")
    }
    if (missing(node_id) && !missing(ott_id)) {
        q <- list(ott_id = ott_id)
    }
    if (!missing(node_id) && missing(ott_id)) {
        q <- list(node_id = node_id)
    }
    otl_POST(path="tree_of_life/subtree", body=q)
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
##' @return something
##' @author Francois Michonneau
##' @examples
##' {"ott_ids":[292466, 501678, 267845, 666104, 316878, 102710, 176458]}
##' @export
tol_induced_subtree <- function(node_ids, ott_ids) {
    if (missing(node_ids) && !missing(ott_ids)) q <- list(ott_ids  = ott_ids)
    if (!missing(node_ids) && missing(ott_ids)) q <- list(node_ids = node_ids)
    if (!missing(node_ids) && !missing(ott_ids)) q <- list(ott_ids = ott_ids,
                                                           node_ids = node_ids)
    otl_POST("tree_of_life/induced_subtree", body=q)
}
