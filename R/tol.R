
##' @title Information about the tree of life
##' @description Basic information about the tree
##' @details Summary information about the current draft tree of life,
##' including information about the list of trees and the taxonomy
##' used to build it.
##' @param study_list Boolean. Whether to return the list of source studies. Optional; default = FALSE.
##' @return An invisible list of synthetic tree summary statistics:
##' \itemize{
##'    \item {tree_id} {The name identifier of the synthetic tree.}
##'    \item {date} {The date that the synthetic tree was constructed.}
##'    \item {taxonomy_version} {The version of the taxonomy used to initialize the graph.}
##'    \item {num_source_studies} {The number of unique source trees used in the synthetic tree.}
##'    \item {num_tips} {The number of terminal (tip) taxa in the synthetic tree.}
##'    \item {root_taxon_name} {The taxonomic name of the root node of the synthetic tree.}
##'    \item {root_node_id} {The node ID of the root node of the synthetic tree.}
##'    \item {root_ott_id} {The OpenTree Taxonomy ID (ottID) of the root node of the synthetic tree.}
##' }
##' @examples
##' res <- tol_about()
##' @author Francois Michonneau
##' @export
tol_about <- function(study_list = FALSE, ...) {
    res <- .tol_about(study_list = study_list, ...)
    class(res) <- "tol_summary"
    res
}


##' @export
print.tol_summary <- function(x, ...) {
    cat("\nOpenTree Synthetic Tree of Life.\n\n")
    cat("\tTree version: ", x$tree_id, "\n", sep="")
    cat("\tTaxonomy version: ", x$taxonomy_version, "\n", sep="")
    cat("\tConstructed: ", x$date, "\n", sep="")
    cat("\tNumber of terminal taxa: ", x$num_tips, "\n", sep="")
    cat("\tNumber of source trees: ", x$num_source_studies, "\n", sep="")
    cat("\tSource list present: ", ifelse(exists("study_list", x), "true", "false"), "\n", sep="")
    cat("\tRoot taxon: ", x$root_taxon_name, "\n", sep="")
    cat("\tRoot ott_id: ", x$root_ott_id, "\n", sep="")
    cat("\tRoot node_id: ", x$root_node_id, "\n", sep="")
}

##' Retrieve the detailed information for the list of studies used in the Tree of Life.
##'
##' This function takes the object resulting from
##' `tol_about(study_list = TRUE)` and returns a data frame listing
##' the \code{tree_id}, \code{study_id} and \code{git_sha} for the
##' studies currently included in the Tree of Life.
##' @title List of studies used for the Tree of Life
##' @param tol an object created using \code{tol_about(study_list = TRUE)}
##' @return a data frame
##' @author Francois Michonneau
##' @export
study_list <- function(tol) UseMethod("study_list")

##' @export
##' @rdname study_list
study_list.tol_summary <- function(tol) {
    if (! exists("study_list", tol)) {
        stop("Make sure that your object has been created using tol_about(study_list = TRUE)")
    }
    tol <- lapply(tol[["study_list"]], function(x) {
        c("tree_id" = x[["tree_id"]],
          "study_id" = x[["study_id"]],
          "git_sha" = x[["git_sha"]])
    })
    tol <- do.call("rbind", tol)
    data.frame(tol, stringsAsFactors = FALSE)
}

##' @title get MRCA
##' @description Most recent common ancestor of nodes Get the MRCA of
##' a set of nodes on the current draft tree. Accepts any combination
##' of node ids and ott ids as input. Returns information about the
##' most recent common ancestor (MRCA) node as well as the most recent
##' taxonomic ancestor (MRTA) node (the closest taxonomic node to the
##' MRCA node in the synthetic tree; the MRCA and MRTA may be the same
##' node). Node ids that are not in the synthetic tree are dropped
##' from the MRCA calculation. For a valid ott id that is not in the
##' synthetic tree (i.e. it is not recovered as monophyletic from the
##' source tree information), the taxonomic descendants of the node
##' are used in the MRCA calculation. Returns any unmatched node ids /
##' ott ids.
##' @details Return the most recent common ancestor of a set of nodes
##' in the synthetic tree.
##' @param ott_ids
##' @param node_ids
##' @return the MRCA
##' @author Francois Michonneau
##' @examples
##' test1 <- tol_mrca(ott_ids=c(412129, 536234))
##' @export
tol_mrca <- function(ott_ids=NULL, node_ids=NULL, ...) {
    res <- .tol_mrca(ott_ids = ott_ids, node_ids = node_ids, ...)
    return(res)
}

##' @title Extract subtree
##' @description Extract subtree from a node or ott id
##' @author Francois Michonneau
##' @param node_id The node id of the node in the tree that should
##'     serve as the root of the tree returned. This argument cannot
##'     be used in combination with ott_id.
##' @param ott_id The ott id of the node in the tree that should serve
##'     as the root of the tree returned. This argument may not be
##'     used in combination with node_id.
##' @param tree_id The identifier for the synthesis tree. We currently
##'     only support a single draft tree in the db at a time, so this
##'     argument is superfluous and may be safely ignored.
##' @details Return a complete subtree of the draft tree descended
##'     from some specified node. The node to use as the start node
##'     may be specified using either a node id or an ott id, but not
##'     both. If the specified node is not in the synthetic tree (or
##'     is entirely absent from the graph), an error will be returned.
##' @return a tree of class \code{"phylo"}
##' @examples
##'\dontrun{
##' res <- tol_subtree(ott_id=81461)
##'}
##' @export
tol_subtree <- function(node_id = NULL, ott_id = NULL, tree_id = NULL,
                        file, ...) {

    res <- .tol_subtree(node_id = node_id, ott_id = ott_id,
                        tree_id = tree_id, ...)

    if (!missing(file)) {
        unlink(file)
        cat(res$newick, file = file)
        return(invisible(file.exists(file)))
    } else {
        phy <- phylo_from_otl(res)
        return(phy)
    }
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
##' @param file If NULL the function returns a phylo object (default),
##' otherwise, the function writes the newick string to the specified
##' file.
##' @return a tree of class \code{"phylo"}
##' @author Francois Michonneau
##' @examples
##' \dontrun{
##' res <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458))
##' }
##' @export
tol_induced_subtree <- function(node_ids=NULL, ott_ids=NULL, file, ...) {
    res <- .tol_induced_subtree(node_ids = node_ids, ott_ids = ott_ids, ...)

    if (length(res$node_ids_not_in_graph) > 0) {
        warning("node ids: ", paste0(res$node_ids_not_in_graph, collapse = ", "), " not in graph.")
    }
    if (length(res$ott_ids_not_in_tree) >  0) {
        warning("ott ids: ", paste0(res$ott_ids_not_in_tree, collaspse = ", "),  " not in tree.")
    }
    if (length(res$ott_ids_not_in_graph) > 0) {
        warning("ott ids: ", paste0(res$ott_ids_not_in_graph, collapse = ", "), " not in graph.")
    }
    if (!missing(file)) {
        unlink(file)
        cat(res$subtree, file = file)
        return(file.exists(file))
    } else {
        phy <- phylo_from_otl(res)
        return(phy)
    }
}
