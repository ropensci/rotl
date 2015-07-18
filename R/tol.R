
##' Basic information about the Open Tree of Life (the synthetic tree)
##'
##' @title Information about the Tree of Life
##'
##' @details Summary information about the current draft tree of life,
##'     including information about the list of trees and the taxonomy
##'     used to build it.
##'
##' @param study_list Whether to return the list of source
##'     studies. (Logical, default = FALSE).
##'
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##'
##' @return An invisible list of synthetic tree summary statistics:
##'
##' \itemize{
##'
##'     \item {tree_id} {The name identifier of the synthetic tree.}
##'
##'     \item {date} {The date that the synthetic tree was
##'     constructed.}
##'
##'     \item {taxonomy_version} {The version of the taxonomy used to
##'     initialize the graph.}
##'
##'     \item {num_source_studies} {The number of unique source trees
##'     used in the synthetic tree.}
##'
##'     \item {num_tips} {The number of terminal (tip) taxa in the
##'     synthetic tree.}
##'
##'     \item {root_taxon_name} {The taxonomic name of the root node
##'     of the synthetic tree.}
##'
##'     \item {root_node_id} {The node ID of the root node of the
##'     synthetic tree.}
##'
##'     \item {root_ott_id} {The OpenTree Taxonomy ID (ottID) of the
##'     root node of the synthetic tree.}  }
##' @seealso \code{\link{study_list}} to explore the list of studies
##'     used in the synthetic tree.
##' @examples
##' \dontrun{
##' res <- tol_about()
##' studies <- study_list(tol_about(study_list=TRUE))
##' }
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

##' Retrieve the detailed information for the list of studies used in
##' the Tree of Life.
##'
##' This function takes the object resulting from
##' \code{tol_about(study_list = TRUE)} and returns a data frame
##' listing the \code{tree_id}, \code{study_id} and \code{git_sha} for
##' the studies currently included in the Tree of Life.
##'
##' @title List of studies used for the Tree of Life
##' @param tol an object created using \code{tol_about(study_list = TRUE)}
##' @return a data frame
##' @export
study_list <- function(tol) UseMethod("study_list")

##' @export
##' @rdname study_list
study_list.tol_summary <- function(tol) {
    if (! exists("study_list", tol)) {
        stop("Make sure that your object has been created using ",
             sQuote("tol_about(study_list = TRUE)"))
    }
    tol <- lapply(tol[["study_list"]], function(x) {
        c("tree_id" = x[["tree_id"]],
          "study_id" = x[["study_id"]],
          "git_sha" = x[["git_sha"]])
    })
    tol <- do.call("rbind", tol)
    data.frame(tol, stringsAsFactors = FALSE)
}

##' Most Recent Common Ancestor for a set of nodes
##'
##' Get the MRCA of a set of nodes on the current synthetic
##' tree. Accepts any combination of node ids and ott ids as
##' input. Returns information about the most recent common ancestor
##' (MRCA) node as well as the most recent taxonomic ancestor (MRTA)
##' node (the closest taxonomic node to the MRCA node in the synthetic
##' tree; the MRCA and MRTA may be the same node). Node ids that are
##' not in the synthetic tree are dropped from the MRCA
##' calculation. For a valid ott id that is not in the synthetic tree
##' (i.e. it is not recovered as monophyletic from the source tree
##' information), the taxonomic descendants of the node are used in
##' the MRCA calculation. Returns any unmatched node ids / ott ids.
##'
##' @title MRCA of taxa from the synthetic tree
##' @details Return the most recent common ancestor of a set of nodes
##'     in the synthetic tree.
##' @param ott_ids the ott ids for which the MRCA is desired
##'     (character or numeric vector)
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##' @return A list
##' @examples
##' \dontrun{
##' birds_mrca <- tol_mrca(ott_ids=c(412129, 536234))
##' }
##' @export
tol_mrca <- function(ott_ids=NULL, ...) {
    res <- .tol_mrca(ott_ids = ott_ids, ...)
    return(res)
}

##' Extract a subtree from the synthetic tree from an ott id.
##'
##'
##' @title Extract a subtree from the synthetic tree
##'
##' @param ott_id the ott id of the node in the tree that should serve
##'     as the root of the tree returned.
##' @param tree_id the identifier for the synthesis tree. Currently a
##'     single draft tree is supported, so this argument is
##'     superfluous and may be safely ignored.
##' @param file if specified, the function will write the subtree to a
##'     file in newick format.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##' @details Return a complete subtree of the draft tree descended
##'     from some specified node. The node to use as the start node
##'     may be specified using an ott id. If the specified node is not
##'     in the synthetic tree (or is entirely absent from the graph),
##'     an error will be returned.
##' @return If no value is specified to the \code{file} argument
##'     (default), a phyogenetic tree of class \code{phylo}.
##'
##'     Otherwise, the function returns invisibly a logical indicating
##'     whether the file was successfully created.
##' @examples
##'    \dontrun{
##'       res <- tol_subtree(ott_id=81461)
##'     }
##' @export

tol_subtree <- function(ott_id = NULL, tree_id = NULL, file, ...) {

    res <- .tol_subtree(ott_id = ott_id, tree_id = tree_id, ...)

    if (!missing(file)) {
        unlink(file)
        cat(res$newick, file = file)
        return(invisible(file.exists(file)))
    } else {
        phy <- phylo_from_otl(res)
        return(phy)
    }
}

##' Extract a subtree based on a vector of ott ids.
##'
##' Return a tree with tips corresponding to the nodes identified in
##' the input set that is consistent with the topology of the current
##' synthetic tree. This tree is equivalent to the minimal subtree
##' induced on the draft tree by the set of identified nodes. Ott ids
##' that do not correspond to any nodes found in the graph, or which
##' are in the graph but are absent from the synthetic tree, will be
##' identified in the output (but obvisouly will be absent from the
##' resulting induced tree). Branch lengths in the result may be
##' arbitrary, and the tip labels of the tree may either be taxonomic
##' names or (for nodes not corresponding directly to named taxa) node
##' ids.
##'
##' @title induced subtree
##' @param ott_ids OTT ids indicating nodes to be used as tips in the
##'     induced tree
##' @param file if specified, the function will write the subtree to a
##'     file in newick format.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##' @return If no value is specified to the \code{file} argument
##'     (default), a phyogenetic tree of class \code{phylo}.
##'
##'     Otherwise, the function returns invisibly a logical indicating
##'     whether the file was successfully created.
##' @examples
##' \dontrun{
##' res <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458))
##' tree_file <- tempfile(fileext=".tre")
##' tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458),
##'                     file=tree_file)
##' }
##' @export

tol_induced_subtree <- function(ott_ids=NULL, file, ...) {
    res <- .tol_induced_subtree(ott_ids = ott_ids, ...)

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
        cat(res$newick, file = file)
        return(file.exists(file))
    } else {
        phy <- phylo_from_otl(res)
        return(phy)
    }
}
