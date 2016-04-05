
##' Basic information about the Open Tree of Life (the synthetic tree)
##'
##' @title Information about the Tree of Life
##'
##' @details Summary information about the current draft tree of life,
##'     including information about the list of trees and the taxonomy
##'     used to build it.
##'
##' @param source_list Whether to return the list of source
##'     trees. (Logical, default = FALSE).
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
tol_about <- function(source_list = FALSE, ...) {
    res <- .tol_about(source_list = source_list, ...)
    class(res) <- "tol_summary"
    res
}


##' @export
print.tol_summary <- function(x, ...) {
    cat("\nOpenTree Synthetic Tree of Life.\n\n")
    cat("\tTree version: ", x$synth_id, "\n", sep="")
    cat("\tTaxonomy version: ", x$taxonomy_version, "\n", sep="")
    cat("\tConstructed on: ", x$date_created, "\n", sep="")
    cat("\tNumber of terminal taxa: ", x$root$num_tips, "\n", sep="")
    cat("\tNumber of source trees: ", x$num_source_trees, "\n", sep="")
    cat("\tNumber of source studies: ", x$num_source_studies, "\n", sep = "")
    cat("\tSource list present: ", ifelse(exists("source_list", x), "true", "false"), "\n", sep="")
    cat("\tRoot taxon: ", x$root$taxon$name, "\n", sep="")
    cat("\tRoot ott_id: ", x$root$taxon$ott_id, "\n", sep="")
    cat("\tRoot node_id: ", x$root$node_id, "\n", sep="")
}

##' Retrieve the detailed information for the list of studies used in
##' the Tree of Life.
##'
##' This function takes the object resulting from
##' \code{tol_about(study_list = TRUE)} and returns a data frame
##' listing the \code{tree_id}, \code{study_id} and \code{git_sha} for
##' the studies currently included in the Tree of Life.
##'
##' @title List of studies used in the Tree of Life
##' @param tol an object created using \code{tol_about(study_list = TRUE)}
##' @return a data frame
##' @export
study_list <- function(tol) UseMethod("study_list")

##' @export
##' @rdname study_list
study_list.tol_summary <- function(tol) {
    if (! exists("source_list", tol)) {
        stop("Make sure that your object has been created using ",
             sQuote("tol_about(study_list = TRUE)"))
    }
    tol <- lapply(tol[["source_id_map"]], function(x) {
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






##' Get summary information about a node in the synthetic tree
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
##' @return \code{tol_node_info} returns a list of summary information
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
##' birds <- tol_node_info(ott_id=81461)
##' synth_sources(birds)
##' tax_rank(birds)
##' ott_id(birds)
##' }
##' @export
tol_node_info <- function(ott_id=NULL, include_lineage=FALSE, ...) {
    res <- .tol_node_info(ott_id = ott_id, include_lineage = include_lineage,
                          ...)
    class(res) <- "tol_node"
    return(res)
}


##' @export
##' @param tax an object returned by \code{tol_node_info}.
##' @rdname tol_node_info
tax_rank.tol_node <- function(tax) {
    tax[["rank"]]
}

##' @export
##' @rdname tol_node_info
ott_id.tol_node <- function(tax, ...) {
    tax[["ott_id"]]
}

##' @export
##' @rdname tol_node_info
synth_sources <- function(tax) UseMethod("synth_sources")


##' @export
##' @rdname tol_node_info
synth_sources.tol_node <- function(tax) {
    tt <- lapply(tax$synth_sources, function(x) {
        c(x["study_id"], x["tree_id"], x["git_sha"])
    })
    tt <- do.call("rbind", tt)
    as.data.frame(tt, stringsAsFactors = FALSE)
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
##' are in the graph but are absent from the synthetic tree (e.g. groups
##' included in the Open Tree Taxonomy but found to be paraphyletic in 
##' in studies contributing to the synthetic tree) , will be
##' identified in the output (but obvisouly will be absent from the
##' resulting induced tree). Branch lengths in the result may be
##' arbitrary, and the tip labels of the tree may either be taxonomic
##' names or (for nodes not corresponding directly to named taxa) node
##' ids.
##'
##' @title Subtree from the Open Tree of Life
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


##' Strip OTT ids from tip labels
##' @param tip_labels a character vector containing tip labels (most likely 
##'     the \code{tip.label} element from a tree returned by
##'     \code{\link{tol_induced_subtree}}
##' @return A character vector containing the contents of \code{tip_labels}
##'     with any OTT ids removed.
##' @examples
##' \dontrun{
##' genera <- c("Perdix", "Clangula", "Dendroica", "Cinclus", "Stellula", "Struthio")
##' tr <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458))
##' tr$tip.label %in% genera
##' tr$tip.label <- strip_ott_ids(tr$tip.label)
##' tr$tip.label %in% genera
##'}
##'@export
strip_ott_ids <- function(tip_labels){
    sub("_ott\\d+$", "", tip_labels)
}
