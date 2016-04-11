
##' Basic information about the Open Tree of Life (the synthetic tree)
##'
##' @title Information about the Tree of Life
##'
##' @details Summary information about the current draft tree of life,
##'     including information about the list of trees and the taxonomy
##'     used to build it.
##'
##' @param include_source_list Logical (default = FALSE). Return an
##'     ordered list of source trees.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##'
##' @return An invisible list of synthetic tree summary statistics:
##'
##' \itemize{
##'
##'     \item {date_created} {String. The creation date of the tree.}
##'
##'     \item {num_source_studies} {Integer. The number of studies
##'     (publications)used as sources.}
##'
##'     \item {num_source_trees} {The number of trees used as sources (may
##'     be >1 tree per study).}
##'
##'     \item {taxonomy} {The Open Tree Taxonomy version used as a source.}
##'
##'     \item {filtered_flags} {List. Taxa with these taxonomy flags were
##'     not used in construction of the tree.}
##'
##'     \item {root} {List. Describes the root node:}
##'         \itemize{
##'             \item {node_id} {String. The canonical identifier of the node.}
##'
##'             \item {num_tips} {Numeric. The number of descendent tips.}
##'
##'             \item {taxon} {A list of taxonomic properties:}
##'             \itemize{
##'                 \item {ott_id} {Numeric. The OpenTree Taxonomy ID (ottID).}
##'
##'                 \item {name} {String. The taxonomic name of the queried node.}
##'
##'                 \item {unique_name} {String. The string that uniquely
##'                 identifies the taxon in OTT.}
##'
##'                 \item {rank} {String. The taxonomic rank of the taxon in OTT.}
##'
##'                 \item {tax_sources} {List. A list of identifiers for taxonomic
##'                 sources, such as other taxonomies, that define taxa judged
##'                 equivalent to this taxon.}
##'             }
##'         }
##'
##'     \item {source_list} {List. Present only if \code{include_source_list} is
##'     "true". The sourceid ordering is the precedence order for synthesis, with
##'     relationships from earlier trees in the list having priority over those
##'     from later trees in the list. See \code{source_id_map} below for study details.}
##'
##'     \item {source_id_map} {Named list of lists. Present only if
##'     \code{include_source_list} is "true". Names correspond to the
##'     sourceids used in \code{source_list} above. Source trees will have the
##'     following properties:}
##'
##'         \itemize{
##'             \item {git_sha} {String. The git SHA identifying a particular source
##'             version.}
##'
##'             \item {tree_id} {String. The tree id associated with the study id used.}
##'
##'             \item {study_id} {String. The study identifier. Will typically include
##'             a prefix ("pg_" or "ot_").}
##'         }
##'
##'     \item {synth_id} {The unique string for this version of the tree.}
##' }
##' @seealso \code{\link{study_list}} to explore the list of studies
##'     used in the synthetic tree.
##'
##' @examples
##' \dontrun{
##' res <- tol_about()
##' studies <- study_list(tol_about(include_source_list=TRUE))
##' }
##' @export
tol_about <- function(include_source_list=FALSE, ...) {
    res <- .tol_about(include_source_list=include_source_list, ...)
    class(res) <- "tol_summary"
    res
}


##' @export
print.tol_summary <- function(x, ...) {
    cat("\nOpenTree Synthetic Tree of Life.\n\n")
    cat("\tTree version: ", x$synth_id, "\n", sep="")
    cat("\tTaxonomy version: ", x$taxonomy, "\n", sep="")
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
##' @title List of studies used in the Tree of Life
##'
##' #details This function takes the object resulting from
##' \code{tol_about(study_list = TRUE)} and returns a data frame
##' listing the \code{tree_id}, \code{study_id} and \code{git_sha} for
##' the studies currently included in the Tree of Life.
##'
##' @param tol an object created using \code{tol_about(study_list = TRUE)}
##'
##' @return a data frame
##' @export
study_list <- function(tol) UseMethod("study_list")


##' @export
##' @rdname study_list
study_list.tol_summary <- function(tol) {
    if (! exists("source_list", tol)) {
        stop("Make sure that your object has been created using ",
             sQuote("tol_about(include_source_list = TRUE)"))
    }
    tol <- lapply(tol[["source_id_map"]], function(x) {
        c("tree_id"=x[["tree_id"]],
          "study_id"=x[["study_id"]],
          "git_sha"=x[["git_sha"]])
    })
    tol <- do.call("rbind", tol)
    data.frame(tol, stringsAsFactors=FALSE)
}


##' Most Recent Common Ancestor for a set of nodes
##'
##' @title MRCA of taxa from the synthetic tree
##'
##' @details Get the MRCA of a set of nodes on the current synthetic
##' tree. Accepts any combination of node ids and ott ids as
##' input. Returns information about the most recent common ancestor
##' (MRCA) node as well as the most recent taxonomic ancestor (MRTA)
##' node (the closest taxonomic node to the MRCA node in the synthetic
##' tree; the MRCA and MRTA may be the same node). If any of the
##' specified nodes is not in the synthetic tree an error will be returned.
##'
##' @param ott_ids Numeric vector. The ott ids for which the MRCA is desired.
##' @param node_ids Character vector. The node ids for which the MRCA is desired.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##'
##' @return A list
##'
##' @examples
##' \dontrun{
##' birds_mrca <- tol_mrca(ott_ids=c(412129, 536234))
##' }
##' @export
tol_mrca <- function(ott_ids=NULL, node_ids=NULL, ...) {
    res <- .tol_mrca(ott_ids=ott_ids, node_ids=node_ids, ...)
    return(res)
}


##' Extract a subtree from the synthetic tree from an ott id.
##'
##' @title Extract a subtree from the synthetic tree
##'
##' @details Given a node, return the subtree of the synthetic tree descended
##'     from that node, either in newick or ArguSON format. The start node may
##'     be specified using either a node id or an ott id, but not both. If the
##'     specified node is not in the synthetic tree an error will be returned.
##'     There is a size limit of 25000 tips for this method.
##'
##' @param ott_id Numeric. The ott id of the node in the tree that should
##'     serve as the root of the tree returned.
##' @param node_id Character. The node id of the node in the tree that should
##'     serve as the root of the tree returned.
##' @param file if specified, the function will write the subtree to a
##'     file in newick format.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##'
##' @return If no value is specified to the \code{file} argument
##'     (default), a phyogenetic tree of class \code{phylo}.
##'     Otherwise, the function returns invisibly a logical indicating
##'     whether the file was successfully created.
##'
##' @examples
##'    \dontrun{
##'       res <- tol_subtree(ott_id=81461)
##'     }
##' @export
tol_subtree <- function(ott_id=NULL, node_id=NULL, file, ...) {
    res <- .tol_subtree(ott_id=ott_id, node_id=node_id, ...)

    if (!missing(file)) {
        unlink(file)
        cat(res$newick, file=file)
        return(invisible(file.exists(file)))
    } else {
        phy <- phylo_from_otl(res)
        return(phy)
    }
}


##' Return the induced subtree on the synthetic tree that relates a list of nodes.
##'
##' @title Subtree from the Open Tree of Life
##'
##' @details Return a tree with tips corresponding to the nodes identified in
##' the input set that is consistent with the topology of the current
##' synthetic tree. This tree is equivalent to the minimal subtree
##' induced on the draft tree by the set of identified nodes.
##'
##' @param ott_ids Numeric vector. OTT ids indicating nodes to be used 
##'     as tips in the induced tree.
##' @param node_ids Character vector. Node ids indicating nodes to be used 
##'     as tips in the induced tree.
##' @param file If specified, the function will write the subtree to a
##'     file in newick format.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##'
##' @return If no value is specified to the \code{file} argument
##'     (default), a phyogenetic tree of class \code{phylo}.
##'
##'     Otherwise, the function returns invisibly a logical indicating
##'     whether the file was successfully created.
##'
##' @examples
##' \dontrun{
##' res <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458))
##' tree_file <- tempfile(fileext=".tre")
##' tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458),
##'                     file=tree_file)
##' }
##' @export
tol_induced_subtree <- function(ott_ids=NULL, node_ids=NULL, file, ...) {
    res <- .tol_induced_subtree(ott_ids=ott_ids, node_ids=node_ids, ...)
    if (!missing(file)) {
        unlink(file)
        cat(res$newick, file=file)
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
##'
##' @return A character vector containing the contents of \code{tip_labels}
##'     with any OTT ids removed.
##'
##' @examples
##' \dontrun{
##' genera <- c("Perdix", "Clangula", "Dendroica", "Cinclus", "Stellula", "Struthio")
##' tr <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104, 316878, 102710, 176458))
##' tr$tip.label %in% genera
##' tr$tip.label <- strip_ott_ids(tr$tip.label)
##' tr$tip.label %in% genera
##'}
##'@export
strip_ott_ids <- function(tip_labels) {
    sub("_ott\\d+$", "", tip_labels)
}


##' Get summary information about a node in the synthetic tree
##'
##' @title Node info
##'
##' @details Returns summary information about a node in the graph. The
##'     node of interest may be specified using either a node id or an
##'     taxon id, but not both. If the specified node or OTT id is not
##'     in the graph, an error will be returned.
##'
##' @param ott_id Numeric. The OpenTree taxonomic identifier.
##' @param node_id Character. The OpenTree node identifier.
##' @param include_lineage Logical (default = FALSE). Whether to return the
##'     lineage of the node from the synthetic tree.
##' @param ... additional arguments to customize the API call (see
##'     ?rotl for more information)
##'
##' @return \code{tol_node_info} returns a list of summary information
##'     about the queried node.
##'
##' \itemize{
##'
##'     \item {node_id} {String. The canonical identifier of the node.}
##'
##'     \item {num_tips} {Numeric. The number of descendent tips.}
##'
##'     \item {taxon} {A list of taxonomic properties. Only returned if
##'     the queried node is a taxon. Each source has:}
##'
##'         \itemize{
##'             \item {ott_id} {Numeric. The OpenTree Taxonomy ID (ottID).}
##'
##'             \item {name} {String. The taxonomic name of the queried node.}
##'
##'             \item {unique_name} {String. The string that uniquely
##'             identifies the taxon in OTT.}
##'
##'             \item {rank} {String. The taxonomic rank of the taxon in OTT.}
##'
##'             \item {tax_sources} {List. A list of identifiers for taxonomic
##'             sources, such as other taxonomies, that define taxa judged
##'             equivalent to this taxon.}
##'         }
##'
##'     The following properties list support/conflict for the node across
##'     synthesis source trees. All properties involve sourceid keys and
##'     nodeid values (see \code{source_id_map} below).
##'
##'     \item {partial_path_of} {List. The edge below this synthetic tree node
##'     is compatible with the edge below each of these input tree nodes (one
##'     per tree). Each returned element is reported as sourceid:nodeid.}
##'
##'     \item {supported_by} {List. Input tree nodes (one per tree) that support
##'     this synthetic tree node. Each returned element is reported as
##'     sourceid:nodeid.}
##'
##'     \item {terminal} {List. Input tree nodes (one per tree) that are equivalent
##'     to this synthetic tree node (via an exact mapping, or the input tree
##'     terminal may be the only terminal descended from this synthetic tree node.
##'     Each returned element is reported as sourceid:nodeid.}
##'
##'     \item {conflicts_with} {Named list of lists. Names correspond to
##'     sourceid keys. Each list contains input tree node ids (one or more per tree)
##'     that conflict with this synthetic node.}
##'
##'     \item {source_id_map} {Named list of lists. Names correspond to the
##'     sourceid keys used in the 4 properties above. Source trees will have the
##'     following properties:}
##'
##'         \itemize{
##'             \item {git_sha} {The git SHA identifying a particular source
##'             version.}
##'
##'             \item {tree_id} {The tree id associated with the study id used.}
##'
##'             \item {study_id} {The study identifier. Will typically include
##'             a prefix ("pg_" or "ot_").}
##'         }
##'     The only sourceid that does not correspond to a source tree is the taxonomy,
##'     which will have the name "ott"+`taxonomy_version`, and the value is the
##'     ott_id of the taxon in that taxonomy version. "Taxonomy" will only ever
##'     appear in \code{supported_by}.
##' }
##'
##' @examples
##' \dontrun{
##' birds <- tol_node_info(ott_id=81461)
##' #synth_sources(birds)
##' tax_rank(birds)
##' ott_id(birds)
##' }
##' @export
tol_node_info <- function(ott_id=NULL, node_id=NULL, include_lineage=FALSE, ...) {
    res <- .tol_node_info(ott_id=ott_id, node_id=node_id,
                          include_lineage=include_lineage, ...)
    class(res) <- "tol_node"
    return(res)
}


##' @export
##' @param tax an object returned by \code{tol_node_info}.
##' @rdname tol_node_info
tax_rank.tol_node <- function(tax) {
    tax[["taxon"]]$rank
}

##' @export
##' @rdname tol_node_info
ott_id.tol_node <- function(tax, ...) {
    tax[["taxon"]]$ott_id
}








## *** the following are deprecated ***
## basically we need to update things for the new `source_id_map`

## @export
## @rdname tol_node_info
synth_sources <- function(tax) UseMethod("synth_sources")


## @export
## @rdname tol_node_info
synth_sources.tol_node <- function(tax) {
    tt <- lapply(tax$synth_sources, function(x) {
        c(x["study_id"], x["tree_id"], x["git_sha"])
    })
    tt <- do.call("rbind", tt)
    as.data.frame(tt, stringsAsFactors=FALSE)
}
