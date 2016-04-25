
.source_list <- function(tax, ...) {
    if (! exists("source_id_map", tax)) {
        ## it should only be missing with tol_about when using
        ## include_source_list=FALSE
        stop("Make sure that your object has been created using ",
             sQuote("tol_about(include_source_list = TRUE)"))
    }
    tt <- lapply(tax[["source_id_map"]], function(x) {
        c(x[["study_id"]], x[["tree_id"]], x[["git_sha"]])
    })
    tt <- do.call("rbind", tt)
    setNames(as.data.frame(tt, stringsAsFactors=FALSE),
             c("study_id", "tree_id", "git_sha"))
}

##' Basic information about the Open Tree of Life (the synthetic tree)
##'
##' @title Information about the Tree of Life
##'
##' @details Summary information about the current draft tree of life,
##'     including information about the list of trees and the taxonomy
##'     used to build it. The object returned by \code{tol_about} can
##'     be passed to the taxonomy methods (\code{tax_name()},
##'     \code{tax_rank()}, \code{tax_sources()}, \code{ott_id}), to
##'     extract relevant taxonomic information for the root of the
##'     synthetic tree.
##'
##' @param include_source_list Logical (default =
##'     \code{FALSE}). Return an ordered list of source trees.
##' @param tax an object created with a call to \code{tol_about}.
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
##'     \item {num_source_trees} {The number of trees used as sources
##'     (may be >1 tree per study).}
##'
##'     \item {taxonomy_version} {The Open Tree Taxonomy version used
##'     as a source.}
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
##'                 \item {ott_id} {Numeric. The OpenTree Taxonomy ID (ott_id).}
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
##'     \item {source_list} {List. Present only if
##'     \code{include_source_list} is \code{TRUE}. The sourceid
##'     ordering is the precedence order for synthesis, with
##'     relationships from earlier trees in the list having priority
##'     over those from later trees in the list. See
##'     \code{source_id_map} below for study details.}
##'
##'     \item {source_id_map} {Named list of lists. Present only if
##'     \code{include_source_list} is \code{TRUE}. Names correspond to
##'     the \sQuote{sourceids} used in \code{source_list}
##'     above. Source trees will have the following properties:}
##'
##'         \itemize{
##'             \item {git_sha} {String. The git SHA identifying a particular source
##'             version.}
#'
##'             \item {tree_id} {String. The tree id associated with the study id used.}
##'
##'             \item {study_id} {String. The study identifier. Will typically include
##'             a prefix ("pg_" or "ot_").}
##'         }
##'
##'     \item {synth_id} {The unique string for this version of the tree.}
##' }
##' @seealso \code{\link{source_list}} to explore the list of studies
##'     used in the synthetic tree (see example).
##'
##' @examples
##' \dontrun{
##' res <- tol_about()
##' tax_sources(res)
##' ott_id(res)
##' studies <- source_list(tol_about(include_source_list=TRUE))}
##' @rdname tol_about
##' @export
tol_about <- function(include_source_list=FALSE, ...) {
    res <- .tol_about(include_source_list=include_source_list, ...)
    class(res) <- c("tol_summary", class(res))
    res
}


##' @export
print.tol_summary <- function(x, ...) {
    cat("\nOpenTree Synthetic Tree of Life.\n\n")
    cat("Tree version: ", x$synth_id, "\n", sep="")
    cat("Taxonomy version: ", x$taxonomy, "\n", sep="")
    cat("Constructed on: ", x$date_created, "\n", sep="")
    cat("Number of terminal taxa: ", x$root$num_tips, "\n", sep="")
    cat("Number of source trees: ", x$num_source_trees, "\n", sep="")
    cat("Number of source studies: ", x$num_source_studies, "\n", sep = "")
    cat("Source list present: ", ifelse(exists("source_list", x), "true", "false"), "\n", sep="")
    cat("Root taxon: ", x$root$taxon$name, "\n", sep="")
    cat("Root ott_id: ", x$root$taxon$ott_id, "\n", sep="")
    cat("Root node_id: ", x$root$node_id, "\n", sep="")
}

tol_about_method_factory <- function(.f) {
    function(tax, ...) {
        res <- list(.f(tax[["root"]][["taxon"]]))
        names(res) <- .tax_unique_name(tax[["root"]][["taxon"]])
        res <- add_otl_class(res, .f)
        res
    }
}

##' @export
##' @rdname tol_about
tax_rank.tol_summary <- tol_about_method_factory(.tax_rank)

##' @export
##' @rdname tol_about
tax_sources.tol_summary <- tol_about_method_factory(.tax_sources)

##' @export
##' @rdname tol_about
unique_name.tol_summary <- tol_about_method_factory(.tax_unique_name)

##' @export
##' @rdname tol_about
tax_name.tol_summary <- tol_about_method_factory(.tax_name)

##' @export
##' @rdname tol_about
ott_id.tol_summary <- tol_about_method_factory(.tax_ott_id)

##' @export
##' @rdname source_list
source_list.tol_summary <- .source_list



##' Most Recent Common Ancestor for a set of nodes
##'
##' @title MRCA of taxa from the synthetic tree
##'
##' @details Get the MRCA of a set of nodes on the current synthetic
##'     tree. Accepts any combination of node ids and ott ids as
##'     input. Returns information about the most recent common
##'     ancestor (MRCA) node as well as the most recent taxonomic
##'     ancestor (MRTA) node (the closest taxonomic node to the MRCA
##'     node in the synthetic tree; the MRCA and MRTA may be the same
##'     node). If they are the same, the taxonomic information will be
##'     in the \code{mrca} slot, otherwise they will be in the
##'     \code{nearest_taxon} slot of the list. If any of the specified
##'     nodes is not in the synthetic tree an error will be returned.
##'
##'     Taxonomic methods (\code{tax_sources()}, \code{ott_id()},
##'     \code{unique_name()}, ...) are availble on the objects
##'     returned by \code{tol_mrca()}. If the MRCA node is MRTA, the
##'     name of the object returned by these methods will start with
##'     \sQuote{ott}, otherwise it will start with \sQuote{mrca}.
##'
##' @param ott_ids Numeric vector. The ott ids for which the MRCA is desired.
##' @param node_ids Character vector. The node ids for which the MRCA is desired.
##' @param tax an object returned by \code{tol_mrca()}.
##' @param ... additional arguments to customize the API call (see
##'     \code{\link{rotl}} for more information).
##'
##' @return An invisible list of the MRCA node properties:
##'
##' \itemize{
##'
##'     \item {mrca} {List of node properties.}
##'
##'     \itemize{
##'         \item {node_id} {String. The canonical identifier of the node.}
##'
##'         \item {num_tips} {Numeric. The number of descendent tips.}
##'
##'         \item {taxon} {A list of taxonomic properties. Only returned if
##'         the queried node is a taxon. (If the node is not a taxon, a
##'         \code{nearest_taxon} list is returned (see below)).}
##'
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
##'                \item {tax_sources} {List. A list of identifiers for taxonomic
##'                 sources, such as other taxonomies, that define taxa judged
##'                 equivalent to this taxon.}
##'             }
##'
##'         The following properties list support/conflict for the node across
##'         synthesis source trees. All properties involve sourceid keys and
##'         nodeid values (see \code{source_id_map} below) Not all properties are
##'         are present for every node.
##'
##'         \item {partial_path_of} {List. The edge below this synthetic tree node
##'         is compatible with the edge below each of these input tree nodes (one
##'         per tree). Each returned element is reported as sourceid:nodeid.}
##'
##'         \item {supported_by} {List. Input tree nodes (one per tree) that support
##'         this synthetic tree node. Each returned element is reported as
##'         sourceid:nodeid.}
##'
##'         \item {terminal} {List. Input tree nodes (one per tree) that are equivalent
##'         to this synthetic tree node (via an exact mapping, or the input tree
##'         terminal may be the only terminal descended from this synthetic tree node.
##'         Each returned element is reported as sourceid:nodeid.}
##'
##'         \item {conflicts_with} {Named list of lists. Names correspond to
##'         sourceid keys. Each list contains input tree node ids (one or more per tree)
##'         that conflict with this synthetic node.}
##'     }
##'
##'     \item {nearest_taxon} {A list of taxonomic properties of the nearest rootward
##'     taxon node to the MRCA node. Only returned if the MRCA node is a not taxon
##'     (otherwise the \code{taxon} list above is returned).}
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
##'            \item {tax_sources} {List. A list of identifiers for taxonomic
##'             sources, such as other taxonomies, that define taxa judged
##'             equivalent to this taxon.}
##'         }
##'
##'     \item {source_id_map} {Named list of lists. Names correspond to the
##'     sourceid keys used in the support/conflict properties of the \code{mrca}
##'     list above. Source trees will have the following properties:}
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
##'
##'    }
##'
##' @examples
##' \dontrun{
##' birds_mrca <- tol_mrca(ott_ids=c(412129, 536234))
##' ott_id(birds_mrca)
##' tax_sources(birds_mrca)}
##' @rdname tol_mrca
##' @export
tol_mrca <- function(ott_ids=NULL, node_ids=NULL, ...) {
    res <- .tol_mrca(ott_ids=ott_ids, node_ids=node_ids, ...)
    class(res) <- c("tol_mrca", class(res))
    return(res)
}

##' @export
print.tol_mrca <- function(x, ...) {
    cat("\nOpenTree MRCA node.\n\n")
    cat("Node id: ", x$mrca$node_id, "\n", sep="")
    cat("Number of terminal descendants: ", x$mrca$num_tips, "\n", sep="")
    if (is_taxon(x[["mrca"]][["taxon"]])) {
        cat("Is taxon: TRUE\n")
        cat("Name: ", x$mrca$taxon$name, "\n", sep="")
        cat("ott id: ", x$mrca$taxon$ott_id, "\n", sep="")
    } else {
        cat("Is taxon: FALSE\n")
        cat("Nearest taxon:\n")
        cat("  Name: ", x$nearest_taxon$name, "\n", sep="")
        cat("  ott id: ", x$nearest_taxon$ott_id, "\n", sep="")
    }
}

tol_mrca_method_factory <- function(.f) {
    function(tax, ...) {
        if (is_taxon(tax[["mrca"]][["taxon"]])) {
            res <- list(.f(tax[["mrca"]][["taxon"]]))
            names(res) <- .tax_unique_name(tax[["mrca"]][["taxon"]])
            attr(res, "taxon_type") <- "mrca"
        } else {
            res <- list(.f(tax[["nearest_taxon"]]))
            names(res) <- .tax_unique_name(tax[["nearest_taxon"]])
            attr(res, "taxon_type") <- "nearest_taxon"
        }
        res <- add_otl_class(res, .f)
        res
    }
}

##' @export
##' @rdname tol_mrca
tax_sources.tol_mrca <- tol_mrca_method_factory(.tax_sources)

##' @export
##' @rdname tol_mrca
unique_name.tol_mrca <- tol_mrca_method_factory(.tax_unique_name)

##' @export
##' @rdname tol_mrca
tax_name.tol_mrca <- tol_mrca_method_factory(.tax_name)

##' @export
##' @rdname tol_mrca
tax_rank.tol_mrca <- tol_mrca_method_factory(.tax_rank)

##' @export
##' @rdname tol_mrca
ott_id.tol_mrca <- tol_mrca_method_factory(.tax_ott_id)

##' @export
##' @rdname tol_mrca
source_list.tol_mrca <- .source_list


##' Extract a subtree from the synthetic tree from an Open Tree node id.
##'
##' @title Extract a subtree from the synthetic tree
##'
##' @details Given a node, return the subtree of the synthetic tree descended
##'     from that node. The start node may be specified using either a node id
##'     or an ott id, but not both. If the specified node is not in the
##'     synthetic tree an error will be returned. There is a size limit of
##'     25000 tips for this method.
##'
##' @param ott_id Numeric. The ott id of the node in the tree that should
##'     serve as the root of the tree returned.
##' @param node_id Character. The node id of the node in the tree that should
##'     serve as the root of the tree returned.
##' @param label_format Character. Defines the label type; one of
##'     \dQuote{\code{name}}, \dQuote{\code{id}}, or
##'      \dQuote{\code{name_and_id}} (the default).
##' @param file If specified, the function will write the subtree to a
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
##' \dontrun{
##' res <- tol_subtree(ott_id=241841)}
##' @export
tol_subtree <- function(ott_id=NULL, node_id=NULL, label_format=NULL,
                        file, ...) {
    res <- .tol_subtree(ott_id=ott_id, node_id=node_id,
                        label_format=label_format, ...)

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
##' @param label_format Character. Defines the label type; one of
##'     \dQuote{\code{name}}, \dQuote{\code{id}}, or
##'      \dQuote{\code{name_and_id}} (the default).
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
##' res <- tol_induced_subtree(ott_ids=c(292466, 267845, 666104, 316878, 102710))
##' tree_file <- tempfile(fileext=".tre")
##' tol_induced_subtree(ott_ids=c(292466, 267845, 666104, 316878, 102710),
##'                     file=tree_file)}
##' @export
tol_induced_subtree <- function(ott_ids=NULL, node_ids=NULL, label_format=NULL,
                                file, ...) {
    res <- .tol_induced_subtree(ott_ids=ott_ids, node_ids=node_ids,
                                label_format=label_format, ...)
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
##' @param tip_labels a character vector containing tip labels (most
##'     likely the \code{tip.label} element from a tree returned by
##'     \code{\link{tol_induced_subtree}}
##' @param remove_underscores logical (defaults to FALSE). If set to
##'     TRUE underscores in tip labels are converted to spaces
##' @return A character vector containing the contents of
##'     \code{tip_labels} with any OTT ids removed.
##'
##' @examples
##' \dontrun{
##' genera <- c("Perdix", "Dendroica", "Cinclus", "Selasphorus", "Struthio")
##' tr <- tol_induced_subtree(ott_ids=c(292466, 267845, 666104, 102710))
##' tr$tip.label %in% genera
##' tr$tip.label <- strip_ott_ids(tr$tip.label)
##' tr$tip.label %in% genera}
##'@export
strip_ott_ids <- function(tip_labels, remove_underscores=FALSE){
    stripped <- sub("_ott\\d+$", "", tip_labels)
    if(remove_underscores){
        return(gsub("_", " ", stripped))
    }
    stripped
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
##'     If the argument \code{include_lineage=TRUE} is used, you can
##'     use \code{tax_lineage()} or \code{tol_lineage} to return the
##'     taxonomic information or the node information for all the
##'     ancestors to this node, down to the root of the tree.
##'
##'
##' @param ott_id Numeric. The OpenTree taxonomic identifier.
##' @param node_id Character. The OpenTree node identifier.
##' @param include_lineage Logical (default = FALSE). Whether to return the
##'     lineage of the node from the synthetic tree.
##' @param ... additional arguments to customize the API call (see
##'     ?rotl for more information)
##'
##' @return \code{tol_node_info} returns an invisible list of summary
##'     information about the queried node:
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
##'
##'    }
##'
##'     \code{tol_lineage} and \code{tax_lineage} return data
##'         frames. \code{tol_lineage} indicate for each ancestor its
##'         node identifier, the number of tips descending from that
##'         node, and whether it corresponds to a taxonomic level.
##'
##' @examples
##' \dontrun{
##' birds <- tol_node_info(ott_id=81461, include_lineage=TRUE)
##' source_list(birds)
##' tax_rank(birds)
##' ott_id(birds)
##' tax_lineage(birds)
##' tol_lineage(birds)}
##' @export
tol_node_info <- function(ott_id=NULL, node_id=NULL, include_lineage=FALSE, ...) {
    res <- .tol_node_info(ott_id=ott_id, node_id=node_id,
                          include_lineage=include_lineage, ...)
    class(res) <- c("tol_node", class(res))
    return(res)
}

tol_node_method_factory <- function(.f) {
    function(tax, ...) {
        res <- setNames(list(.f(tax[["taxon"]])),
                        .tax_unique_name(tax[["taxon"]]))
        res <- add_otl_class(res, .f)
        res
    }
}

##' @export
print.tol_node <- function(x, ...) {
    cat("\nOpenTree node.\n\n")
    cat("Node id: ", x$node_id, "\n", sep="")
    cat("Number of terminal descendants: ", x$num_tips, "\n", sep="")
    if (is_taxon(x[["taxon"]])) {
        cat("Is taxon: TRUE\n")
        cat("Name: ", x$taxon$name, "\n", sep="")
        cat("Rank: ", x$taxon$rank, "\n", sep="")
        cat("ott id: ", x$taxon$ott_id, "\n", sep="")
    } else {
        cat("Is taxon: FALSE\n")
    }
}

##' @export
##' @param tax an object returned by \code{tol_node_info}.
##' @rdname tol_node_info
tax_rank.tol_node <- tol_node_method_factory(.tax_rank)

##' @export
##' @rdname tol_node_info
tax_sources.tol_node <- tol_node_method_factory(.tax_sources)

##' @export
##' @rdname tol_node_info
unique_name.tol_node <- tol_node_method_factory(.tax_unique_name)

##' @export
##' @rdname tol_node_info
tax_name.tol_node <- tol_node_method_factory(.tax_name)

##' @export
##' @rdname tol_node_info
ott_id.tol_node <- tol_node_method_factory(.tax_ott_id)

##' @export
##' @rdname tol_node_info
source_list.tol_node <- .source_list

##' @export
##' @rdname tol_node_info
tax_lineage.tol_node <- function(tax, ...) {
    check_lineage(tax)
    lg <- lapply(tax[["lineage"]], function(x) {
        if (exists("taxon", x)) {
            build_lineage(x[["taxon"]])
        } else {
            NULL
        }

    })
    lg <- do.call("rbind", lg)
    as.data.frame(lg, stringsAsFactors = FALSE)
}


##' @export
##' @rdname tol_node_info
tol_lineage.tol_node <- function(tax, ...) {
    check_lineage(tax)
    lg <- lapply(tax[["lineage"]], function(x) {
        c("node_id" = x[["node_id"]],
          "num_tips" = x[["num_tips"]],
          "is_taxon" = exists("taxon", x))
    })
    lg <- do.call("rbind", lg)
    as.data.frame(lg, stringsAsFactors = FALSE)
}
