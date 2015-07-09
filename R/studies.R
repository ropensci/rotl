##' Return a list of studies that match given properties
##'
##' @title find_study
##' @param exact Should exact matching be used? (logical, default
##'     \code{FALSE})
##' @param property The property to be searched on (character)
##' @param value The property-value to be searched on (character)
##' @param verbose Should the output include all metadata (logical
##'     default \code{FALSE})
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @seealso \code{\link{studies_properties}} which lists properties
##'     against which the studies can be searched
##' @export
##' @examples
##' \dontrun{
##' study <- studies_find_studies(property="ot:studyId", value="pg_719")
##' }

studies_find_studies <- function(property=NULL, value=NULL, verbose=FALSE,
                                 exact=FALSE, ...) {
    res <- .studies_find_studies(property = property, value = value,
                                 verbose = verbose, exact = exact, ...)
    class(res) <- c("found_studies", class(res))
    return(res)
}

##' Return a list of trees that match a given properties
##'
##' The list of possible values to be used as values for the argument
##' \code{property} can be found using the function
##' \code{\link{studies_properties}}.
##'
##' @title find trees
##' @param property The property to be searched on (character)
##' @param value The property-value to be searched on (character)
##' @param verbose Should the output include all metadata? (logical,
##'     default \code{FALSE})
##' @param exact Should exact matching be used? (logical, default
##'     \code{FALSE})
##' @param ... additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @seealso \code{\link{studies_properties}} which lists properties
##'     against which the studies can be searched
##' @export
##' @examples
##' \dontrun{
##' res <- studies_find_trees(property="ot:ottTaxonName", value="Garcinia")
##' }

studies_find_trees <- function(property=NULL, value=NULL, verbose=FALSE,
                               exact=FALSE, ...) {
    res <- .studies_find_trees(property = property, value = value,
                               verbose = verbose, exact = exact, ...)
    class(res) <- c("found_studies", class(res))
    return(res)
}

##'@export
print.found_studies <- function(x, ...){
  
 cat(" List of Open Tree studies with", length(x[[1]]), "hits \n")
}
##' Return the list of study properties that can be used to search
##' studies and trees used in the synthetic tree.
##'
##' The list returned has 2 elements \code{tree_properties} and
##' \code{studies_properties}. Each of these elements lists theadditional arguments to customize the API request
##' properties that can be used to search for trees and studies that
##' are contributing to the synthetic tree.
##'
##' @title Studies properties
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @return A list of the study properties that can be used to find
##'     studies and trees that are contributing to the synthetic tree.
##' @seealso \code{\link{studies_find_trees}}
##' @export
##' @examples
##' \dontrun{
##'  all_the_properties <- studies_properties()
##'  unlist(all_the_properties$tree_properties)
##' }

studies_properties <- function(...) {
    res <- .studies_properties(...)
    lapply(res, unlist)
}




##' Returns a study for given ID
##'
##' If \code{file_format} is missing, the function returns an object
##' of the class \code{phylo} from the \code{\link[ape]{ape}} package
##' (default), or an object of the class \code{nexml} from the
##' \code{RNeXML} package.
##'
##' Otherwise \code{file_format} can be either \code{newick},
##' \code{nexus}, \code{nexml} or \code{json}, and the function will
##' generate a file of the selected format. In this case, a file name
##' needs to be provided using the argument \code{file}. If a file
##' with the same name already exists, it will be silently
##' overwritten.
##'
##' @title Get Study
##' @param study_id the study ID for the study of interest (character)
##' @param object_format the class of the object the query should
##'     return (either \code{phylo} or \code{nexml}). Ignored if
##'     \code{file_format} is specified.
##' @param file_format the format of the file to be generated
##'     (\code{newick}, \code{nexus}, \code{nexml} or \code{json}).
##' @param file the file name where the output of the function will be
##'     saved.
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @return if \code{file_format} is missing, an object of class
##'     \code{phylo} or \code{nexml}, otherwise a logical indicating
##'     whether the file was successfully created.
##' @seealso \code{\link{get_study_meta}}
##' @export
##' @importFrom jsonlite toJSON
##' @examples
##' \dontrun{
##' that_one_study <- get_study(study_id="pg_719", object_format="phylo")
##' if (require(RNeXML)) { ## if RNeXML is installed get the object directly
##'    nexml_study <- get_study(study_id="pg_719", object_format="nexml")
##' } else { ## otherwise write it to a file
##'    get_study(study_id="pg_719", file_format="nexml", file=tempfile(fileext=".nexml"))
##' }
##' }
get_study <- function(study_id = NULL, object_format = c("phylo", "nexml"),
                      file_format, file, ...) {
    object_format <- match.arg(object_format)
    if (!missing(file)) {
        if (!missing(file_format)) {
            file_format <- match.arg(file_format, c("newick", "nexus", "nexml", "json"))
            res <- .get_study(study_id, format = file_format)
            unlink(file)
            if (identical(file_format, "json")) {
                cat(jsonlite::toJSON(res), file=file)
            } else {
                cat(res, file=file)
            }
            return(invisible(file.exists(file)))
        } else {
            stop(sQuote("file_format"), " must be specified.")
        }
    } else if (identical(object_format, "phylo")) {
        file_format <- "newick"
        res <- .get_study(study_id = study_id, format=file_format, ...)
        res <- phylo_from_otl(res)
    } else if (identical(object_format, "nexml")) {
        file_format <- "nexml"
        res <- .get_study(study_id = study_id, format = file_format, ...)
        res <- nexml_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}

##' Returns a specific tree from within a study
##'
##' @title Study Tree
##' @param study_id the identifier of a study (character)
##' @param tree_id the identifier of a tree within the study
##' @param object_format the class of the object to be returned
##'     (default and currently only possible value \code{phylo} from
##'     the \code{\link[ape]{ape}} package).
##' @param tip_label the format of the tip
##'     labels. \dQuote{\code{original_label}} (default) returns the
##'     original labels as provided in the study,
##'     \dQuote{\code{ott_id}} labels are replaced by their ott IDs,
##'     \dQuote{\code{ott_taxon_name}} labels are replaced by their
##'     Open Tree Taxonomy taxon name.
##' @param file_format the format of the file to be generated
##'     (\code{newick} default, \code{nexus}, or \code{json}).
##' @param file the file name where the output of the function will be
##'     saved.
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @return if \code{file_format} is missing, an object of class
##'     \code{phylo}, otherwise a logical indicating whether the file
##'     was successfully created.
##' @export
##' @importFrom jsonlite toJSON
##' @examples
##' \dontrun{
##'  tree <- get_study_tree(study_id="pg_1144", tree="tree2324")
##'
##'  ## comparison of the first few tip labels depending on the options used
##'  head(get_study_tree(study_id="pg_1144", tree="tree2324", tip_label="original_label")$tip.label)
##'  head(get_study_tree(study_id="pg_1144", tree="tree2324", tip_label="ott_id")$tip.label)
##'  head(get_study_tree(study_id="pg_1144", tree="tree2324", tip_label="ott_taxon_name")$tip.label)
##' }

get_study_tree <- function(study_id=NULL, tree_id=NULL, object_format=c("phylo"),
                           tip_label = c("original_label", "ott_id", "ott_taxon_name"),
                           file_format, file, ...) {

    object_format <- match.arg(object_format)
    tip_label <- match.arg(tip_label)
    tip_label <- switch(tip_label,
                        original_labels = "ot:originallabel",
                        ott_id =  "ot:ottid",
                        ott_taxon_name = "ot:otttaxonname")
    if (!missing(file)) {
        if (!missing(file_format)) {
            file_format <- match.arg(file_format, c("newick", "nexus", "json"))
            if (missing(file)) stop("You must specify a file to write your output")
            res <- .get_study_tree(study_id = study_id, tree_id = tree_id,
                                   format=file_format, tip_label = tip_label, ...)
            unlink(file)
            if (identical(file_format, "json")) {
                cat(jsonlite::toJSON(res), file=file)
            } else {
                cat(res, file=file)
            }
            return(invisible(file.exists(file)))
        } else {
            stop(sQuote("file_format"), " must be specified.")
        }
    } else if (identical(object_format, "phylo")) {
        file_format <- "newick"
        res <- .get_study_tree(study_id = study_id, tree_id = tree_id,
                               format=file_format, tip_label = tip_label, ...)
        res <- phylo_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}

##' Retrieve metadata about a study in the Open Tree of Life datastore.
##'
##' \code{get_study_meta} returns a long list of attributes for the
##' studies that are contributing to the synthetic tree. To help with
##' the extraction of relevant information from this list, several
##' helper functions exists: \itemize{
##'
##'   \item {get_tree_ids} { The identifiers of the trees
##'   associated with the study }
##'
##'   \item {get_publication} { The citation information of the
##'   publication for the study. The DOI (or URL) for the study is
##'   available as an attribute to the returned object (i.e.,
##'   \code{attr(object, "DOI")} ) }.
##'
##'   \item {candidate_for_synth} { The identifier of the tree(s) from
##'   the study used in the synthetic tree. This is a subset of the
##'   result of \code{get_tree_ids}.}
##'
##' }
##'
##' @title Study Metadata
##' @param study_id the study identifier (character)
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @param sm an object created by \code{get_study_meta}
##' @return named-list containing the metadata associated with the
##'     study requested
##' @export
##' @examples
##' \dontrun{
##' req <- get_study_meta("pg_719")
##' get_tree_ids(req)
##' candidate_for_synth(req)
##' get_publication(req)
##' }
get_study_meta <- function(study_id, ...) {
    res <- .get_study_meta(study_id = study_id, ...)
    class(res) <- "study_meta"
    attr(res, "study_id") <- study_id
    res
}

##' @export
print.study_meta <- function(x, ...) {
    cat("Metadata for OToL study ", attr(x, "study_id"), " . Contents:\n", sep="")
    cat(paste0("  $nexml$", names(x$nexml)), sep="\n")
}

##' @export
##' @rdname get_study_meta
get_tree_ids <- function(sm) UseMethod("get_tree_ids")

##' @export
##' @rdname get_study_meta
get_publication <- function(sm) UseMethod("get_publication")

##' @export
##' @rdname get_study_meta
candidate_for_synth <- function(sm) UseMethod("candidate_for_synth")

##' @export
##' @rdname get_study_meta
get_tree_ids.study_meta <- function(sm) {
    ## only keep the number of the ID
    st_id <- gsub("[^0-9]", "", sm[["nexml"]][["^ot:studyId"]])
    unlist(sm[["nexml"]][["treesById"]][[paste0("trees", st_id)]][["^ot:treeElementOrder"]])
}

##' @export
##' @rdname get_study_meta
get_publication.study_meta <- function(sm) {
    pub <- sm[["nexml"]][["^ot:studyPublicationReference"]]
    attr(pub, "DOI") <- sm[["nexml"]][["^ot:studyPublication"]][["@href"]]
    pub
}

##' @export
##' @rdname get_study_meta
candidate_for_synth.study_meta <- function(sm) {
    unlist(sm[["nexml"]][["^ot:candidateTreeForSynthesis"]])
}

##' Retrieve subtree from a specific tree in the Open Tree of Life data store
##'
##' @title Study subtree
##' @param study_id the study identifier (character)
##' @param tree_id the tree identifier (character)
##' @param object_format the class of the object returned by the
##'     function (default, and currently only possibility \code{phylo}
##'     from the \code{\link[ape]{ape}} package)
##' @param file_format character, the file format to use to save the
##'     results of the query (possible values, \sQuote{newick},
##'     \sQuote{nexus}, \sQuote{json}).
##' @param file character, the path and file name where the output
##'     should be written.
##' @param subtree_id, either a node id that specifies a subtree or
##'     \dQuote{ingroup} which returns the ingroup for this subtree.
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @export
##' @importFrom jsonlite toJSON
##' @examples
##' \dontrun{
##' small_tr <- get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="node552052")
##' ingroup  <- get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="ingroup")
##' nexus_file <- tempfile(fileext=".nex")
##' get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="ingroup", file=nexus_file,
##'                   file_format="nexus")
##' }
get_study_subtree <- function(study_id, tree_id, subtree_id, object_format=c("phylo"),
                              file_format, file, ...) {
    object_format <- match.arg(object_format)
    if (!missing(file)) {
        if (!missing(file_format)) {
            if (missing(file)) stop("You must specify a file to write your output")
            file_format <- match.arg(file_format, c("newick", "nexus", "json"))
            res <- .get_study_subtree(study_id = study_id, tree_id = tree_id,
                                      subtree_id = subtree_id, format=file_format, ...)
            unlink(file)
            if (identical(file_format, "json")) {
                cat(jsonlite::toJSON(res), file=file)
            } else {
                cat(res, file=file)
            }
            return(invisible(file.exists(file)))
        } else {
            stop(sQuote("file_format"), " must be specified.")
        }
    } else if (identical(object_format, "phylo")) {
        file_format <- "newick"
        res <-  .get_study_subtree(study_id = study_id, tree_id = tree_id,
                                   subtree_id = subtree_id, format=file_format, ...)
        res <- phylo_from_otl(res)
        ## NeXML should be possible for both object_format and file_format but it seems there
        ## is something wrong with the server at this time (FM - 2015-06-07)
        ## } else if (identical(object_format, "nexml")) {
        ##    file_format <- "nexml"
        ##    res <- .get_study_subtree(study_id, tree_id, subtree_id, format=file_format)
        ##    res <- nexml_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}
