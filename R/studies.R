##' Return a list of studies that match given properties
##' @title find_study
##' @param exact Boolean, exact matching (default = FALSE)
##' @param property character, the property to be searched on
##' @param value character, the property-value  to be searched on
##' @param verbose Boolean, include all metadata (default=FALSE)
##' @seealso \code{\link{studies_properties}} which lists properties against
##' which the studies can be searched
##' @export
##' @examples
##' study <- studies_find_studies(property="ot:studyId", value="pg_719")

studies_find_studies <- function(property=NULL, value=NULL, verbose=FALSE,
                                 exact=FALSE, ...) {
    res <- .studies_find_studies(property = property, value = value,
                                 verbose = verbose, exact = exact, ...)
    return(res)
}

##' Return a list of trees that match a given properties
##' @title find trees
##' @param property character, the property to be searched on
##' @param value character, the property-value  to be searched on
##' @param verbose Boolean, include all metadata (default=FALSE)
##' @param exact Boolean, exact matching (default = FALSE)
##' @seealso \code{\link{studies_properties}} which lists properties against
##' which the studies can be searched
##' @export
##' @examples
##' res <- studies_find_trees(property="ot:ottTaxonName", value="Garcinia")

studies_find_trees <- function(property=NULL, value=NULL, verbose=FALSE,
                               exact=FALSE, ...) {
    res <- .studies_find_trees(property = property, value = value,
                               verbose = verbose, exact = exact, ...)
    return(res)
}

##' Property of a study
##' @title studies properties
##' @return something
##' @author Francois Michonneau
##' @export
##' @examples
##'  all_the_properties <- studies_properties()
##'  unlist(all_the_properties$tree_properties)

studies_properties <- function(...) {
    res <- .studies_properties(...)
    return(res)
}


##' returns study for given ID
##'
##' study
##' @param study_id
##' @param object_format
##' @param file_format
##' @param file
##' @return some study
##' @author Francois Michonneau
##' @export
##' @examples
##' \dontrun{
##' that_one_study <- get_study(study_id="pg_719", object_format="phylo")
##' }
get_study <- function(study_id = NULL, object_format = c("phylo", "nexml"),
                      file_format = NULL, file, ...) {
    object_format <- match.arg(object_format)
    if (!is.null(file_format)) {
        if (missing(file)) stop("You must specify a file to write your output")
        file_format <- match.arg(file_format, c("newick", "nexus", "nexml", "json"))
        res <- .get_study(study_id, format = file_format)
        if (identical(file_format, "json")) {
            cat(jsonlite::toJSON(res), file=file)
        } else {
            cat(res, file=file)
        }
        return(invisible(file.exists(file)))
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

##' returns specific tree from a study
##'
##' @title Study Tree
##' @param study_id
##' @param tree_id
##' @param object_format
##' @param tip_label
##' @param file_format
##' @param file
##' @param format char Tree format (default = json)
##' @return A tree file in desired format
##' @export
##' @examples
##' \dontrun{
##'  nexson_tr <- get_study_tree(study_id="pg_1144", tree="tree2324")
##'}
get_study_tree <- function(study_id=NULL, tree_id=NULL, object_format=c("phylo"),
                           tip_label = c("original_label", "ott_id", "ott_taxon_name"),
                           file_format=NULL, file, ...) {

    object_format <- match.arg(object_format)
    tip_label <- match.arg(tip_label)
    tip_label <- switch(tip_label,
                        original_labels = "ot:originallabel",
                        ott_id =  "ot:ottid",
                        ott_taxon_name = "ot:otttaxonname")
    if (!is.null(file_format)) {
        file_format <- match.arg(file_format, c("nexus", "newick", "json"))
        if (missing(file)) stop("You must specify a file to write your output")
        res <- .get_study_tree(study_id = study_id, tree_id = tree_id,
                               format=file_format, tip_label = tip_label, ...)
        if (identical(file_format, "json")) {
            cat(jsonlite::toJSON(res), file=file)
        } else {
            cat(res, file=file)
        }
        return(invisible(file.exists(file)))
    } else if (identical(object_format, "phylo")) {
        file_format <- "newick"
        res <- .get_study_tree(study_id = study_id, tree_id = tree_id,
                               format=file_format, tip_label = tip_label, ...)
        res <- phylo_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}

##' Retrieve metadata about a study in the Open Tree of Life datastore
##' @title Study Metadata
##' @param study_id character, study id
##' @return named-list json file with metadata
##' @export
##' @examples
##' req <- get_study_meta("pg_719")
##' req$nexml$`^ot:studyPublication`
get_study_meta <- function(study_id, ...) {
   .get_study_meta(study_id = study_id, ...)
}

##' Retrieve subtree from a specific tree in the Open Tree of Life data store
##' @param study_id character, the study id
##' @param tree_id character,  the tree id
##' @param object_format character, the class of the object returned
##' by the function (default, and currently only possibility
##' \code{phylo} from the APE package)
##' @param file_format character, the file format to use to save the
##' results of the query.
##' @param file character, the path and file name where the output
##' should be written.
##' @param subtree_id, either a node id that specifies a subtree or "ingroup"
##' which returns the ingroup is for this subtree, a 400 error otherwise
##' @export
##' @examples
##' \dontrun{
##' small_tr <- get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="node552052")
##' ingroup  <- get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="ingroup")
##' }
get_study_subtree <- function(study_id, tree_id, subtree_id, object_format=c("phylo"),
                              file_format=NULL, file, ...) {
    object_format <- match.arg(object_format)
    if (!is.null(file_format)) {
        if (missing(file)) stop("You must specify a file to write your output")
        file_format <- match.arg(file_format, c("newick", "nexus", "json"))
        res <- .get_study_subtree(study_id = study_id, tree_id = tree_id,
                                  subtree_id = subtree_id, format=file_format, ...)
        if (identical(file_format, "json")) {
            cat(jsonlite::toJSON(res), file=file)
        } else {
            cat(res, file=file)
        }
        return(invisible(file.exists(file)))
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
