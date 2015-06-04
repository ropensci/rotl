##' Return a list of studies that match a given properties
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

studies_find_studies <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE) {
    res <- .studies_find_studies(property, value, verbose, exact)
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

studies_find_trees <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE) {
    res <- .studies_find_trees(property, value, verbose, exact)
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

studies_properties <- function() {
    res <- .studies_properties()
    return(res)
}


##' returns study for given ID
##'
##' study
##' @title study
##' @param study
##' @return some study
##' @author Francois Michonneau
##' @export
##' @examples
##' \dontrun{
##' that_one_study <- get_study(study_id="pg_719", obj_format="phylo")
##' }
# this function should accept nexml (both for object and text)
get_study <- function(study_id=NULL, object_format=c("phylo"),
                      text_format=c("", "nexus", "newick", "json"),
                      file) {

    object_format <- match.arg(object_format)
    text_format <- match.arg(text_format)
    if(text_format == ""){
        text_format <- NULL
    }
    if (!is.null(text_format)) {
        if (missing(file)) stop("You must specify a file to write your output")
        res <- .get_study(study_id, format=text_format)
        if (identical(text_format, "json")) {
            cat(jsonlite::toJSON(res), file=file)
        } else {
            cat(res, file=file)
        }
        invisible(res)
    } else if (identical(object_format, "phylo")) {
        text_format <- "newick"
        res <- .get_study(study_id, format=text_format)
        res <- phylo_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}

##' returns specific tree from a study
##'
##' @title Study Tree
##' @param study char study id
##' @param tree tree id
##' @param format char Tree format (default = json)
##' @return A tree file in desired format
##' @export
##' @examples
##' \dontrun{
##'  nexson_tr <- get_study_tree(study_id="pg_1144", tree="tree2324")
##'}
get_study_tree <- function(study_id=NULL, tree_id=NULL, object_format=c("phylo"),
                           text_format=NULL, file) {
    object_format <- match.arg(object_format)
    if (!is.null(text_format)) {
        text_format <- match.arg(text_format, c("nexus", "newick", "json"))
        if (missing(file)) stop("You must specify a file to write your output")
        text_format <- match.arg(text_format, c("nexus", "newick", "json"))
        res <- .get_study_tree(study_id, tree_id, format=text_format)
        if (identical(text_format, "json")) {
            cat(jsonlite::toJSON(res), file=file)
        } else {
            cat(res, file=file)
        }
        return(invisible(file.exists(file)))
    } else if (identical(object_format, "phylo")) {
        text_format <- "newick"
        res <- .get_study_tree(study_id, tree_id, format=text_format)
        res <- phylo_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}

## get_study_internal <- function(fnx, file, ...) {
##     if (!is.null(text_format)) {
##         if (missing(file)) stop("You must specify a file to write your output")
##         res <- do.call(fnx, list(...))
##         if (identical(text_format, "json")) {
##             cat(jsonlite::toJSON(res), file=file)
##         } else {
##             cat(res, file=file)
##         }
##         invisible(res)
##     } else if (identical(object_format, "phylo")) {
##         text_format <- "newick"
##         res <- do.call(study, tree, format=text_format)
##         res <- phylo_from_otl(res)
##     } else stop("Something is very wrong. Contact us.")
##   res
## }

##' Retrieve metadata about a study in the Open Tree of Life datastore
##' @title Study Metadata
##' @param study character, study id
##' @return named-list json file with metadata
##' @export
##' @examples
##' req <- get_study_meta("pg_719")
##' req$nexml$`^ot:studyPublication`
get_study_meta <- function(study_id) {
   .get_study_meta(study_id)
}

##' Retrieve subtree from a specific tree in the Open Tree of Life data store
##' @param study character, study id
##' @param tree character, tree_id
##' @param subtree_id, either a node id that specifies a subtree or "ingroup"
##' which returns the ingroup is for this subtree, a 400 error otherwise
##' @export
##' @examples
##' \dontrun{
##' small_tr <- get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="node552052")
##' ingroup  <- get_study_subtree(study_id="pg_1144", tree="tree2324", subtree_id="ingroup")
##' }
get_study_subtree <- function(study_id, tree_id, subtree_id, object_format=c("phylo"),
                              text_format=NULL, file) {
    ## NeXML should be possible for both object_format and text_format but it seems there
    ## is something wrong with the server at this time (FM - 2014-09-19)
    object_format <- match.arg(object_format)
    if (!is.null(text_format)) {
        if (missing(file)) stop("You must specify a file to write your output")
        text_format <- match.arg(text_format, c("newick", "nexus", "json"))
        res <- .get_study_subtree(study_id, tree_id, subtree_id, format=text_format)
        if (identical(text_format, "json")) {
            cat(jsonlite::toJSON(res), file=file)
        } else {
            cat(res, file=file)
        }
        invisible(res)
    } else if (identical(object_format, "phylo")) {
        text_format <- "newick"
        res <-  .get_study_subtree(study_id, tree_id, subtree_id, format=text_format)
        res <- phylo_from_otl(res)
    } else if (identical(object_format, "nexml")) {
        text_format <- "nexml"
        res <- .get_study_subtree(study_id, tree_id, subtree_id, format=text_format)
        res <- nexml_from_otl(res)
    } else stop("Something is very wrong. Contact us.")
    res
}

get_study_otu <- function(study_id, otu=NULL){
    otl_GET(path=paste("study", study_id, "otu", otu, sep="/"))
}

get_study_otus <- function(study_id, otus) {
    otl_GET(path=paste("study", study_id, "otu", otus, sep="/"))
}

get_study_otumap <- function(study_id){
    otl_GET(path=paste("study", study_id,"otumap", sep="/"))
}
