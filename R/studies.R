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
##' req <- studies_find_studies(property="ot:studyId", value="pg_719")
##' httr::content(req)

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
##' req <- studies_find_trees(property="ot:ottTaxonName", value="Garcinia")
##' length(httr::content(req)$matched_studies)

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
##'  prop_list = httr::content(all_the_properties)
##'  unlist(prop_list$tree_properties)

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
##' that_one_study <- get_study(study="pg_719")
get_study <- function(study=NULL, format=c("", "nexus", "newick", "nexml", "json")) {
    res <- .get_study(study, format)
    return(res)
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
##'  nexson_tr <- get_study_tree(study="pg_1144", tree="tree2324")
get_study_tree <- function(study=NULL, tree=NULL, format=c("", "nexus", "newick", "json")) {
    res <- .get_study_tree(study, tree, format)
    return(res)
}

cho_get_study_tree <- function()

##' Retrieve metadata about a study in the Open Tree of Life datastore
##' @title Study Metadata
##' @param study character, study id
##' @return named-list json file with metadata
##' @export
##' @examples
##' req <- get_study_meta("pg_719")
##' req_list <- httr::context(req)
##' req_lsit$nexml$`^ot:studyPublication`

get_study_meta <- function(study){
    otl_GET(path= paste("study", study, "meta", sep="/"))
}

##' Retrieve subtree from a specific tree in the Open Tree of Life data store
##' @param study character, study id
##' @param tree character, tree_id
##' @param subtree_id, either a node id that specifies a subtree or "ingroup"
##' which returns the ingroup is for this subtree, a 400 error otherwise
##' @export
##' @examples
##' small_tr <- get_study_subtree(study="pg_1144", tree="tree2324", subtree_id="node552052")
##' ingroup  <- get_study_subtree(study="pg_1144", tree="tree2324", subtree_id="ingroup")

get_study_subtree <- function(study, tree, subtree_id){
    url_stem <- paste("study", study, "tree", tree, sep="/")
    otl_GET(path=paste(url_stem, "?subtree_id=", subtree_id, sep=""))
}

get_study_otu <- function(study, otu=NULL){
    otl_GET(path=paste("study", study, "otu", otu, sep="/"))
}

get_study_otus <- function(study, otus) {
    otl_GET(path=paste("study", study, "otu", otus, sep="/"))
}

get_study_otumap <- function(study){
    otl_GET(path=paste("study", study,"otumap", sep="/"))

}
