##' Return a list of studes that match a given properties
##' @title find_study
##' @param exact Boolean, exact matching (default = FALSE)
##' @param property character, the property to be searched on
##' @param value character, the property-value  to be searched on
##' @param verbose Boolean, include all metadata (default=FALSE)
##' @examples
##' all_s <- studies_find_studies()
##' length(all_s$

studies_find_studies <- function(property, value, verbose=FALSE, exact=FALSE){
    otl_POST(path="studies/find_studies/", body=list(property=property,
                                                     value=value,
                                                     verbose=otl_bool(verbose),
                                                     exact=otl_bool(exact)))
}

#studies_find_tree
#studies_properties




##' returns study for given ID
##'
##' study
##' @title study
##' @param study
##' @return some study
##' @author Francois Michonneau
##' @export
get_study <- function(study="pg_719") {
    otl_GET(path=paste("study", study, sep="/"))
}

##TODO: Tree functions takes several optional args, as discussed
## https://github.com/OpenTreeOfLife/phylesystem-api/blob/docv2/docs/README.md#fine-grained-access-via-get
##' returns specific treee from a study
##'
##' @title Study Tree
##' @param study char study id
##" @param tree tree id
##' @param format char Tree format (default = json)
##' @return A tree in desired format


get_study_tree <- function(study="pg_1144", tree="tree2324", ...){
    otl_GET(path=paste("study", study, "tree", tree, sep="/"))
}
