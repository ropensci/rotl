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
