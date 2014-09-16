##' Return a list of studes that match a given properties
##' @title find_study
##' @param exact Boolean, exact matching (default = FALSE)
##' @param property character, the property to be searched on
##' @param value character, the property-value  to be searched on
##' @param verbose Boolean, include all metadata (default=FALSE)
##' @export
##' @examples
##' req <- studies_find_studies(property="ot:studyId", value="pg_719")
##' httr::content(req)

studies_find_studies <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE){
    req_body <- list()
    if(!is.null(property)){
        req_body$property <- property
    }
    if(!is.null(value)){
        req_body$value <- value
    }
    otl_POST(path="studies/find_studies/", body=c(req_body,
                                                  jsonlite::unbox(verbose),
                                                  jsonlite::unbox(exact)))
}

##' Return a list of trees that match a given properties
##' @title find trees
##' @param property character, the property to be searched on
##' @param value character, the property-value  to be searched on
##' @param verbose Boolean, include all metadata (default=FALSE)
##' @param exact Boolean, exact matching (default = FALSE)
##' @export
##' @examples
##' req <- studies_find_trees(property="ot:ottTaxonName", value="Garcinia")
##' httr::content(req)


studies_find_trees <- function(property=NULL, value=NULL, verbose=FALSE, exact=FALSE){
    req_body <- list()
    if(!is.null(property)){
        req_body$property <- property
    }
    if(!is.null(value)){
        req_body$value <- value
    }
    otl_POST(path="studies/find_trees/",   body=c(req_body,
                                                  jsonlite::unbox(verbose),
                                                  jsonlite::unbox(exact)))
}


##' returns properties on which studies and study-trees can be searched

studies_properties <- function(){
    otl_POST(path="studies/properties/", body=list())
}




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
