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
