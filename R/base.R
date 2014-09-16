
otl_url <- function() { "http://devapi.opentreeoflife.org/v2/" }

otl_parse <- function(req) {
    txt <- content(req, as="text")
    if (identical(txt, "")) stop("No output to parse", call. = FALSE)
    jsonlite::fromJSON(txt, simplifyVector=FALSE)
}

otl_check <- function(req) {
    if (req$status_code < 400) return(invisible())

    msg <- otl_parse(req)$message
    stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

otl_GET <- function(path, ...) {
    ur <- paste0(otl_url(), paste(path, collapse="/"))
    req <- GET(ur, ...)
    otl_check(req)
    req
}

otl_POST <- function(path, body, ...) {
    stopifnot(is.list(body))

    body_json <- jsonlite::toJSON(body)

    req <- POST(paste0(otl_url(), paste(path, collapse="/")), body=body_json, ...)
    otl_check(req)

    req
}

##' Basic information about the tree
##'
##' Summary information about the current draft tree of life,
##' including information about the list of trees and the taxonomy
##' used to build it.
##' @title Information about the tree of life
##' @return Some JSON
##' @author Francois Michonneau
##' @export
otl_about <- function() {
    otl_POST("tree_of_life/about", list())
}

##' Reurns the MRCA
##'
##' Most recent common ancestor
##' @title get MRCA
##' @param nds
##' @return the MRCA
##' @author Francois Michonneau
##' @export
otl_mrca <- function(q=list("ott_ids"=c(412129, 536234))) {
    otl_POST(path="tree_of_life/mrca", q)
}

##' returns study for given ID
##'
##' study
##' @title study
##' @param study
##' @return some study
##' @author Francois Michonneau
##' @export
otl_study <- function(study="pg_719") {
    otl_GET(path=c("study", study))
}
