##' @importFrom assertthat assert_that is.flag
## This endpoint currently returns JSON in XML with mime type as text/html
.collection_find_collections <- function(property = NULL, value = NULL,
                                         verbose = FALSE, ...) {
    assertthat::assert_that(assertthat::is.flag(verbose))
    req_body <- list()
    req_body$verbose <- verbose
    res <- otl_POST(path = "collections/find_collections",
                    body = req_body, ...)
    res
}

.collection_properties <- function(...) {
    req_body <- list()
    res <- otl_POST(path = "collections/properties",
                    body = req_body, ...)
    res
}


.get_collection <- function(owner_id = NULL, collection_name = NULL, ...) {
    assertthat::assert_that(assertthat::is.string(owner_id))
    assertthat::assert_that(assertthat::is.string(collection_name))
    req_body <- list()
    res <- otl_GET(path = paste("collections", owner_id, collection_name,
                                sep = "/"), ...)
    res
}
