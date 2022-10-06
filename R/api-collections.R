## This endpoint currently returns JSON in XML with mime type as text/html
.collection_find_collections <- function(property = NULL, value = NULL,
                                         verbose = FALSE, ...) {
  check_is_flag(verbose)

  req_body <- list()
  req_body$verbose <- verbose
  res <- otl_POST(
    path = "collections/find_collections",
    body = req_body, ...
  )
  res
}

.collection_properties <- function(...) {
  req_body <- list()
  res <- otl_POST(
    path = "collections/properties",
    body = req_body, ...
  )
  res
}


.get_collection <- function(owner_id = NULL, collection_name = NULL, ...) {
  check_is_string(owner_id)
  check_is_string(collection_name)

  req_body <- list()
  res <- otl_GET(path = paste("collections", owner_id, collection_name,
    sep = "/"
  ), ...)
  res
}
