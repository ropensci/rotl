
otl_url <- function() { "http://api.opentreeoflife.org" }

otl_version <- function() { "v2" }

otl_parse <- function(req) {
    txt <- httr::content(req, as="text")
    if (identical(txt, "")) stop("No output to parse", call. = FALSE)
    jsonlite::fromJSON(txt, simplifyVector=FALSE)
}

otl_check <- function(req) {
    if (req$status_code < 400) return(invisible())

    msg <- otl_parse(req)$message
    stop("HTTP failure: ", req$status_code, "\n", msg, call. = FALSE)
}

otl_GET <- function(path, ...) {
    req <- httr::GET(otl_url(), path=paste(otl_version(), path, sep="/"), ...)
    otl_check(req)
    req
}

otl_POST <- function(path, body, ...) {
    stopifnot(is.list(body))

    body_json <- ifelse(length(body), jsonlite::toJSON(body, auto_unbox=TRUE), "")

    req <- httr::POST(otl_url(), path=paste(otl_version(), path, sep="/"), body=body_json, ...)
    otl_check(req)

    req
}

otl_bool <- function(bool) ifelse(bool, "true", "false")
