
otl_url <- function() { "http://api.opentreeoflife.org" }

otl_version <- function() { "v2" }

otl_parse <- function(req) {
    txt <- httr::content(req, as="text")
    if (identical(txt, "")) stop("No output to parse", call. = FALSE)
    jsonlite::fromJSON(txt, simplifyVector=FALSE)
}

otl_check <- function(req) {
    if (!req$status_code < 400) {
    	    msg <- otl_parse(req)$message
        stop("HTTP failure: ", req$status_code, "\n", msg, call. = FALSE)
    }
    otl_check_error(req)
}

otl_GET <- function(path, ...) {
    req <- httr::GET(otl_url(), path=paste(otl_version(), path, sep="/"), ...)
    otl_check(req)
    req
}

otl_POST <- function(path, body, ...) {
    stopifnot(is.list(body))

    body_json <- ifelse(length(body), jsonlite::toJSON(body), "")

    req <- httr::POST(otl_url(), path=paste(otl_version(), path, sep="/"), body=body_json, ...)
    otl_check(req)

    req
}

otl_check_error <- function(req) {
	cont <- httr::content(req)
	if (exists("error", cont)) {
		stop(paste("Error: ", cont$error, "\n", sep = ""))
	}
}

otl_formats <- function(format){
    switch(tolower(format), 
           "nexus" = ".nex",
           "newick" = ".tre",
           "nexml" = ".nexml",
           "")#fall through is no extension = nex(j)son
}


