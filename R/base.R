otl_url <- function(dev=FALSE) {
    if (dev) {
        "http://devapi.opentreeoflife.org"
    } else {
        "http://api.opentreeoflife.org"
    }
}

otl_version <- function(version) {
    if (missing(version)) {
        return("v2")
    } else {
        return(version)
    }
}

otl_parse <- function(req) {
    txt <- httr::content(req, as="text")
    if (identical(txt, "")) {
        stop("No output to parse; check your query.", call. = FALSE)
    }
    if (substr(txt, 1, 1) == "{") {
        jsonlite::fromJSON(txt, simplifyVector = FALSE)$description
    } else txt
}

otl_check <- function(req) {
    if (!req$status_code <  400) {
        msg <- otl_parse(req)
        stop("HTTP failure: ", req$status_code, "\n", msg, call. = FALSE)
    }
    otl_check_error(req)
}

otl_GET <- function(path, ...) {
    req <- httr::GET(otl_url(), path=paste(otl_version(), path, sep="/"), ...)
    otl_check(req)
    req
}

otl_POST <- function(path, body, dev_url=FALSE, ...) {
    stopifnot(is.list(body))

    body_json <- ifelse(length(body), jsonlite::toJSON(body), "")

    req <- httr::POST(otl_url(dev=dev_url), path=paste(otl_version(), path, sep="/"), body=body_json, ...)
    otl_check(req)

    req
}

otl_check_error <- function(req) {
    cont <- httr::content(req)
    if (is.list(cont) && exists("error", cont)) {
        stop(paste("Error: ", cont$error, "\n", sep = ""))
    }
}

otl_formats <- function(format) {
    switch(tolower(format),
           "nexus" = ".nex",
           "newick" = ".tre",
           "nexml" = ".nexml",
           "json" = ".json",
           "") #fall through is no extension = nex(j)son
}

## Strip all characters except the ottId from a OpenTree label (internal or terminal)
otl_ottid_from_label <- function(label) {
	return(as.numeric(gsub("(.+[ _]ott)([0-9]+)", "\\2", label)));
}

phylo_from_otl <- function(res) {
    fnm <- tempfile()
    if (is.list(res)) {
        if (!is.null(res$newick)) {
            cat(res$newick, file=fnm)
        } else if (!is.null(res$subtree)) {
            cat(res$subtree, file=fnm)
        } else {
            stop("Cannot find tree")
        }
    } else if (is.character(res)) {
        cat(res, file=fnm)
    } else stop("I don't know how to deal with this format.")
    phy <- rncl::make_phylo(fnm, file.format="newick")
    unlink(fnm)
    return(phy)
}

nexml_from_otl <- function(res) {
    fnm <- tempfile()
    cat(res, file=fnm)
    phy <- RNeXML::nexml_read(x=fnm)
    unlink(fnm)
    phy
}
