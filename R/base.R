otl_url <- function(dev=FALSE) {
    if (dev) {
        "https://devapi.opentreeoflife.org"
    } else {
        "https://api.opentreeoflife.org"
    }
}

otl_version <- function(version) {
    if (missing(version)) {
        return("v2")
    } else {
        return(version)
    }
}

##' @importFrom httr content
##' @importFrom jsonlite fromJSON
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
    otl_check_error(req)
    if (!req$status_code <  400) {
        msg <- otl_parse(req)
        stop("HTTP failure: ", req$status_code, "\n", msg, call. = FALSE)
    }
}

##' @importFrom httr GET
otl_GET <- function(path, dev_url = FALSE, otl_v = otl_version(), ...) {
    req <- httr::GET(otl_url(), path=paste(otl_v, path, sep="/"), ...)
    otl_check(req)
    req
}

##' @importFrom jsonlite toJSON
##' @importFrom httr POST
otl_POST <- function(path, body, dev_url = FALSE, otl_v = otl_version(), ...) {
    stopifnot(is.list(body))

    body_json <- ifelse(length(body), jsonlite::toJSON(body), "")

    req <- httr::POST(otl_url(dev = dev_url),
                      path=paste(otl_v, path, sep="/"),
                      body=body_json, ...)
    otl_check(req)
    req
}

##' @importFrom httr content
otl_check_error <- function(req) {
    cont <- httr::content(req)
    if (is.list(cont)) {
        if (exists("error", cont)) {
            stop(paste("Error: ", cont$error, "\n", sep = ""))
        } else if (exists("message", cont)) {
            stop(paste("Message: ", cont$message, "\n", sep = ""))
        }
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

##' @importFrom rncl read_newick_phylo
phylo_from_otl <- function(res, dedup = FALSE) {
    if (is.list(res)) {
        if (!is.null(res$newick)) {
            tree <- res$newick
        } else if (!is.null(res$subtree)) {
            tree <- res$subtree
        } else {
            stop("Cannot find tree")
        }
    } else if (is.character(res)) {
        tree <- res
    } else stop("I don't know how to deal with this format.")
    if (grepl("\\(", tree)) {
        fnm <- tempfile()
        cat(tree, file = fnm)
        if (!dedup) {
            phy <- rncl::read_newick_phylo(fnm)
        } else {
            dedup_tr <- deduplicate_labels(fnm)
            phy <- rncl::read_newick_phylo(dedup_tr)
            unlink(dedup_tr)
        }
        unlink(fnm)
    } else {
        phy <- tree_to_labels(tree)$tip_label
    }
    return(phy)
}


nexml_from_otl <- function(res) {
    if (!requireNamespace("RNeXML", quietly = TRUE)) {
        stop("The RNeXML package is needed to use the nexml file format")
    }
    fnm <- tempfile()
    cat(res, file=fnm)
    phy <- RNeXML::nexml_read(x=fnm)
    unlink(fnm)
    phy
}

## check if the argument provided looks like a number (can be coerced
## to integer/numeric).
check_numeric <- function(x) {
    if (length(x) != 1) {
        stop("only 1 element should be provided")
    }
    if (!is.numeric(x)) {
        x <- as.character(x)
        if (any(is.na(x))) return(FALSE)
        return(grepl("^[0-9]+$", x))
    } else {
        return(x %% 1 == 0)
    }
}
