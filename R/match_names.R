check_args_match_names <- function(response, row_number, taxon_name, ott_id) {
    orig_order <- attr(response, "original_order")
    if (missing(row_number) && missing(taxon_name) && missing(ott_id)) {
        stop("You must specify one of \'row_number\', \'taxon_name\' or \'ott_id\'")
    } else if (!missing(row_number) && missing(taxon_name) && missing(ott_id)) {
        if (!is.numeric(row_number)) stop("\'row_number\' must be a numeric")
        if (row_number > length(orig_order)) {
            stop("\'row_number\' is greater than actual number of rows.")
        }
        i <- orig_order[row_number]
    } else if (missing(row_number) && !missing(taxon_name) && missing(ott_id)) {
        if (!is.character(taxon_name)) stop("\'taxon_name\' must be a character")
        i <- orig_order[match(tolower(taxon_name), response$search_string)]
        if (any(is.na(i))) stop("Can't find ", taxon_name)
    } else if (missing(row_number) && missing(taxon_name) && !missing(ott_id)) {
        if (!is.numeric(ott_id)) stop("\'ott_id\" must be a numeric")
        i <- orig_order[match(ott_id, response$ott_id)]
        if (any(is.na(i))) stop("Can't find ", ott_id)
    } else {
        stop("You must use only one of \'row_number\', \'taxon_name\' or \'ott_id\'")
    }

    if (length(i) > 1) stop("You must supply a single element for each argument.")
    i
}

##' @export
inspect_match_names <- function(response, row_number, taxon_name, ott_id) {

    i <- check_args_match_names(response, row_number, taxon_name, ott_id)

    res <- attr(response, "original_response")
    summary_match <- do.call("rbind", lapply(httr::content(res)$results[[i]]$match, function(x) {
            searchStr <- x$search_string
            uniqNames <- x$unique_name
            approxMatch <- x$is_approximate_match
            ott_id <- x$'ot:ottId'
            isSynonym <- x$is_synonym
            isDeprecated <- x$is_deprecated
            c(searchStr, uniqNames, approxMatch, ott_id, isSynonym, isDeprecated)
        }))
    summary_match <- data.frame(summary_match, stringsAsFactors=FALSE)
    names(summary_match) <- c("search_string", "unique_name", "approximate_match",
                              "ott_id", "is_synonym", "is_deprecated")
    summary_match
}

##' @export
list_synonyms_match_names <- function(response, row_number, taxon_name, ott_id) {
    i <- check_args_match_names(response, row_number, taxon_name, ott_id)

    res <- attr(response, "original_response")
    list_synonyms <- lapply(httr::content(res)$results[[i]]$match, function(x) {
        paste(unlist(x$synonyms), collapse=", ")
    })
    name_synonyms <- lapply(httr::content(res)$results[[i]]$match, function(x) {
        x$unique_name
    })
    names(list_synonyms) <- name_synonyms
    list_synonyms
}

##' @export
update_match_names <- function(response, row_number, taxon_name, ott_id,
                               new_row_number, new_ott_id) {
    i <- check_args_match_names(response, row_number, taxon_name, ott_id)
    res <- attr(response, "original_response")
    tmpRes <- httr::content(res)$results[[i]]

    if (missing(row_number)) {
        if (!missing(taxon_name)) {
            rnb <- match(tolower(taxon_name), response$search_string)
        } else if (!missing(ott_id)) {
            rnb <- match(ott_id, response$ott_id)
        }
    } else {
        rnb <- row_number
    }

    if (missing(new_row_number) && missing(new_ott_id)) {
        stop("You must specify either \'new_row_number\' or \'new_ott_id\'")
    } else if (!missing(new_row_number) && missing(new_ott_id)) {
        if (new_row_number > length(tmpRes$matches))
            stop("\'new_row_number\' is greater than actual number of rows.")
        j <- new_row_number
    } else if (missing(new_row_number) && !missing(new_ott_id)) {
        allOttId <- sapply(tmpRes$matches, function(x) x$'ot:ottId')
        j <- match(new_ott_id, allOttId)
        if (any(is.na(j))) stop("Can't find ", new_ott_id)
    } else {
        stop("You must use only one of \'new_row_number\' or \'new_ott_id\'")
    }
    if (length(j) > 1) stop("You must supply a single element for each argument.")

    searchStr <- tmpRes$matches[[j]]$search_string
    uniqNames <- tmpRes$matches[[j]]$unique_name
    approxMatch <- tmpRes$matches[[j]]$is_approximate_match
    ott_id <- tmpRes$matches[[j]]$'ot:ottId'
    isSynonym <- tmpRes$matches[[j]]$is_synonym
    isDeprecated <- tmpRes$matches[[j]]$is_deprecated
    nMatch <- length(tmpRes$match)
    response[rnb, ] <- c(searchStr, uniqNames, approxMatch, ott_id, nMatch, isSynonym, isDeprecated)
    response
}
