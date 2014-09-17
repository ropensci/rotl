
##' Match names
##'
##' Accepts one or more taxonomic names and returns information about
##' potential matches for these names to known taxa in OTT. This
##' service uses taxonomic contexts to disambiguate homonyms and
##' misspelled names; a context may be specified using the
##' context_name parameter. If no context is specified, then the
##' context will be inferred: the shallowest taxonomic context that
##' contains all unambiguous names in the input set will be used. A
##' name is considered unambiguous if it is not a synonym and has only
##' one exact match to any taxon name in the entire taxonomy.
##' Taxonomic contexts are uncontested higher taxa that have been
##' selected to allow limits to be applied to the scope of TNRS
##' searches (e.g. 'match names only within flowering plants'). Once a
##' context has been identified (either user-specified or inferred),
##' all taxon name matches will performed only against taxa within
##' that context. For a list of available taxonomic contexts, see the
##' contexts service.
##' @title Match names
##' @param taxon_names taxon names to be queried (character vector)
##' @param context_name name of the taxonomic context to be searched
##' (character vector)
##' @param do_approximate_matching A boolean indicating whether or not
##' to perform approximate string (a.k.a. "fuzzy") matching. Will
##' greatly improve speed if this is turned OFF (false). By default,
##' however, it is on (true).
##' @param ids An array of ids to use for identifying names. These
##' will be assigned to each name in the names array. If ids is
##' provided, then ids and names must be identical in length.
##' @param include_deprecated A boolean indicating whether or not to
##' include deprecated taxa in the search.
##' @param include_dubious Whether to include so-called 'dubious'
##' taxa--those which are not accepted by OTT.
##' @return something
##' @author Francois Michonneau
##' @export
tnrs_match_names <- function(taxon_names, context_name=NULL, do_approximate_matching=TRUE,
                             ids=NULL, include_deprecated=FALSE, include_dubious=FALSE) {
    if (missing(taxon_names)) {
        stop("\'names\' must be specified")
    }
    if (!is.null(ids)) {
        if (length(ids) != length(taxon_names))
            stop("\'ids\' and \'taxon_names\' must be of the same length.")
    }
    if (!is.logical(do_approximate_matching)) stop("Argument \'do_approximate_matching\' should be logical")
    if (!is.logical(include_deprecated)) stop("Argument \'include_deprecated\' should be logical")
    if (!is.logical(include_dubious)) stop("Argument \'include_dubious\' should be logical")

    q <- list(names = taxon_names, context_name = context_name,
              do_approximate_matching = jsonlite::unbox(do_approximate_matching),
              ids = ids, include_deprecated = jsonlite::unbox(include_deprecated),
              include_dubious = jsonlite::unbox(include_dubious))
    toKeep <- sapply(q, is.null)
    q <- q[!toKeep]

    res <- otl_POST("tnrs/match_names", q)
    check_tnrs(res)
    summary_match <- do.call("rbind", lapply(httr::content(res)$results, function(x) {
        searchStr <- x$matches[[1]]$search_string
        uniqNames <- x$matches[[1]]$unique_name
        approxMatch <- x$matches[[1]]$is_approximate_match
        ottId <- x$matches[[1]]$'ot:ottId'
        isSynonym <- x$matches[[1]]$is_synonym
        isDeprecated <- x$matches[[1]]$is_deprecated
        nMatch <- length(x$match)
        c(searchStr, uniqNames, approxMatch, ottId, nMatch, isSynonym, isDeprecated)
    }))
    summary_match <- data.frame(summary_match, stringsAsFactors=FALSE)
    names(summary_match) <- c("search_string", "unique_name", "approximate_match",
                              "ottId", "number_matches", "is_synonym", "is_deprecated")
    summary_match <- summary_match[match(tolower(taxon_names), summary_match$search_string), ]
    rownames(summary_match) <- NULL
    attr(summary_match, "original_response") <- res
    summary_match
}

##' @export
inspect_match_names <- function(response, i) {
    res <- attr(response, "original_response")
    summary_match <- do.call("rbind", lapply(httr::content(res)$results[[i]]$match, function(x) {
            searchStr <- x$search_string
            uniqNames <- x$unique_name
            approxMatch <- x$is_approximate_match
            ottId <- x$'ot:ottId'
            isSynonym <- x$is_synonym
            isDeprecated <- x$is_deprecated
            c(searchStr, uniqNames, approxMatch, ottId, isSynonym, isDeprecated)
        }))
    summary_match <- data.frame(summary_match, stringsAsFactors=FALSE)
    names(summary_match) <- c("search_string", "unique_name", "approximate_match",
                              "ottId", "is_synonym", "is_deprecated")
    summary_match
}

##' @export
list_synonyms_match_names <- function(response, i) {
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
update_match_names <- function(response, i, j) {
    res <- attr(response, "original_response")
    tmpRes <- httr::content(res)$results[[i]]
    searchStr <- tmpRes$matches[[j]]$search_string
    uniqNames <- tmpRes$matches[[j]]$unique_name
    approxMatch <- tmpRes$matches[[j]]$is_approximate_match
    ottId <- tmpRes$matches[[j]]$'ot:ottId'
    isSynonym <- tmpRes$matches[[j]]$is_synonym
    isDeprecated <- tmpRes$matches[[j]]$is_deprecated
    nMatch <- length(tmpRes$match)
    response[i, ] <- c(searchStr, uniqNames, approxMatch, ottId, nMatch, isSynonym, isDeprecated)
    response
}


##' Return a list of pre-defined taxonomic contexts (i.e. clades),
##' which can be used to limit the scope of tnrs queries.
##'
##' Taxonomic contexts are available to limit the scope of TNRS
##' searches. These contexts correspond to uncontested higher taxa
##' such as 'Animals' or 'Land plants'. This service returns a list
##' containing all available taxonomic context names, which may be
##' used as input (via the context_name parameter) to limit the search
##' scope of other services including match_names and
##' autocomplete_name.
##' @title tnrs context
##' @return something
##' @author Francois Michonneau
##' @export
tnrs_contexts <- function() {
    otl_POST("tnrs/contexts", list())
}


##' Return a taxonomic context given a list of taxonomic names.
##'
##' Find the least inclusive taxonomic context that includes all the
##' unambiguous names in the input set. Unambiguous names are names
##' with exact matches to non-homonym taxa. Ambiguous names (those
##' without exact matches to non-homonym taxa) are indicated in
##' results.
##' @title infer context
##' @param taxon_names
##' @return something
##' @author Francois Michonneau
##' @export
tnrs_infer_context <- function(taxon_names=NULL) {
    if (is.null(taxon_names)) {
        stop("\'taxon_names\' must be specified")
    }
    q <- list(names=taxon_names)
    otl_POST("tnrs/infer_context", q)
}

check_tnrs <- function(req) {
    cont <- httr::content(req)
    if (length(cont$results) < 1) {
        warning("Nothing returned")
    }
    if (length(cont$unmatched_name_ids)) {
        warning(paste(cont$unmatched_name_ids, collapse=", "), " are not matched")
    }
}


get_synonyms <- function(req, i) {
    cont <- httr::content(req)
    sapply(cont$results[[i]]$matches, function(x) x$unique_name)
}
