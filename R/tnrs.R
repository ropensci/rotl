
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
##' @param names taxon names to be queried (character vector)
##' @param context_name name of the taxonomic context to be searched
##' (length-one character vector)
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
##' @examples \dontrun{
##'   deuterostomes <- tnrs_match_names(names=c("echinodermata", "xenacoelomorpha", "chordata", "hemichordata"))
##' }
##' @export
tnrs_match_names <- function(names=NULL, context_name=NULL, do_approximate_matching=TRUE,
                             ids=NULL, include_deprecated=FALSE, include_dubious=FALSE) {

    if (!is.null(context_name) &&
        !context_name %in% unlist(tnrs_contexts())) {
        stop("The \'context_name\' is not valid. Check possible values using tnrs_contexts()")
    }

    res <- .tnrs_match_names(names, context_name, do_approximate_matching,
                             ids, include_deprecated, include_dubious)
    check_tnrs(res)
    summary_match <- do.call("rbind", lapply(res$results, function(x) {
        searchStr <- x$matches[[1]]$search_string
        uniqNames <- x$matches[[1]]$unique_name
        approxMatch <- x$matches[[1]]$is_approximate_match
        ott_id <- x$matches[[1]]$'ot:ottId'
        isSynonym <- x$matches[[1]]$is_synonym
        isDeprecated <- x$matches[[1]]$is_deprecated
        nMatch <- length(x$match)
        c(searchStr, uniqNames, approxMatch, ott_id, nMatch, isSynonym, isDeprecated)
    }))
    summary_match <- data.frame(summary_match, stringsAsFactors=FALSE)
    names(summary_match) <- c("search_string", "unique_name", "approximate_match",
                              "ott_id", "number_matches", "is_synonym", "is_deprecated")
    summary_match$search_string <- gsub("\\\\", "", summary_match$search_string)
    summary_match <- summary_match[match(tolower(names), summary_match$search_string), ]
    attr(summary_match, "original_order") <- as.numeric(rownames(summary_match))
    rownames(summary_match) <- NULL
    attr(summary_match, "original_response") <- res
    summary_match
}

check_tnrs <- function(req) {
    if (length(req$results) < 1) {
        warning("Nothing returned")
    }
    if (length(req$unmatched_name_ids)) {
        warning(paste(req$unmatched_name_ids, collapse=", "), " are not matched")
    }
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
    res <- .tnrs_contexts()
    return(res)
}


## *** tnrs_contexts needs a summary function ***



##' @title Infer context
##' @description Return a taxonomic context given a list of taxonomic names
##' @details Find the least inclusive taxonomic context that includes all the
##' unambiguous names in the input set. Unambiguous names are names
##' with exact matches to non-homonym taxa. Ambiguous names (those
##' without exact matches to non-homonym taxa) are indicated in
##' results.
##' @param names Vector of taxon names.
##' @return something
##' @author Francois Michonneau
##' @examples
##' res <- tnrs_infer_context(names=c("Stellula calliope", "Struthio camelus"))
##' @export
tnrs_infer_context <- function(names=NULL) {
    res <- .tnrs_infer_context(names)
    return(res)
}


get_synonyms <- function(req, i) {
    cont <- httr::content(req)
    sapply(cont$results[[i]]$matches, function(x) x$unique_name)
}
