##' Some valid taxonomic names do not occur in the Synthetic
##' Tree. This convenience function allows you to check whether a
##' given Open Tree Taxonomy identifier (OTT id) is in the tree. A taxonomic
##' name may not occur in the synthetic tree because (1) it is an
##' extinct or invalid taxon, or (2) it is part of a group that is not
##' monophyletic in the tree.
##'
##' @title Check that OTT ids occur in the Synthetic Tree
##' @param ott_ids a vector of Open Tree Taxonomy identifiers
##' @param ...  additional arguments to customize the API request (see
##'     \code{\link{rotl}} package documentation).
##' @return A named logical vector. \code{TRUE} indicates that the OTT
##'     id is in the synthetic tree, and \code{FALSE} that it is not.
##' @examples
##' \dontrun{
##'   plant_families <- c("Asteraceae", "Solanaceae", "Poaceae", "Amaranthaceae",
##'                       "Zamiaceae", "Araceae", "Juncaceae")
##'   matched_names <- tnrs_match_names(plant_families)
##'   ## This fails because some ott ids are not in the tree
##'   ## plant_tree <- tol_induced_subtree(ott_id(matched_names))
##'   ## So let's check which ones are actually in the tree first:
##'   in_tree <- is_in_tree(ott_id(matched_names))
##'   ## This now works:
##'   plant_tree <- tol_induced_subtree(ott_id(matched_names)[in_tree])
##' }
##'
##' @export
is_in_tree <- function(ott_ids, ...) UseMethod("is_in_tree")


##' @export
is_in_tree.otl_ott_id <- function(ott_ids, ...) {

    in_tree <- vapply(ott_ids, function(ottid) {
        test <- try(tol_node_info(ottid, ...), silent = TRUE)
        if (inherits(test, "try-error")) {
            if (grepl("not find any synthetic tree nodes corresponding to the OTT id provided", test) &&
                grepl(paste0("(", ottid, ")"), test)) {
            } else {
                warning("something seems off, check your internet connection?")
            }
            return(FALSE)
        } else {
            ott_id(test)[[1]] == ottid
        }

    }, logical(1), USE.NAMES = TRUE)

    in_tree
}

##' @export
is_in_tree.default <- function(ott_ids, ...) {
    chk_ids <- vapply(ott_ids, check_numeric, logical(1))
    if (all(chk_ids))
        is_in_tree.otl_ott_id(ott_ids)
    else
        stop("Invalid format for ott_ids (they should look like numbers)")
}
