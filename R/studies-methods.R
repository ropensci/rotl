
### list_trees -----------------------------------------------------------------

##' List trees ids in objects returned by
##' \code{\link{studies_find_studies}} and
##' \code{\link{studies_find_trees}}.
##'
##' \code{list_trees} returns all trees associated with a particular
##' study when used on an object returned by
##' \code{\link{studies_find_studies}}, but only the trees that match
##' the search criteria when used on objects returned by
##' \code{\link{studies_find_trees}}.
##'
##' @param matched_studies an object created by
##'     \code{studies_find_trees} or \code{studies_find_studies}.
##' @param study_id a \code{study_id} listed in the object returned by
##'     \code{studies_find_trees}
##' @param ... Currently unused
##' @return \code{list_trees} returns a list of the tree_ids for each
##'     study that match the requested criteria. If a \code{study_id}
##'     is provided, then only the trees for this study are returned
##'     as a vector.
##' @seealso \code{\link{studies_find_studies}} and
##'     \code{\link{studies_find_trees}}. The help for these functions
##'     have examples demonstrating the use of \code{list_trees}.
##' @export
list_trees <- function(matched_studies, ...) UseMethod("list_trees")

##' @rdname list_trees
##' @export
list_trees.matched_studies <- function(matched_studies, study_id, ...) {
  res <- attr(matched_studies, "found_trees")
  if (missing(study_id)) {
    res
  } else {
    if (is.na(match(study_id, names(res))))
      stop(sQuote(study_id), " isn't a valid id.")
    else
      res[[study_id]]
  }
}




##' @export
##' @rdname get_study_meta
get_tree_ids <- function(sm) UseMethod("get_tree_ids")

##' @export
##' @rdname get_study_meta
get_publication <- function(sm) UseMethod("get_publication")

##' @export
##' @rdname get_study_meta
candidate_for_synth <- function(sm) UseMethod("candidate_for_synth")

##' @export
##' @rdname get_study_meta
get_study_year <- function(sm) UseMethod("get_study_year")

##' @export
##' @rdname get_study_meta
get_tree_ids.study_meta <- function(sm) {
    unlist(sm[["nexml"]][["treesById"]][[sm[["nexml"]][["^ot:treesElementOrder"]][[1]]]][["^ot:treeElementOrder"]])
}

##' @export
##' @rdname get_study_meta
get_publication.study_meta <- function(sm) {
    pub <- sm[["nexml"]][["^ot:studyPublicationReference"]]
    attr(pub, "DOI") <- sm[["nexml"]][["^ot:studyPublication"]][["@href"]]
    pub
}

##' @export
##' @rdname get_study_meta
candidate_for_synth.study_meta <- function(sm) {
    unlist(sm[["nexml"]][["^ot:candidateTreeForSynthesis"]])
}



##' @export
##' @rdname get_study_meta
get_study_year.study_meta <- function(sm) {
    sm[["nexml"]][["^ot:studyYear"]]
}
