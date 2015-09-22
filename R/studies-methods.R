
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
