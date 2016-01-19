## Unexported function that generates a data frame summarizing the metadata.
## This function is used by both studies_find_studies and studies_find_trees,
## to generate the output when using the argument detailed=TRUE
##' @importFrom stats setNames
summarize_meta <- function(study_ids) {
    fill <- function(x) {
        if (length(unlist(x))) {
            x
        } else {
            ""
        }
    }
    meta_raw <- lapply(study_ids, function(x) get_study_meta(x))
    ## Extract the metadata
    meta <- lapply(meta_raw, function(m) {
      c(tree_ids =  fill(list(get_tree_ids(m))),
        study_year = fill(get_study_year(m)),
        publication = fill(get_publication(m)),
        doi = fill(attr(get_publication(m), "DOI")),
        candidate = fill(list(candidate_for_synth(m)))
        )
    })
    ## Convert into a data frame
    dat <- lapply(meta, function(m) {
        c(n_trees = length(m[["tree_ids"]]),
          tree_ids = limit_trees(m[["tree_ids"]]),
          candidate = paste(m[["candidate"]], collapse = ", "),
          study_year = m[["study_year"]],
          title =  fill(extract_title(m[["publication"]])),
          study_doi = m[["doi"]])
    })
    dat <- do.call("rbind", dat)
    dat <- cbind(study_ids = study_ids, dat)
    rownames(dat) <- NULL
    dat <- data.frame(dat, stringsAsFactors = FALSE)

    ## Add list of found trees as attributes
    found_trees <- lapply(meta, function(m) {
      m[["tree_ids"]]
    })
    found_trees <- stats::setNames(found_trees, study_ids)
    attr(dat, "found_trees") <- found_trees
    attr(dat, "metadata") <- meta_raw

    dat
}



## Unexported function that attempts to extract title from the
## citation information associated with the study information. The
## function gets the element that follows what looks like a year in
## the string.
## pub_orig: the publication string extracted from the study metadata
## split_char: the character on which the bibliographic elements are
## separated with. (currently only deals with . and ,)
extract_title <- function(pub_orig, split_char = "\\.") {
    pub <- unlist(strsplit(pub_orig, split = split_char))
    pub <- gsub("^\\s|\\s$", "",  pub)
    which_year <- grep("^\\d{4}[a-z]?$", pub)
    res <- pub[which_year + 1]
    if (length(res) > 0)
        return(res)
    else if (split_char == ",") {
        return(character(0))
    } else {
        extract_title(pub_orig, ",")
    }
}

## Unexported function that limit the display of tree_ids to the first
## 5 values.
limit_trees <- function(x) {
    if (length(x) > 4)
        x <- c(x[1:5], "...")
    paste(x, collapse = ", ")
}
