## Create a vector (character) that contains the NEWICK tree strings
## found in a file
parse_newick <- function(file) {
    trs <- readLines(file, warn = FALSE)
    trs <- strsplit(trs, split = ";")
    trs <- sapply(trs, function(x) gsub("^\\s+|\\s+$", "", x))
    trs <- unlist(trs)
    trs <- gsub("\\s", "_", trs)
    trs <- trs[nchar(trs) > 0]
    trs
}

## Internal function to be used by `deduplicate_labels` that:
## 1. identify duplicated labels
## 2. make them unique
## 3. replace the duplicated labels by their unique counterparts
dedup_lbl <- function(tr_str) {
    tr_lbl <- tree_to_labels(tr_str, remove_quotes = TRUE)$tip_label
    tr_lbl_unq <- make.unique(tr_lbl, sep = "_")
    if (!identical(tr_lbl, tr_lbl_unq)) {
        for (i in seq_along(tr_lbl)) {
            tr_str <- sub(paste0("([\\(|,]\\'?)\\Q", tr_lbl[i], "\\E(\\'?[:|\\)|,])"),
                          paste0("\\1", tr_lbl_unq[i], "\\2"),  tr_str)
        }
        warning("Some tip labels were duplicated and have been modified: ",
                paste(tr_lbl[duplicated(tr_lbl)], collapse = ", "))
    }
    paste0(tr_str, ";")
}

## Main function: takes a file with potentially duplicated tip labels
## and reate a new file with unique labels
deduplicate_labels <- function(file) {
    tr_strs <- parse_newick(file)
    tr_dedup <- sapply(tr_strs, dedup_lbl)
    tmp_tr <- tempfile()
    cat(tr_dedup, file = tmp_tr, sep = "\n")
    tmp_tr
}
