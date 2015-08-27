## Function to extract tip and edge labels from newick formatted strings
## useful when the tree is too small to be read in by ape/rncl.
## tr needs to be a newick formatted tree string
## - missing tips are removed (OK for OTL as it won't happen)
tree_to_labels <- function(tr, remove_quotes = TRUE) {

    n_right <- unlist(gregexpr("\\)", tr))
    n_left <- unlist(gregexpr("\\(", tr))

    if (n_right[1] == -1) n_right <- 0 else n_right <- length(n_right)
    if (n_left[1] == -1) n_left <- 0 else n_left <- length(n_left)

    if (!identical(n_right, n_left)) {
        stop("invalid newick string, numbers of ( and ) don't match")
    }

    ## remove white spaces
    tr <- gsub("\\s+", "", tr)

    ## remove branch lengths
    tr <- gsub(":[0-9]+(\\.[0-9]+)?", "", tr)

    ## TODO?: remove comments

    if (n_right < 1) {
        ## if only 1 tip
        tip_lbl <- gsub(";$", "", tr)
        edge_lbl <- character(0)
    } else {
        ## extract edge labels
        edge_lbl <- unlist(strsplit(tr, ")"))
        edge_lbl <- grep("^[^\\(]", edge_lbl, value = T)
        edge_lbl <- gsub("(,|;).*$", "", edge_lbl)
        edge_lbl <- edge_lbl[nzchar(edge_lbl)]

        ## extract tips
        tip_lbl <- unlist(strsplit(tr, ","))
        tip_lbl <- gsub("^\\(*", "", tip_lbl)
        tip_lbl <- gsub("\\).*$", "", tip_lbl)
        tip_lbl <- tip_lbl[nzchar(tip_lbl)]
    }

    if (remove_quotes) {
        tip_lbl <- gsub("^(\\\"|\\\')(.+)(\\\'|\\\")$", "\\2", tip_lbl)
    }

    list(tip_label = tip_lbl, edge_label = edge_lbl)
}
