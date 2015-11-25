############################################################################
## tol_subtree                                                            ##
############################################################################

context("test tol_subtree")

test_that("tol_subtree fails if ott_id is invalid", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = 6666666),
                 "Invalid")
})

test_that("tol_subtree fails if more than one ott_id is provided", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = c(666666, 6666667)),
                 "A subtree can only be inferred from a single")
})

test_that("tol_subtree fails if ott_id doesn't look like a number", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = "111A1111"),
                 "needs to look like a number")
})

test_that("tol_subtree returns a phylo object by default", {
    skip_on_cran()
    expect_true(inherits(tol_subtree(ott_id = 81461), "phylo"))
})

test_that("tol_subtree returns a newick file when providing a file argument", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tr <- tol_subtree(ott_id = 81461,  file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})



############################################################################
## tol_induced_subtree                                                    ##
############################################################################

context("test tol_induced_subtree")

test_that("warning for node ids that are not in TOL graph", {
    skip_on_cran()
    expect_warning(tol_induced_subtree(ott_ids = c(357968, 867416, 939325, 9999999)),
                   "not in graph")
})

test_that("error if ott_ids provided don't look like numbers", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_ids = c("13242", "kitten")),
                 "must look like numbers")
})

test_that("warning for ott ids that are not in TOL graph", {
    skip_on_cran()
    expect_warning(tol_induced_subtree(ott_ids = c(357968, 867416, 939325, 777777777)),
                   "not in graph")
})

## test_that("warning for ott ids not in tree",
##           ???)

test_that("tol_induced_subtree generates a newick file when providing a file argument", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tr <- tol_induced_subtree(ott_ids=c(292466, 267845, 666104), file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})

############################################################################
## tol_mrca                                                               ##
############################################################################

test_that("tol_mrca fails if ott_ids are not numbers", {
    skip_on_cran()
    expect_error(tol_mrca(ott_ids = c(13243, "a13415")),
                 "must look like numbers")
})

test_that("tol_mrca returns a list", {
    skip_on_cran()
    birds <- tol_mrca(ott_ids = c(412129, 536234))
    expect_true(inherits(birds, "list"))
    expect_true(all(names(birds) %in%
                      c("mrca_rank", "mrca_name",
                        "nearest_taxon_mrca_node_id", "invalid_node_ids",
                        "tree_id", "ott_id",
                        "mrca_unique_name","node_ids_not_in_tree",
                        "nearest_taxon_mrca_unique_name","nearest_taxon_mrca_ott_id",
                        "ott_ids_not_in_tree","nearest_taxon_mrca_name",
                        "invalid_ott_ids","mrca_node_id",
                        "nearest_taxon_mrca_rank")))
})

############################################################################
## strip_ott_ids                                                          ##
############################################################################

test_that("OTT ids can be striped from tip labels to allow taxon-matching", {
    skip_on_cran()
    genera <- c("Setophaga", "Cinclus", "Struthio")
    tr <- tol_induced_subtree(ott_ids=c(666104, 267845, 292466))
    expect_true(all(strip_ott_ids(tr$tip.label) %in% genera))
})
