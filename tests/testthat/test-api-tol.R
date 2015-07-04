context("Tree of Life API")

############################################################################
## .tol_about                                                             ##
############################################################################

test_that("study_list is logical for .tol_about", {
    skip_on_cran()
    expect_error(.tol_about("true"),
                 "logical")
})

############################################################################
## .tol_mrca                                                              ##
############################################################################

test_that("neither ott_ids nor node_ids are NULL for .tol_mrca", {
    skip_on_cran()
    expect_error(.tol_mrca(NULL),
                 "Must supply")
})

############################################################################
## .tol_subtree                                                           ##
############################################################################

test_that("ott_id is not NULL", {
    skip_on_cran()
    expect_error(.tol_subtree(ott_id = NULL, tree_id = NULL),
                 "must be provided")
})

test_that("providing tree_id gives a warning", {
    skip_on_cran()
    expect_warning(.tol_subtree(ott_id = 814461, tree_id = "v2"),
                   "currently ignored")
})

############################################################################
## .tol_induced_subtree                                                   ##
############################################################################

test_that("ott_ids is not NULL", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(ott_ids = NULL),
                 "must be provided")
})

test_that("NAs are not accepted for ott_ids", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(ott_ids = c(123, NA, 456)),
                 "NAs are not allowed")
})
