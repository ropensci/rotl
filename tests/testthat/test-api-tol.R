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
    expect_error(.tol_mrca(NULL, NULL),
                 "Must supply")
})

## test_that("ott_ids is character for .tol_mrca",
##           expect_error(.tol_mrca(TRUE, NULL),
##                        "character"))

## test_that("node_ids is character for .tol_mrca",
##           expect_error(.tol_mrca(NULL, TRUE),
##                        "character"))

############################################################################
## .tol_subtree                                                           ##
############################################################################

test_that("neither node_id nor ott_id are NULL", {
    skip_on_cran()
    expect_error(.tol_subtree(node_id = NULL, tree_id = NULL),
                 "Must supply")
})

test_that("node_id and ott_id are not both specified", {
    skip_on_cran()
    expect_error(.tol_subtree(node_id = 123, ott_id = 123),
                 "Use only")
})

test_that("providing tree_id gives a warning", {
    skip_on_cran()
    expect_warning(.tol_subtree(ott_id = 814461, tree_id = "v2"),
                   "currently ignored")
})

############################################################################
## .tol_induced_subtree                                                   ##
############################################################################

test_that("neither node_ids nor ott_ids are NULL", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(NULL, NULL),
                 "Must supply")
})

test_that("NAs are not accepted for node_ids", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(node_ids = c(123, NA, 456)),
                 "NAs are not allowed")
})

test_that("NAs are not accepted for ott_ids", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(ott_ids = c(123, NA, 456)),
                 "NAs are not allowed")
})

test_that("NAs are not accepted for ott_ids", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(
              node_ids = c(123, NA, 456),
              ott_ids = c(123, NA, 456)),
              "NAs are not allowed")
})
