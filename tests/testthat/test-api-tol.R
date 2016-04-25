context("Tree of Life API")

############################################################################
## .tol_about                                                             ##
############################################################################

test_that("include_source_list is logical for .tol_about", {
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
                 "Must provide")
})

############################################################################
## .tol_subtree                                                           ##
############################################################################

test_that("ott_id is not NULL", {
    skip_on_cran()
    expect_error(.tol_subtree(ott_id = NULL, node_id = NULL),
                 "Must provide")
})

############################################################################
## .tol_induced_subtree                                                   ##
############################################################################

test_that("ott_ids is not NULL", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(ott_ids = NULL),
                 "Must provide")
})

test_that("NAs are not accepted for ott_ids", {
    skip_on_cran()
    expect_error(.tol_induced_subtree(ott_ids = c(123, NA, 456)),
                 "NAs are not allowed")
})

####################
## .tol_node_info ##
####################

test_that("include_lineage must be logical with .tol_node_info", {
    skip_on_cran()
    expect_error(.tol_node_info(ott_id = "ott_123", include_lineage = "123"),
                 "logical")
})

test_that("ott_id must be a numeric with .tol_node_info", {
    skip_on_cran()
    expect_error(.tol_node_info(ott_id = "test"),
                 "look like numbers")
})

test_that("node_id must be a character with .tol_node_info", {
    skip_on_cran()
    expect_error(.tol_node_info(node_id = 123),
                 "must look like")
})
