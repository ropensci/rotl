context("Graph of Life API tests")


######################
## .gol_source_tree ##
######################

test_that("empty arguments throw errors with .gol_source_tree", {
    skip_on_cran()
    expect_error(.gol_source_tree(study_id = NULL, tree_id = NULL, git_sha = NULL),
                 "Must supply")
})

test_that("empty arguments throw errors with .gol_source_tree", {
    skip_on_cran()
    expect_error(.gol_source_tree(study_id = 123, tree_id = NULL, git_sha = NULL),
                 "character")
})

test_that("empty arguments throw errors with .gol_source_tree", {
    skip_on_cran()
    expect_error(.gol_source_tree(study_id = "something", tree_id = NULL, git_sha = NULL),
                 "Must supply")
})

test_that("empty arguments throw errors with .gol_source_tree", {
    skip_on_cran()
    expect_error(.gol_source_tree(study_id = "something", tree_id = 123, git_sha = NULL),
                 "character")
})


test_that("empty arguments throw errors with .gol_source_tree", {
    skip_on_cran()
    expect_error(.gol_source_tree(study_id = "something", tree_id = "something", git_sha = NULL),
                 "Must supply")
})

test_that("empty arguments throw errors with .gol_source_tree", {
    skip_on_cran()
    expect_error(.gol_source_tree(study_id = "something", tree_id = "something", git_sha = 123),
                 "character")
})


####################
## .gol_node_info ##
####################

test_that("include_lineage must be logical with .gol_node_info", {
    skip_on_cran()
    expect_error(.gol_node_info(ott_id = "ott_123", include_lineage = "123"),
                 "logical")
})

test_that("ott_id must be a numeric .gol_node_info", {
    skip_on_cran()
    expect_error(.gol_node_info(ott_id = "test"),
                 "look like a number")
})
