context("studies API tests")


###########################
## .studies_find_studies ##
###########################

test_that("argument verbose needs to be logical for .studies_find_studies", {
    skip_on_cran()
    expect_error(.studies_find_studies(NULL, NULL, "123", FALSE),
                 "logical")
})

test_that("argument exact needs to be logical for .studies_find_studies", {
    skip_on_cran()
    expect_error(.studies_find_studies(NULL, NULL, TRUE, "123"),
                 "logical")
})

test_that("argument property needs to be character for .studies_find_studies", {
    skip_on_cran()
    expect_error(.studies_find_studies(123, NULL, TRUE, TRUE),
                 "character")
})

test_that("argument value needs to be character for .studies_find_studies", {
    skip_on_cran()
    expect_error(.studies_find_studies("test", 123, TRUE, TRUE),
                 "character")
})

test_that("both property & value need to be provided for .studies_find_studies", {
    skip_on_cran()
    expect_error(.studies_find_studies("test", NULL, TRUE, TRUE),
                 "Must supply")
})

test_that("both property & value need to be provided for .studies_find_studies", {
    skip_on_cran()
    expect_error(.studies_find_studies(NULL, "test", TRUE, TRUE),
                 "Must supply")
})


###########################
## .studies_find_trees ##
###########################

test_that("argument verbose needs to be logical for .studies_find_trees", {
    skip_on_cran()
    expect_error(.studies_find_trees(NULL, NULL, "123", FALSE),
                 "logical")
})

test_that("argument exact needs to be logical for .studies_find_trees", {
    skip_on_cran()
    expect_error(.studies_find_trees(NULL, NULL, TRUE, "123"),
                 "logical")
})

test_that("argument property needs to be character for .studies_find_trees", {
    skip_on_cran()
    expect_error(.studies_find_trees(123, NULL, TRUE, TRUE),
                 "character")
})

test_that("argument value needs to be character for .studies_find_trees", {
    skip_on_cran()
    expect_error(.studies_find_trees("test", 123, TRUE, TRUE),
                 "character")
})

test_that("both property & value need to be provided for .studies_find_trees", {
    skip_on_cran()
    expect_error(.studies_find_trees("test", NULL, TRUE, TRUE),
                 "Must supply")
})

test_that("both property & value need to be provided for .studies_find_trees", {
    skip_on_cran()
    expect_error(.studies_find_trees(NULL, "test", TRUE, TRUE),
                 "Must supply")
})

test_that("exact works as intended", {
    skip_on_cran()
    expect_equal(length(.studies_find_studies("ot:focalCladeOTTTaxonName",
                                              "felidae", exact = TRUE)$matched_studies), 0)
})


test_that("exact works as intended", {
    skip_on_cran()
    expect_true(length(.studies_find_studies("ot:focalCladeOTTTaxonName",
                                             "Felidae", exact = TRUE)$matched_studies) >= 1)
})

############################################################################
## .get_study                                                             ##
############################################################################


test_that("study_id isn't NULL for .get_study", {
    skip_on_cran()
    expect_error(.get_study(NULL, "test"),
                 "Must supply")
})

test_that("study_id is character for .get_study", {
    skip_on_cran()
    expect_error(.get_study(TRUE, "test"),
                 "character")
})


############################################################################
## .get_study_tree                                                        ##
############################################################################

test_that("study_id isn't NULL for .get_study_tree", {
    skip_on_cran()
    expect_error(.get_study_tree(NULL, NULL),
                 "Must supply")
})

test_that("study_id isn't NULL for .get_study_tree", {
    skip_on_cran()
    expect_error(.get_study_tree("123", NULL),
                 "Must supply")
})

test_that("study_id isn't NULL for .get_study_tree", {
    skip_on_cran()
    expect_error(.get_study_tree(NULL, "123"),
                 "Must supply")
})

test_that("study_id is character for .get_study", {
    skip_on_cran()
    expect_error(.get_study_tree(TRUE, "test"),
                 "character")
})

test_that("study_id is character for .get_study", {
    skip_on_cran()
    expect_error(.get_study_tree("test", TRUE),
                 "character")
})


############################################################################
## .get_study_subtree                                                        ##
############################################################################

test_that("study_id isn't NULL for .get_study_subtree", {
    skip_on_cran()
    expect_error(.get_study_subtree(NULL, NULL, NULL),
                 "Must supply")
})

test_that("tree_id isn't NULL for .get_study_subtree", {
    skip_on_cran()
    expect_error(.get_study_subtree("123", NULL, "123"),
                 "Must supply")
})

test_that("subtree_id isn't NULL for .get_study_subtree", {
    skip_on_cran()
    expect_error(.get_study_subtree(NULL, "123", "123"),
                 "Must supply")
})

test_that("study_id isn't NULL for .get_study_subtree", {
    skip_on_cran()
    expect_error(.get_study_subtree("123", "123", NULL),
                 "Must supply")
})

test_that("study_id is character for .get_study", {
    skip_on_cran()
    expect_error(.get_study_subtree(TRUE, "test", "test"),
                 "character")
})

test_that("tree_id is character for .get_study", {
    skip_on_cran()
    expect_error(.get_study_subtree("test", TRUE, "test"),
                 "character")
})

test_that("subtree_id is character for .get_study", {
    skip_on_cran()
    expect_error(.get_study_subtree("test", "test", TRUE),
                 "character")
})
