context("taxonomy API")



#####################
## .taxonomy_taxon ##
#####################

test_that("ott_id is not null for .taxonomy_taxon",
          expect_error(.taxonomy_taxon(NULL),
                       "Must supply"))

test_that("ott_id is of length 1 for .taxonomy_taxon",
          expect_error(.taxonomy_taxon(c(123, 456, 789)),
                       "Must only supply"))

test_that("ott_id is a numeric for .taxonomy_taxon",
          expect_error(.taxonomy_taxon(TRUE),
                       "numeric"))


############################################################################
## .taxonomy_subtree                                                      ##
############################################################################


test_that("ott_id is not null for .taxonomy_subtree",
          expect_error(.taxonomy_subtree(NULL),
                       "Must supply"))

test_that("ott_id is of length 1 for .taxonomy_subtree",
          expect_error(.taxonomy_subtree(c(123, 456, 789)),
                       "Must only supply"))

test_that("ott_id is a numeric for .taxonomy_subtree",
          expect_error(.taxonomy_subtree(TRUE),
                       "numeric"))


############################################################################
## .taxonomy_lica                                                      ##
############################################################################


test_that("ott_id is not null for .taxonomy_lica",
          expect_error(.taxonomy_lica(NULL),
                       "Must supply"))

test_that("ott_id is a numeric for .taxonomy_lica",
          expect_error(.taxonomy_lica(TRUE),
                       "numeric"))
