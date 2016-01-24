context("taxonomy API")


############################################################################
## .taxonomy_taxon                                                        ##
############################################################################


test_that("ott_id is not null for .taxonomy_taxon", {
    skip_on_cran()
    expect_error(.taxonomy_taxon(NULL),
                 "Must supply")
})

test_that("ott_id is of length 1 for .taxonomy_taxon", {
    skip_on_cran()
    expect_error(.taxonomy_taxon(c(123, 456, 789)),
                 "Must only supply")
})

test_that("ott_id is a numeric for .taxonomy_taxon", {
    skip_on_cran()
    expect_error(.taxonomy_taxon(TRUE),
                 "look like a number")
})

test_that("include_lineage is a flag", {
    skip_on_cran()
    expect_error(.taxonomy_taxon(ott_id = 515698, include_lineage = c(TRUE, FALSE)),
                 "is not a flag")
    expect_error(.taxonomy_taxon(ott_id = 515698, include_lineage = c("na")),
                 "is not a flag")
    expect_error(.taxonomy_taxon(ott_id = 515698, include_lineage = c(1235)),
                 "is not a flag")
})

test_that("list_terminal_descendants is a flag", {
    skip_on_cran()
    expect_error(.taxonomy_taxon(ott_id = 515698, list_terminal_descendants = c(TRUE, FALSE)),
                 "is not a flag")
    expect_error(.taxonomy_taxon(ott_id = 515698, list_terminal_descendants = c("na")),
                 "is not a flag")
    expect_error(.taxonomy_taxon(ott_id = 515698, list_terminal_descendants = c(1235)),
                 "is not a flag")
})


############################################################################
## .taxonomy_subtree                                                      ##
############################################################################


test_that("ott_id is not null for .taxonomy_subtree", {
    skip_on_cran()
    expect_error(.taxonomy_subtree(NULL),
                 "Must supply")
})

test_that("ott_id is of length 1 for .taxonomy_subtree", {
    skip_on_cran()
    expect_error(.taxonomy_subtree(c(123, 456, 789)),
                 "Must only supply")
})

test_that("ott_id is a numeric for .taxonomy_subtree", {
    skip_on_cran()
    expect_error(.taxonomy_subtree(TRUE),
                 "look like a number")
})


############################################################################
## .taxonomy_lica                                                      ##
############################################################################


test_that("ott_id is not null for .taxonomy_lica", {
    skip_on_cran()
    expect_error(.taxonomy_lica(NULL),
                 "Must supply")
})

test_that("ott_id is a numeric for .taxonomy_lica", {
    skip_on_cran()
    expect_error(.taxonomy_lica(TRUE),
                 "look like a number")
})
