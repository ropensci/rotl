context("tnrs API")


############################################################################
## .tnrs_match_names                                                      ##
############################################################################

test_that("names argument is provided for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names(NULL, NULL, TRUE, NULL, FALSE),
                 "must supply")
})

test_that("names argument is character for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names(TRUE, NULL, TRUE, NULL, FALSE),
                 "character")
})

test_that("names and ids have the same lengths for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names("Felis", NULL, TRUE, c("abc", "def"), FALSE),
                 "same length")
})

test_that("ids must be character for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names("Felis", NULL, TRUE, TRUE, FALSE),
                 "character")
})

test_that("do_approximate_matching is logical for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names("Felis", NULL, "true", NULL, FALSE),
                 "logical")
})

test_that("include_suppressed is logical for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names("Felis", NULL, TRUE, NULL, "true"),
                 "logical")
})


test_that("context_name is character for .tnrs_match_names", {
    skip_on_cran()
    expect_error(.tnrs_match_names("Felis", TRUE, TRUE, NULL, FALSE, TRUE),
                 "character")
})


############################################################################
## .tnrs_infer_context                                                    ##
############################################################################

test_that("names is not NULL for .tnrs_infer_context", {
    skip_on_cran()
    expect_error(.tnrs_infer_context(NULL),
                 "Must supply")
})

test_that("names is character for .tnrs_infer_context", {
    skip_on_cran()
    expect_error(.tnrs_infer_context(TRUE),
                 "character")
})
