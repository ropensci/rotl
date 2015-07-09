context("tnrs")

############################################################################
## tnrs_match_names                                                       ##
############################################################################

test_that("tnrs_match_names fails if incorrect context is provided", {
    skip_on_cran()
    expect_error(tnrs_match_names("felis", context_name = "Cats"),
                 "Check possible values using tnrs_contexts")
})

test_that("tnrs_match_names fails if invalid name provided (nothing returned)", {
    skip_on_cran()
    expect_error(tnrs_match_names("fluffy", do_approximate_matching = FALSE),
                 "No matches for any of the provided taxa")
})

test_that("tnrs_match_names warns if a name is not matched", {
    skip_on_cran()
    expect_warning(tnrs_match_names(c("fluffy", "felis"), do_approximate_matching = FALSE),
                   "are not matched")
})

## everything else is covered by the match_names + the API tests

############################################################################
## tnrs_contexts                                                          ##
############################################################################

test_that("tnrs_contexts", {
    skip_on_cran()
    tc <- tnrs_contexts()
    expect_true(inherits(tc, "tnrs_contexts"))
    expect_true(all(names(tc) %in% c("ANIMALS", "MICROBES", "FUNGI", "PLANTS", "LIFE")))
})

############################################################################
## tnrs_infer_context                                                     ##
############################################################################

test_that("tnrs_infer_context", {
    skip_on_cran()
    tic <- tnrs_infer_context(c("Felis", "Leo"))
    expect_equal(tic[["context_name"]], "Mammals")
    expect_equal(tic[["context_ott_id"]], 244265)
    expect_equal(tic[["ambiguous_names"]][[1]], "leo")
})
