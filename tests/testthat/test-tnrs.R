context("tnrs")

############################################################################
## tnrs_match_names                                                       ##
############################################################################

test_that("tnrs_match_names fails if incorrect context is provided", {
  skip_on_cran()
  expect_error(
    tnrs_match_names("felis", context_name = "Cats"),
    "Check possible values using tnrs_contexts"
  )
})

test_that("tnrs_match_names warns if single invalid name provided", {
  skip_on_cran()
  expect_warning(
    tnrs_match_names("fluffy", do_approximate_matching = FALSE),
    "are not matched"
  )
})

test_that("tnrs_match_names warns if at least one name is not matched", {
  skip_on_cran()
  expect_warning(
    tnrs_match_names(c("fluffy", "felis"), do_approximate_matching = FALSE),
    "are not matched"
  )
})

test_that("tnrs_match_names warns if some names are duplicated", {
  skip_on_cran()
  expect_warning(tnrs_match_names(c("pan", "pongo", "Pan", "pongo", "felis", "feLis")))
})


test_that("object returned by tnrs_match_names have the correct data type", {
  skip_on_cran()
  birds <- c("stercorarius parasiticus", "ficedula albicollis", "sterna dougallii")
  taxa <- tnrs_match_names(birds, do_approximate_matching = FALSE)
  expect_true(is.logical(taxa[["approximate_match"]]))
  expect_true(is.logical(taxa[["is_synonym"]]))
  expect_equal(sum(taxa[["score"]]), 3)
})

test_that("tnrs_match_names deals correctly with non-exact matches", {
  skip_on_cran()
  birds <- c("stercorarius parasiticus", "ficedula albicollis", "sternadougallii")
  expect_warning(
    taxa <- tnrs_match_names(birds, do_approximate_matching = FALSE),
    "are not matched"
  )
  expect_equal(nrow(taxa), 3L)
  expect_equivalent(
    taxa[match("sternadougallii", taxa[["search_string"]]), ],
    list("sternadougallii", NA_character_, NA, NA_real_, NA_integer_, NA, NA_character_, NA_integer_)
  )
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
  expect_equal(tic[["ambiguous_names"]][[1]], "Leo")
})
