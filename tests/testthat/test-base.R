context("base functions")

test_that("otl_url returns the correct strings", {
    skip_on_cran()
    expect_match(otl_url(dev = TRUE), "^https://devapi.opentreeoflife.org$")
    expect_match(otl_url(dev = FALSE), "^https://api.opentreeoflife.org$")
})

test_that("otl_version", {
    skip_on_cran()
    expect_equal(otl_version(), "v3")
    expect_equal(otl_version("foobar"), "foobar")
})

test_that("otl_ottid_from_label", {
    skip_on_cran()
    expect_equal(otl_ottid_from_label("flkdjfs_ott314343"),
                 314343)
})


test_that("errors that would otherwise not get caught in phylo_from_otl", {
    expect_error(phylo_from_otl(list(something = "((A, B), C);")),
                 "Cannot find tree")
    expect_error(phylo_from_otl(999), "I don't know how to deal with this format")
})

############################################################################
## check_numeric                                                          ##
############################################################################

test_that("check_numeric works on integer", {
    expect_true(check_numeric("123"))
    expect_true(check_numeric(123))
    expect_true(check_numeric(123L))
    expect_true(check_numeric(list(123)))
})

test_that("check_numeric fails if there are characters", {
    expect_false(check_numeric("A123"))
    expect_false(check_numeric("1A23"))
    expect_false(check_numeric("123A"))
    expect_false(check_numeric("12-3"))
})

test_that("check_numeric fails with more exotic types", {
    expect_false(check_numeric(NA))
    expect_false(check_numeric(TRUE))
    expect_false(check_numeric(1.23))
    expect_false(check_numeric(0.9999999999999))

})

test_that("check_numeric fails if more than 1 element provided",
          expect_error(check_numeric(c(1, 2))))
