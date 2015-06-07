context("base functions")

test_that("otl_url returns the correct strings", {
          expect_match(otl_url(TRUE), "^http://devapi.opentreeoflife.org$")
          expect_match(otl_url(FALSE), "^http://api.opentreeoflife.org$")
      })

test_that("otl_version", {
              expect_equal(otl_version(), "v2")
              expect_equal(otl_version("foobar"), "foobar")
          })

test_that("otl_ottid_from_label",
          expect_equal(otl_ottid_from_label("flkdjfs_ott314343"),
                       314343))


test_that("errors that would otherwise not get caught in phylo_from_otl", {
              expect_error(phylo_from_otl(list(something = "((A, B), C);")),
                           "Cannot find tree")
              expect_error(phylo_from_otl(999), "I don't know how to deal with this format")
          })
