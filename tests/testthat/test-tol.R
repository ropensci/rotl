############################################################################
## tol_subtree                                                            ##
############################################################################

context("test tol_subtree")

test_that("tol_subtree fails if ott_id is invalid",
          expect_error(tol_subtree(ott_id = 6666666),
                       "Invalid"))

test_that("tol_subtree fails if node_id is invalid",
          expect_error(tol_subtree(node_id = 9999999),
                       "failure"))

test_that("tol_subtree returns a phylo object by default",
          expect_true(inherits(tol_subtree(ott_id = 81461), "phylo")))

test_that("tol_subtree returns a newick file when providing a file argument", {
    ff <- tempfile(fileext = ".tre")
    tr <- tol_subtree(ott_id = 81461,  file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1)))
})



############################################################################
## tol_induced_subtree                                                    ##
############################################################################

context("test tol_induced_subtree")

test_that("warning for node ids that are not in TOL graph",
          expect_warning(tol_induced_subtree(ott_ids = c(357968, 867416, 939325, 9999999)),
                         "not in graph"))

test_that("warning for ott ids that are not in TOL graph",
          expect_warning(tol_induced_subtree(node_ids = c(77777777),
                                             ott_ids = c(357968, 867416, 939325)),
                         "not in graph"))

## test_that("warning for ott ids not in tree",
##           ???)

test_that("tol_induced_subtree generates a newick file when providing a file argument", {
    ff <- tempfile(fileext = ".tre")
    tr <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104), file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1)))
})
