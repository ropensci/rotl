context("test tol_induced_subtree")

test_that("warning for node ids that are not in TOL graph",
          expect_warning(tol_induced_subtree(ott_ids = c(357968, 867416, 939325, 9999999)),
                         "not in graph"))

test_that("warning for ott ids that are not in TOL graph",
          expect_warning(tol_induced_subtree(node_ids = c(77777777), ott_ids = c(357968, 867416, 939325)),
                         "not in graph"))

## test_that("warning for ott ids not in tree",
##           ???)
