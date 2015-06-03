context("Graph of Life (gol)")

test_that("gol about", {
              tmp_gol <- gol_about()
              expect_true(inherits(tmp_gol, "gol"))
              expect_equal(length(tmp_gol),  6L)
          })
