context("Graph of Life (gol)")

test_that("gol about", {
              tmp_gol <- gol_about()
              expect_true(inherits(tmp_gol, "gol"))
              expect_equal(length(tmp_gol),  6L)
          })

test_that("gol node info. If any of these tests break, it means that the doc for this function needs to be updated.", {
              expect_true(all(names(gol_node_info) %in%
                                c("in_graph", "tree_id", "name","rank","ott_id", "num_tips",
                                  "tree_sources", "tax_source", "synth_sources",
                                  "node_id", "in_synth_tree", "num_synth_children")))
          })
