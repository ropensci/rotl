context("Graph of Life (gol)")

test_that("gol about. If any of these tests break, it means that the doc for this function needs to be updated.", {
    tmp_gol <- gol_about()
    expect_true(inherits(tmp_gol, "gol"))
    expect_equal(length(tmp_gol),  6L)
    expect_true(all(names(tmp_gol) %in% c("graph_num_source_trees",
                                          "graph_taxonomy_version",
                                          "graph_num_tips",
                                          "graph_root_name",
                                          "graph_root_node_id",
                                          "graph_root_ott_id")))
})

test_that("gol node info. If any of these tests break, it means that the doc for this function needs to be updated.", {
              expect_true(all(names(gol_node_info) %in%
                                c("in_graph", "tree_id", "name","rank","ott_id", "num_tips",
                                  "tree_sources", "tax_source", "synth_sources",
                                  "node_id", "in_synth_tree", "num_synth_children")))
          })
