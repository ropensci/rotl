context("Graph of Life (gol)")

############################################################################
## gol_about                                                              ##
############################################################################

test_that("gol about. If any of these tests break, it means that the doc for this function needs to be updated.", {
    skip_on_cran()
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

############################################################################
## gol_node_info                                                          ##
############################################################################

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    gol_info <- gol_node_info(ott_id = 81461)
}

test_that("gol node info. If any of these tests break, it means that the doc for this function needs to be updated.", {
    skip_on_cran()
    expect_true(all(names(gol_info) %in%
                      c("tree_id", "num_synth_tips", "name", "rank", "ott_id", "num_tips",
                        "tree_sources", "tax_source", "synth_sources",
                        "node_id", "in_synth_tree")))
    expect_true(inherits(gol_info, "gol_node"))
})

test_that("gol_node tax_rank method", {
    skip_on_cran()
    expect_equal(tax_rank(gol_info), "class")
})

test_that("gol_node ott_id method", {
    skip_on_cran()
    expect_equal(ott_id(gol_info), 81461)
})

test_that("gol_node synth_sources method", {
    skip_on_cran()
    expect_true(inherits(synth_sources(gol_info), "data.frame"))
    expect_true(all(names(synth_sources(gol_info)) %in%
                      c("study_id", "tree_id", "git_sha")))
})
