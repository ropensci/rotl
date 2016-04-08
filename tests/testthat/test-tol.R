############################################################################
## tol_about                                                              ##
############################################################################

context("test tol_about (and in turn print.tol_summary)")

test_that("Names in object returned are correct/match the docs", {
    skip_on_cran()
    req <- tol_about(include_source_list = TRUE)
    expect_true(all(names(req) %in%
                    c("source_list", "date_created", "root", "num_source_trees",
                      "taxonomy_version", "num_source_studies",
                      "filtered_flags", "synth_id", "source_id_map")))
    expect_true(all(names(req$root) %in%
                    c("taxon", "num_tips", "node_id")))
    expect_true(all(names(req$root$taxon) %in%
                    c("tax_sources", "name", "unique_name", "rank", "ott_id")))
})


############################################################################
## tol_subtree                                                            ##
############################################################################

context("test tol_subtree")

test_that("tol_subtree fails if ott_id is invalid", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = 6666666))
})

test_that("tol_subtree fails if more than one ott_id is provided", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = c(666666, 6666667)),
                 "A subtree can only be inferred from a single")
})

test_that("tol_subtree fails if ott_id doesn't look like a number", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = "111A1111"),
                 "needs to look like a number")
})

test_that("tol_subtree returns a phylo object by default", {
    skip_on_cran()
    expect_true(inherits(tol_subtree(ott_id = 81461), "phylo"))
})

test_that("tol_subtree returns a newick file when providing a file argument", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tr <- tol_subtree(ott_id = 81461,  file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})


############################################################################
## tol_induced_subtree                                                    ##
############################################################################

context("test tol_induced_subtree")

test_that("warning for node ids that are not in TOL graph", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_ids = c(357968, 867416, 939325, 9999999)),
                   "not found")
})

test_that("error if ott_ids provided don't look like numbers", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_ids = c("13242", "kitten")),
                 "must look like numbers")
})


## test_that("warning for ott ids not in tree",
##           ???)

test_that("tol_induced_subtree generates a newick file when providing a file argument", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tr <- tol_induced_subtree(ott_ids=c(292466, 267845, 666104), file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})


############################################################################
## tol_mrca                                                               ##
############################################################################

test_that("tol_mrca fails if ott_ids are not numbers", {
    skip_on_cran()
    expect_error(tol_mrca(ott_ids = c(13243, "a13415")),
                 "must look like numbers")
})

test_that("tol_mrca returns a list", {
    skip_on_cran()
    birds <- tol_mrca(ott_ids = c(412129, 536234))
    expect_true(inherits(birds, "list"))
    expect_true(all(names(birds) %in%
                    c("mrca",
                      "source_id_map",
                      "nearest_taxon")))
})


############################################################################
## strip_ott_ids                                                          ##
############################################################################

test_that("OTT ids can be striped from tip labels to allow taxon-matching", {
    skip_on_cran()
    genera <- c("Setophaga", "Cinclus", "Struthio")
    tr <- tol_induced_subtree(ott_ids=c(666104, 267845, 292466))
    expect_true(all(strip_ott_ids(tr$tip.label) %in% genera))
})


############################################################################
## tol_node_info                                                          ##
############################################################################

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    tol_info <- tol_node_info(ott_id = 81461)
}

test_that("tol node info.", {
    skip_on_cran()
    expect_true(all(names(tol_info) %in%
                      c("partial_path_of", "supported_by", "source_id_map", "taxon",
                        "num_tips", "terminal", "node_id")))
    expect_true(inherits(tol_info, "tol_node"))
})

# note that tax info is now contained in a taxon 'blob'
test_that("tol_node tax_rank method", {
    skip_on_cran()
    expect_equal(tax_rank(tol_info), "class")
})

test_that("tol_node ott_id method", {
    skip_on_cran()
    expect_equal(ott_id(tol_info), 81461)
})

test_that("tol_node synth_sources method", {
    skip_on_cran()
    expect_true(inherits(synth_sources(tol_info), "data.frame"))
    expect_true(all(names(synth_sources(tol_info)) %in%
                      c("study_id", "tree_id", "git_sha")))
})
