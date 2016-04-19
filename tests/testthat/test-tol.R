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
    expect_true(all(names(source_list(req)) %in% c("study_id",
                                                   "tree_id",
                                                   "git_sha")))
    expect_true(nrow(source_list(req)) > 1)
    expect_true(all(grepl("^(ot|pg)", source_list(req)[["study_id"]])))
    expect_true(all(grepl("^tr", source_list(req)[["tree_id"]], ignore.case = TRUE)))
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
                 "only 1 element should be provided")
})

test_that("tol_subtree fails if ott_id doesn't look like a number", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = "111A1111"),
                 "must look like a number")
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
    expect_true(inherits(birds, "tol_mrca"))
    expect_true(all(names(birds) %in%
                    c("mrca",
                      "source_id_map",
                      "nearest_taxon")))
})

test_that("methods for tol_mrca where the node is a taxon", {
    skip_on_cran()
    hol <- tol_mrca(c(431586, 957434))
    expect_true(length(tax_sources(hol)[[1]]) > 1)
    expect_true(any(grepl("worms", tax_sources(hol)[[1]])))
    expect_equal(unique_name(hol)[[1]], "Holothuria")
    expect_equal(tax_name(hol)[[1]], "Holothuria")
    expect_equal(tax_rank(hol)[[1]], "genus")
    expect_equal(ott_id(hol)[[1]], 5004030)
    expect_equal(names(tax_sources(hol)), "ott5004030")
    expect_true(all(names(source_list(hol)) %in% c("tree_id",
                                                   "study_id",
                                                   "git_sha")))
})

test_that("methods for tol_mrca where the node is not a taxon", {
    skip_on_cran()
    birds_mrca <- tol_mrca(ott_ids=c(412129, 536234))
    expect_true(length(tax_sources(birds_mrca)[[1]]) >=  1)
    expect_true(any(grepl("ncbi", tax_sources(birds_mrca)[[1]])))
    expect_equal(unique_name(birds_mrca)[[1]], "Neognathae")
    expect_equal(tax_name(birds_mrca)[[1]], "Neognathae")
    expect_equal(tax_rank(birds_mrca)[[1]], "superorder")
    expect_equal(ott_id(birds_mrca)[[1]], 241846)
    expect_equal(names(ott_id(birds_mrca)), "mrcaott246ott5481")
    expect_true(all(names(source_list(birds_mrca)) %in% c("tree_id",
                                                          "study_id",
                                                          "git_sha")))
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
    tol_lin <- tol_node_info(ott_id = 81461, include_lineage = TRUE)
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
    expect_equal(tax_rank(tol_info)[[1]], "class")
})

test_that("tol_node ott_id method", {
    skip_on_cran()
    expect_equal(ott_id(tol_info)[[1]], 81461)
    expect_equal(names(ott_id(tol_info)), "ott81461")
})

test_that("tol_node tax_sources", {
    skip_on_cran()
    expect_true(any(grepl("worms", tax_sources(tol_info)[[1]])))
    expect_equal(names(tax_sources(tol_info)), "ott81461")
})

test_that("tol_node unique_name", {
    skip_on_cran()
    expect_equal(unique_name(tol_info)[[1]], "Aves")
    expect_equal(names(unique_name(tol_info)), "ott81461")
})

test_that("tol_node tax_name", {
    skip_on_cran()
    expect_equal(tax_name(tol_info)[[1]], "Aves")
    expect_equal(names(tax_name(tol_info)), "ott81461")
})


test_that("tol_node source_list method", {
    skip_on_cran()
    expect_true(inherits(source_list(tol_info), "data.frame"))
    expect_true(all(names(source_list(tol_info)) %in%
                      c("study_id", "tree_id", "git_sha")))
})

test_that("tol_node tol_lineage", {
    skip_on_cran()
    expect_error(tol_lineage(tol_info), "needs to be created")
    expect_true(inherits(tol_lineage(tol_lin), "data.frame"))
    expect_true(nrow(tol_lineage(tol_lin)) > 1)
    expect_true(all(names(tol_lineage(tol_lin)) %in% c("node_id",
                                                       "num_tips",
                                                       "is_taxon")))
    expect_true(all(grepl("^(ott|mrca)", tol_lineage(tol_lin)[["node_id"]])))
})

test_that("tol_node tax_lineage", {
    skip_on_cran()
    expect_error(tax_lineage(tol_info), "needs to be created")
    expect_true(inherits(tax_lineage(tol_lin), "data.frame"))
    expect_true(nrow(tax_lineage(tol_lin)) > 1)
    expect_true(all(names(tax_lineage(tol_lin)) %in% c("rank",
                                                       "name",
                                                       "unique_name")))
    expect_true(any(grepl("no rank", tax_lineage(tol_lin)[["rank"]])))
    expect_true(any(grepl("cellular organisms", tax_lineage(tol_lin)[["name"]])))
})
