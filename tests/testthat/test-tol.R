############################################################################
## tol_about                                                              ##
############################################################################

context("test tol_about (and in turn print.tol_summary)")

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    req <- tol_about(include_source_list = TRUE)
}

test_that("Names in object returned are correct/match the docs", {
    skip_on_cran()
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
    expect_error(source_list(tol_about(include_source_list = FALSE)),
                 "has been created using")
    expect_true(nrow(source_list(req)) > 1)
    expect_true(all(grepl("^(ot|pg)", source_list(req)[["study_id"]])))
    expect_true(all(grepl("^tr", source_list(req)[["tree_id"]], ignore.case = TRUE)))
})



test_that("tol_node tax_rank method", {
    skip_on_cran()
    expect_true(inherits(tax_rank(req),
                         c("otl_rank", "list")))
    expect_equal(tax_rank(req)[[1]], "no rank")
})

test_that("tol_node ott_id method", {
    skip_on_cran()
    expect_true(inherits(ott_id(req),
                         c("otl_ott_id", "list")))
    expect_equal(ott_id(req)[[1]], 93302)
    expect_equal(names(ott_id(req)), "cellular organisms")
})

test_that("tol_node tax_sources", {
    skip_on_cran()
    expect_true(inherits(tax_sources(req),
                         c("otl_tax_sources", "list")))
    expect_true(any(grepl("ncbi", tax_sources(req)[[1]])))
    expect_equal(names(tax_sources(req)), "cellular organisms")
})

test_that("tol_node unique_name", {
    skip_on_cran()
    expect_true(inherits(unique_name(req),
                         c("otl_unique_name", "list")))
    expect_equal(unique_name(req)[[1]], "cellular organisms")
    expect_equal(names(unique_name(req)), "cellular organisms")
})

test_that("tol_node tax_name", {
    skip_on_cran()
    expect_true(inherits(tax_name(req),
                         c("otl_name", "list")))
    expect_equal(tax_name(req)[[1]], "cellular organisms")
    expect_equal(names(tax_name(req)), "cellular organisms")
})

### ott_id() --------------------------------------------------------------------

test_that("taxonomy_taxon_info with ott_id for tol_about", {
    skip_on_cran()
    expect_equal(ott_id(req),
                 ott_id(taxonomy_taxon_info(ott_id(req))))
})

## can't do that, it's pulling the whole tree
## test_that("taxonomy_subtree with ott_id for tol_about", {
##     taxonomy_subtree(ott_id = ott_id(req))
## })

test_that("tol_node_info with ott_id for tol_about", {
    skip_on_cran()
    expect_equal(ott_id(req),
                 ott_id(tol_node_info(ott_id(req))))
})

## can't do that, it's pulling the whole tree
## test_that("tol_subtree with ott_id for tol_about", {
##     tol_subtree(ott_id = ott_id(req))
## })

test_that("tol_mrca with ott_id for tol_about", {
    skip_on_cran()
    expect_equal(ott_id(req)[1],
                 ott_id(tol_mrca(ott_id(req)))[1])
})

test_that("tol_induced_subtree with ott_id for tol_about", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_id(req)),
                 "least two valid")
})

test_that("taxonomy_mrca with ott_id for tol_about", {
    skip_on_cran()
    expect_equal(ott_id(req),
                 ott_id(taxonomy_mrca(ott_id(req))))
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
                 "Please provide a single")
})

test_that("tol_subtree fails if ott_id doesn't look like a number", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = "111A1111"),
                 "must look like numbers")
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

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    birds <- tol_mrca(ott_ids = c(412129, 536234))
    hol <- tol_mrca(c(431586, 957434))
    mono <- tol_mrca(ott_ids = c(962377, 79623))
}

test_that("tol_mrca fails if ott_ids are not numbers", {
    skip_on_cran()
    expect_error(tol_mrca(ott_ids = c(13243, "a13415")),
                 "must look like numbers")
})

test_that("tol_mrca returns a list", {
    skip_on_cran()
    expect_true(inherits(birds, "list"))
    expect_true(inherits(birds, "tol_mrca"))
    expect_true(all(names(birds) %in%
                    c("mrca",
                      "source_id_map",
                      "nearest_taxon")))
})

test_that("methods for tol_mrca where the node is a taxon", {
    skip_on_cran()
    expect_true(inherits(tax_sources(hol),
                         c("otl_tax_sources", "list")))
    expect_true(inherits(unique_name(hol),
                         c("otl_unique_name", "list")))
    expect_true(inherits(tax_name(hol),
                         c("otl_name", "list")))
    expect_true(inherits(tax_rank(hol),
                         c("otl_rank", "list")))
    expect_true(inherits(ott_id(hol),
                         c("otl_ott_id", "list")))
    expect_true(length(tax_sources(hol)[[1]]) > 1)
    expect_true(any(grepl("worms", tax_sources(hol)[[1]])))
    expect_equal(unique_name(hol)[[1]], "Holothuria")
    expect_equal(tax_name(hol)[[1]], "Holothuria")
    expect_equal(tax_rank(hol)[[1]], "genus")
    expect_equal(ott_id(hol)[[1]], 5004030)
    expect_equal(names(tax_sources(hol)), "Holothuria")
    expect_true(all(names(source_list(hol)) %in% c("tree_id",
                                                   "study_id",
                                                   "git_sha")))
    expect_equal(attr(tax_sources(hol), "taxon_type"), "mrca")
})

test_that("methods for tol_mrca where the node is not a taxon", {
    skip_on_cran()
    expect_true(inherits(birds, "list"))
    expect_true(inherits(tax_sources(birds),
                         c("otl_tax_sources", "list")))
    expect_true(inherits(unique_name(birds),
                         c("otl_unique_name", "list")))
    expect_true(inherits(tax_name(birds),
                         c("otl_name", "list")))
    expect_true(inherits(tax_rank(birds),
                         c("otl_rank", "list")))
    expect_true(inherits(ott_id(birds),
                         c("otl_ott_id", "list")))
    expect_true(length(tax_sources(birds)[[1]]) >=  1)
    expect_true(any(grepl("ncbi", tax_sources(birds)[[1]])))
    expect_equal(unique_name(birds)[[1]], "Neognathae")
    expect_equal(tax_name(birds)[[1]], "Neognathae")
    expect_equal(tax_rank(birds)[[1]], "superorder")
    expect_equal(ott_id(birds)[[1]], 241846)
    expect_equal(names(ott_id(birds)), "Neognathae")
    expect_true(all(names(source_list(birds)) %in% c("tree_id",
                                                          "study_id",
                                                          "git_sha")))
    expect_equal(attr(tax_sources(birds), "taxon_type"), "nearest_taxon")
})

### ott_id() --------------------------------------------------------------------

test_that("taxonomy_taxon_info with ott_id for tol_mrca", {
    skip_on_cran()
    expect_equal(ott_id(mono)[1],
                 ott_id(taxonomy_taxon_info(ott_id(mono)))[1])
})

test_that("taxonomy_subtree with ott_id for tol_mrca", {
    skip_on_cran()
    tt <- taxonomy_subtree(ott_id = ott_id(mono))
    expect_true(length(tt[["tip_label"]]) > 10)
    expect_true(length(tt[["edge_label"]]) > 10)
})

test_that("tol_node_info with ott_id for tol_mrca", {
    skip_on_cran()
    expect_equal(ott_id(mono)[1],
                 ott_id(tol_node_info(ott_id(mono)))[1])
})

test_that("tol_subtree with ott_id for tol_mrca", {
    skip_on_cran()
    tt <- tol_subtree(ott_id = ott_id(mono))
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
    expect_true(length(tt$node.label) > 1)
})

test_that("tol_mrca with ott_id for tol_mrca", {
    skip_on_cran()
    expect_equal(ott_id(mono)[1],
                 ott_id(tol_mrca(ott_id(mono)))[1])
})

test_that("tol_induced_subtree with ott_id for tol_mrca", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_id(mono)),
                 "least two valid")
})

test_that("taxonomy_mrca with ott_id for tol_mrca", {
    skip_on_cran()
    expect_equivalent(ott_id(mono),
                      ott_id(taxonomy_mrca(ott_id(mono))))
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
    tol_mono <- tol_node_info(ott_id = 962396)
}

test_that("tol node info.", {
    skip_on_cran()
    expect_true(all(names(tol_info) %in%
                      c("partial_path_of", "supported_by", "source_id_map", "taxon",
                        "num_tips", "terminal", "node_id")))
    expect_true(inherits(tol_info, "tol_node"))
})


### methods ---------------------------------------------------------------------

test_that("tol_node tax_rank method", {
    skip_on_cran()
    expect_true(inherits(tax_rank(tol_info),
                         c("otl_tax_rank", "list")))
    expect_equal(tax_rank(tol_info)[[1]], "class")
})

test_that("tol_node ott_id method", {
    skip_on_cran()
    expect_true(inherits(ott_id(tol_info),
                         c("otl_ott_id", "list")))
    expect_equal(ott_id(tol_info)[[1]], 81461)
    expect_equal(names(ott_id(tol_info)), "Aves")
})

test_that("tol_node tax_sources", {
    skip_on_cran()
    expect_true(inherits(tax_sources(tol_info),
                         c("otl_tax_sources", "list")))
    expect_true(any(grepl("worms", tax_sources(tol_info)[[1]])))
    expect_equal(names(tax_sources(tol_info)), "Aves")
})

test_that("tol_node unique_name", {
    skip_on_cran()
    expect_true(inherits(unique_name(tol_info),
                         c("otl_unique_name", "list")))
    expect_equal(unique_name(tol_info)[[1]], "Aves")
    expect_equal(names(unique_name(tol_info)), "Aves")
})

test_that("tol_node tax_name", {
    skip_on_cran()
    expect_true(inherits(tax_name(tol_info),
                         c("otl_name", "list")))
    expect_equal(tax_name(tol_info)[[1]], "Aves")
    expect_equal(names(tax_name(tol_info)), "Aves")
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
                                                       "unique_name",
                                                       "ott_id")))
    expect_true(any(grepl("no rank", tax_lineage(tol_lin)[["rank"]])))
    expect_true(any(grepl("cellular organisms", tax_lineage(tol_lin)[["name"]])))
})

### ott_id() --------------------------------------------------------------------

test_that("taxonomy_taxon_info with ott_id for tol_info", {
    skip_on_cran()
    expect_equivalent(ott_id(tol_mono),
                 ott_id(taxonomy_taxon_info(ott_id(tol_mono))))
})

test_that("taxonomy_subtree with ott_id for tol_info", {
    skip_on_cran()
    tt <- taxonomy_subtree(ott_id = ott_id(tol_mono))
    expect_true(length(tt[["tip_label"]]) > 10)
    expect_true(length(tt[["edge_label"]]) > 10)
})

test_that("tol_node_info with ott_id for tol_info", {
    skip_on_cran()
    expect_equivalent(ott_id(tol_mono),
                 ott_id(tol_node_info(ott_id(tol_mono))))
})

test_that("tol_subtree with ott_id for tol_info", {
    skip_on_cran()
    tt <- tol_subtree(ott_id = ott_id(tol_mono))
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
    expect_true(length(tt$node.label) > 1)
})

test_that("tol_mrca with ott_id for tol_info", {
    skip_on_cran()
    expect_equivalent(ott_id(tol_mono),
                 ott_id(tol_mrca(ott_id(tol_mono))))
})

test_that("tol_induced_subtree with ott_id for tol_info", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_id(tol_mono)),
                 "least two valid")
})

test_that("taxonomy_mrca with ott_id for tol_info", {
    skip_on_cran()
    expect_equivalent(ott_id(tol_mono),
                      ott_id(taxonomy_mrca(ott_id(tol_mono))))
})
