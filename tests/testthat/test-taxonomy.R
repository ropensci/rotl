context("taxonomy")

############################################################################
## taxonomy about                                                         ##
############################################################################

test_that("taxonomy_about is a list", {
    skip_on_cran()
    tt <- taxonomy_about()
    expect_true(inherits(tt, "list"))
})

test_that("taxonomy_about has the names listed in documentation (if it breaks update documentation)", {
    skip_on_cran()
    tt <- taxonomy_about()
    expect_true(all(names(tt) %in% c("weburl", "author", "name", "source", "version")))
})


############################################################################
## taxon Info                                                             ##
############################################################################

test_that("taxonomy taxon info", {
    skip_on_cran()
    tid <- 515698
    tt <- taxonomy_taxon_info(tid)
    expect_equal(tt[[1]][["ott_id"]], tid)
    expect_true(inherits(tt, "taxon_info"))
})

test_that("taxonomy with include_lineage=TRUE", {
    skip_on_cran()
    tt <- taxonomy_taxon_info(515698, include_lineage = TRUE)
    expect_true(exists("lineage", tt[[1]]))
    expect_true(length(tt[[1]]$lineage) > 1)
})

test_that("taxonomy with include_lineage=FALSE", {
    skip_on_cran()
    tt <- taxonomy_taxon_info(515698, include_lineage = FALSE)
    expect_false(exists("lineage", tt[[1]]))
})

test_that("taxonomy with include_terminal_descendants=TRUE", {
    skip_on_cran()
    tt <- taxonomy_taxon_info(515698, include_terminal_descendants = TRUE)
    expect_true(exists("terminal_descendants", tt[[1]]))
    expect_true(length(tt[[1]][["terminal_descendants"]]) > 1)
})

test_that("taxonomy with include_terminal_descendants=FALSE", {
    skip_on_cran()
    tt <- taxonomy_taxon_info(515698, include_terminal_descendants = FALSE)
    expect_false(exists("terminal_descendants", tt[[1]]))
})

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    tid <- c(5004030, 337928, 631176)
    tax_info <- taxonomy_taxon_info(tid)
}

test_that("taxonomy_taxon tax_rank method", {
    skip_on_cran()
    expect_true(inherits(tax_rank(tax_info),
                         c("otl_tax_rank", "list")))
    expect_equal(names(tax_rank(tax_info)),
                 c("Holothuria", "Acanthaster",
                   "Diadema (genus in Holozoa)"))
    expect_equal(unlist(unname(tax_rank(tax_info))),
                 rep("genus", 3))
})

test_that("taxonomy_taxon ott_taxon_name method", {
    skip_on_cran()
    expect_true(inherits(tax_name(tax_info),
                         c("otl_tax_info", "list")))
    expect_equal(names(tax_name(tax_info)),
                 c("Holothuria", "Acanthaster",
                   "Diadema (genus in Holozoa)"))
    expect_equal(unlist(unname(tax_name(tax_info))),
                 c("Holothuria", "Acanthaster", "Diadema"))
})

test_that("taxonomy_taxon synonyms method", {
    skip_on_cran()
    expect_true(inherits(synonyms(tax_info),
                         c("otl_synonyms", "list")))
    expect_equal(names(synonyms(tax_info)),
                 c("Holothuria", "Acanthaster",
                   "Diadema (genus in Holozoa)"))
    expect_true(all(c("Diamema", "Centrechinus") %in%
                    synonyms(tax_info)[[3]]))
})

test_that("taxonomy_taxon is_suppressed method", {
    skip_on_cran()
    expect_true(inherits(is_suppressed(tax_info),
                         c("otl_is_suppressed", "list")))
    expect_equal(names(is_suppressed(tax_info)),
                 c("Holothuria", "Acanthaster",
                   "Diadema (genus in Holozoa)"))
    expect_equal(unlist(unname(is_suppressed(tax_info))),
                 c(FALSE, FALSE, FALSE))
})

test_that("taxonomy_taxon flags method", {
    skip_on_cran()
    expect_true(inherits(flags(tax_info),
                         c("otl_flags", "list")))
    expect_equal(names(flags(tax_info)),
                 c("Holothuria", "Acanthaster",
                   "Diadema (genus in Holozoa)"))
    expect_equal(unlist(unname(flags(tax_info))),
                 NULL)
})

test_that("higher taxonomy method", {
    skip_on_cran()
    expect_error(tax_lineage(tax_info), "needs to be created")
    lg <- tax_lineage(taxonomy_taxon_info(tid, include_lineage = TRUE))
    expect_true(inherits(lg, "list"))
    expect_true(inherits(lg[[1]], "data.frame"))
    expect_true(all(names(lg[[1]]) %in% c("rank", "name", "unique_name", "ott_id")))
    expect_true(any(grepl("no rank", lg[[1]][["rank"]])))
    expect_true(any(grep("life", lg[[1]][["name"]])))
})

### ott_id() --------------------------------------------------------------------

test_that("taxonomy_taxon_info with ott_id for tax_info", {
    skip_on_cran()
    expect_equivalent(ott_id(tax_info),
                 ott_id(taxonomy_taxon_info(ott_id(tax_info))))
})

test_that("taxonomy_subtree with ott_id for tax_info", {
    skip_on_cran()
    expect_error(taxonomy_subtree(ott_id = ott_id(tax_info)),
                 "supply one")
})

test_that("tol_node_info with ott_id for tax_info", {
    skip_on_cran()
    expect_error(tol_node_info(ott_id(tax_info)),
                 "provide a single")
})

test_that("tol_subtree with ott_id for tax_info", {
    skip_on_cran()
    expect_error(tol_subtree(ott_id = ott_id(tax_info)),
                 "provide a single")
})

test_that("tol_mrca with ott_id for tax_info", {
    skip_on_cran()
    expect_equivalent(list("Euleutheroza" = 317277),
                      ott_id(tol_mrca(ott_id(tax_info))))
})

test_that("tol_induced_subtree with ott_id for tax_info", {
    skip_on_cran()
    expect_true(inherits(tol_induced_subtree(ott_id(tax_info)),
                         "phylo"))
})

test_that("taxonomy_mrca with ott_id for tax_info", {
    skip_on_cran()
    expect_equivalent(list("Euleutheroza" = 317277),
                      ott_id(taxonomy_mrca(ott_id(tax_info))))
})


############################################################################
## taxon subtree                                                          ##
############################################################################

test_that("taxonomy subtree raw output", {
    skip_on_cran()
    tt <- taxonomy_subtree(515698, output_format = "raw")
    expect_true(inherits(tt, "list"))
    expect_identical(names(tt), "newick")
})

test_that("taxonomy subtree returns warning if file is provided with something else than newick output", {
    skip_on_cran()
    expect_warning(taxonomy_subtree(515698, output_format = "raw", file = "/foo/bar"),
                   "ignored")
})

test_that("taxonomy subtree writes a 'valid' newick file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tt <- taxonomy_subtree(515698, output_format = "newick", file = ff)
    expect_true(tt)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})

test_that("taxonomy subtree returns a valid newick string", {
    skip_on_cran()
    tt <- taxonomy_subtree(515698, output_format = "newick")
    expect_true(inherits(ape::read.tree(text = tt), "phylo"))
})

test_that("taxonomy subtree returns a valid phylo object", {
    skip_on_cran()
    tt <- taxonomy_subtree(515698, output_format = "phylo")
    expect_true(inherits(tt, "phylo"))
})

test_that("taxonomy subtree returns valid internal node names", {
    skip_on_cran()
    tt <- taxonomy_subtree(515698, output_format = "taxa")
    expect_true(inherits(tt, "list"))
    expect_equal(length(tt), 2)
    expect_equal(length(tt$tip_label), 14)
    expect_equal(length(tt$edge_label), 2)
})

test_that("taxonomy subtree works if taxa has only 1 descendant", {
    skip_on_cran()
    tt <- taxonomy_subtree(ott_id = 3658331, output_format = "taxa")
    expect_true(inherits(tt, "list"))
    expect_equal(length(tt), 2)
    expect_true(inherits(tt$tip_label, "character"))
})

############################################################################
## taxonomic MRCA                                                         ##
############################################################################

 if (identical(Sys.getenv("NOT_CRAN"), "true"))  {
     tax_mrca <- taxonomy_mrca(ott_id = c(515698, 590452, 643717))
     tax_mrca_mono <- taxonomy_mrca(ott_id = c(79623, 962377))
 }

test_that("taxonomic most recent common ancestor", {
    skip_on_cran()
    expect_true(inherits(tax_mrca, "taxon_mrca"))
    expect_true(inherits(tax_mrca, "list"))
})

test_that("mrca tax_rank method", {
    skip_on_cran()
    expect_equal(tax_rank(tax_mrca)[1],
                 list("Asterales" = "order"))
})

test_that("mrca tax_name method", {
    skip_on_cran()
    expect_equal(tax_name(tax_mrca)[1],
                 list("Asterales" = "Asterales"))
})

test_that("mrca ott_id method", {
    skip_on_cran()
    expect_equal(ott_id(tax_mrca)[1],
                 list("Asterales" = 1042120))
    expect_true(inherits(ott_id(tax_mrca), "otl_ott_id"))
})

test_that("mrca unique_name method", {
    skip_on_cran()
    expect_equal(unique_name(tax_mrca)[1],
                 list("Asterales" = "Asterales"))
    expect_true(inherits(unique_name(tax_mrca),
                         "otl_unique_name"))
})

test_that("mrca tax_sources method", {
    skip_on_cran()
    expect_equal(tax_sources(tax_mrca)[1],
                 list("Asterales" =
                 c("ncbi:4209", "worms:234044",
                   "gbif:414", "irmng:10011")))
    expect_true(inherits(tax_sources(tax_mrca),
                         "otl_tax_sources"))
})

test_that("mrca is_suppressed method", {
    skip_on_cran()
    expect_true(inherits(is_suppressed(tax_mrca),
                         c("otl_is_suppressed", "list")))
    expect_equal(is_suppressed(tax_mrca)[1],
                 list("Asterales" = FALSE))
})

test_that("mrca flags method", {
    skip_on_cran()
    expect_true(inherits(flags(tax_mrca),
                         c("otl_flags", "list")))
    expect_equal(flags(tax_mrca)[1],
                 list("Asterales" = NULL))
})

### ott_id() --------------------------------------------------------------------

test_that("taxonomy_taxon_info with ott_id for tax_mrca", {
    skip_on_cran()
    expect_equivalent(ott_id(tax_mrca_mono),
                 ott_id(taxonomy_taxon_info(ott_id(tax_mrca_mono))))
})

test_that("taxonomy_subtree with ott_id for tax_mrca", {
    skip_on_cran()
    tt <- taxonomy_subtree(ott_id = ott_id(tax_mrca_mono))
    expect_true(length(tt[["tip_label"]]) > 10)
    expect_true(length(tt[["edge_label"]]) > 1)
})

test_that("tol_node_info with ott_id for tax_mrca", {
    skip_on_cran()
    expect_equivalent(ott_id(tax_mrca_mono),
                 ott_id(tol_node_info(ott_id(tax_mrca_mono))))
})

test_that("tol_subtree with ott_id for tax_mrca", {
    skip_on_cran()
    tt <- tol_subtree(ott_id = ott_id(tax_mrca_mono))
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
    expect_true(length(tt$node.label) > 1)
})

test_that("tol_mrca with ott_id for tax_mrca", {
    skip_on_cran()
    expect_equivalent(ott_id(tax_mrca_mono),
                 ott_id(tol_mrca(ott_id(tax_mrca_mono))))
})

test_that("tol_induced_subtree with ott_id for tax_mrca", {
    skip_on_cran()
    expect_error(tol_induced_subtree(ott_id(tax_mrca_mono)),
                 "least two valid")
})

test_that("taxonomy_mrca with ott_id for tax_mrca", {
    skip_on_cran()
    expect_equivalent(ott_id(tax_mrca_mono),
                      ott_id(taxonomy_mrca(ott_id(tax_mrca_mono))))
})
