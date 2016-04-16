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
    expect_equal(names(tax_rank(tax_info)), as.character(tid))
    expect_equal(unname(tax_rank(tax_info)), rep("genus", 3))
})

test_that("taxonomy_taxon ott_taxon_name method", {
    skip_on_cran()
    expect_equal(names(tax_name(tax_info)), as.character(tid))
    expect_equal(unname(tax_name(tax_info)), c("Holothuria", "Acanthaster", "Diadema"))
})

test_that("taxonomy_taxon synonyms method", {
    skip_on_cran()
    expect_equal(names(synonyms(tax_info)), as.character(tid))
    expect_true(all(c("Diamema", "Centrechinus") %in% synonyms(tax_info)[[3]]))
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
## taxonomic LICA                                                         ##
############################################################################

 if (identical(Sys.getenv("NOT_CRAN"), "true"))  {
     tax_mrca <- taxonomy_mrca(ott_id = c(515698,590452,643717))
 }

test_that("taxonomic least inclusive comman ancestor", {
    skip_on_cran()
    expect_true(inherits(tax_mrca, "taxon_mrca"))
})

test_that("mrca tax_rank method", {
    skip_on_cran()
    expect_equal(tax_rank(tax_mrca), "order")
})

test_that("mrca ott_taxon_name method", {
    skip_on_cran()
    expect_equal(tax_name(tax_mrca), "Asterales")
})

test_that("mrca ott_id method", {
    skip_on_cran()
    expect_equal(ott_id(tax_mrca), 1042120)
})

test_that("mrca unique_name method", {
    skip_on_cran()
    expect_equal(unique_name(tax_mrca), "Asterales")
})

test_that("mrca tax_sources method", {
    skip_on_cran()
    expect_equal(tax_sources(tax_mrca),
                 c("ncbi:4209", "worms:234044",
                   "gbif:414","irmng:10011"))
})
