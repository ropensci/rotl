context("taxonomy")

############################################################################
## taxonomy about                                                         ##
############################################################################

test_that("taxonomy_about is a list", {
    tt <- taxonomy_about()
    expect_true(inherits(tt, "list"))
})

test_that("taxonomy_about has the names listed in documentation (if it breaks update documentation)", {
    tt <- taxonomy_about()
    expect_true(all(names(tt) %in% c("weburl", "author", "source")))
})


############################################################################
## taxon Info                                                             ##
############################################################################

test_that("taxonomy taxon info", {
    tid <- 515698
    tt <- taxonomy_taxon(tid)
    expect_equal(tt[[1]]$`ot:ottId`[1], tid)
    expect_true(inherits(tt, "taxon_info"))
})

tid <- c(924443, 337928, 631176)
tax_info <- taxonomy_taxon(tid)

test_that("taxonomy_taxon tax_rank method", {
    expect_equal(names(tax_rank(tax_info)), as.character(tid))
    expect_equal(unname(tax_rank(tax_info)), rep("genus", 3))
})

test_that("taxonomy_taxon ott_taxon_name method", {
    expect_equal(names(ott_taxon_name(tax_info)), as.character(tid))
    expect_equal(unname(ott_taxon_name(tax_info)), c("Holothuria", "Acanthaster", "Diadema"))
})

test_that("taxonomy_taxon node_id method", {
    expect_equal(names(node_id(tax_info)), as.character(tid))
    expect_equal(unname(node_id(tax_info)), c(3315679, 3297625, 3312119))
})

############################################################################
## taxon subtree                                                          ##
############################################################################

test_that("taxonomy subtree raw output", {
              tt <- taxonomy_subtree(515698, output_format = "raw")
              expect_true(inherits(tt, "list"))
              expect_identical(names(tt), "subtree")
          })

test_that("taxonomy subtree returns warning if file is provided with something else than newick output",
          expect_warning(taxonomy_subtree(515698, output_format = "raw", file = "/foo/bar"),
                         "ignored"))

test_that("taxonomy subtree writes a 'valid' newick file", {
              ff <- tempfile(fileext = ".tre")
              tt <- taxonomy_subtree(515698, output_format = "newick", file = ff)
              expect_true(tt)
              expect_true(grepl("^\\(", readLines(ff, n = 1)))
          })

test_that("taxonomy subtree returns a valid newick string", {
              tt <- taxonomy_subtree(515698, output_format = "newick")
              expect_true(inherits(ape::read.tree(text = tt), "phylo"))
          })

test_that("taxonomy subtree returns a valid phylo object", {
              tt <- taxonomy_subtree(515698, output_format = "phylo")
              expect_true(inherits(tt, "phylo"))
          })

test_that("taxonomy subtree returns valid internal node names", {
              tt <- taxonomy_subtree(515698, output_format = "taxa_internal")
              expect_equal(length(tt), 2)
          })

test_that("taxonomy subtree returns valid species names", {
              tt <- taxonomy_subtree(515698, output_format = "taxa_species")
              expect_equal(length(tt), 14)
          })

test_that("taxonomy subtree returns valid internal + species names", {
              tt <- taxonomy_subtree(515698, output_format = "taxa_all")
              expect_equal(length(tt), 16)
          })

############################################################################
## taxonomic LICA                                                         ##
############################################################################

tax_lica <- taxonomy_lica(ott_id = c(515698,590452,409712,643717))

test_that("taxonomic least inclusive comman ancestor",
          expect_true(inherits(tax_lica, "taxon_lica"))
          )

test_that("lica tax_rank method",
          expect_equal(tax_rank(tax_lica), "order")
          )

test_that("lica ott_taxon_name method",
          expect_equal(ott_taxon_name(tax_lica), "Asterales")
          )

test_that("lica node_id method",
          expect_equal(node_id(tax_lica), 3940323)
          )

test_that("lica ott_id method",
          expect_equal(ott_id(tax_lica), 1042120)
          )
