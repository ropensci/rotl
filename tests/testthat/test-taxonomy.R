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


### taxon Info

test_that("taxonomy taxon info", {
              tid <- 515698
              tt <- taxonomy_taxon(tid)
              expect_equal(tt$`ot:ottId`[1], tid)
          })


## taxon subtree
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

### taxonomic LICA
test_that("taxonomic least inclusive comman ancestor",
          expect_identical(taxonomy_lica(ott_id = c(515698,590452,409712,643717))$lica$synonyms[[1]],
                       "Asterales"))
