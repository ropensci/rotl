context("test of studies")


############################################################################
## get_study                                                              ##
############################################################################

test_that("get_study returns an error when asking for a study that doesn't exist",
          expect_error(get_study("tt_666666"), "GET failure"))

test_that("get_study generates a phylo object", {
              tr <- get_study("pg_719", object_format = "phylo")
              expect_true(inherits(tr, "multiPhylo"))
              expect_equal(length(tr), 3)
              expect_true(length(tr[[1]]$tip.label) > 1)
          })

test_that("get_study generates a nexml object", {
              tr <- get_study("pg_719", object_format = "nexml")
              expect_true(inherits(tr, "nexml"))
          })

test_that("get_study throws error if no file name given, but asking for one",
          expect_error(get_study("pg_719", text_format = "newick"),
                       "must specify a file")
          )

test_that("get_study generates a newick file", {
              ff <- tempfile()
              tr <- get_study("pg_719", text_format = "newick", file = ff)
              expect_true(tr)
              expect_true(grepl("^\\(", readLines(ff, n = 1)))
          })

test_that("get_study generates a nexus file", {
              ff <- tempfile()
              tr <- get_study("pg_719", text_format = "nexus", file = ff)
              expect_true(tr)
              expect_true(grepl("^#NEXUS", readLines(ff, n = 1)))
          })

test_that("get_study generates a nexml file", {
              ff <- tempfile()
              tr <- get_study("pg_719", text_format = "nexml", file = ff)
              expect_true(tr)
              expect_true(grepl("^<\\?xml", readLines(ff, n = 1)))
          })

test_that("get_study generates a json file", {
              ff <- tempfile()
              tr <- get_study("pg_719", text_format = "json", file = ff)
              expect_true(tr)
              expect_true(grepl("^\\{", readLines(ff, n = 1)))
          })



############################################################################
## get_study_tree                                                         ##
############################################################################

test_that("get_study_tree returns error when tree doesn't exist",
          expect_error(get_study_tree("2655", "tree5555"),
                       "not found in study"))

test_that("get_study_tree returns error when study doesn't exist",
          expect_error(get_study_tree("5555555", "tree555555"),
                       "GET failure"))


test_that("get_study_tree generates nexus file", {
              ff <- tempfile(fileext = ".nex")
              tt <- get_study_tree("pg_1144", "tree2324", text_format = "nexus",
                                   file = ff)
              expect_true(tt)
              expect_true(grepl("^#NEXUS", readLines(ff, n = 1)))
          })

test_that("get_study_tree generates newick file", {
              ff <- tempfile(fileext = ".tre")
              tt <- get_study_tree("pg_1144", "tree2324", text_format = "newick",
                                   file = ff)
              expect_true(tt)
              expect_true(grepl("^\\(", readLines(ff, n = 1)))
          })

test_that("get_study_tree generates json file", {
              ff <- tempfile(fileext = ".json")
              tt <- get_study_tree("pg_1144", "tree2324", text_format = "json",
                                   file = ff)
              expect_true(tt)
              expect_true(grepl("^\\{", readLines(ff, n = 1)))
          })

test_that("get_study_tree returns a phylo object", {
              tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo")
              expect_true(inherits(tt, "phylo"))
              expect_true(length(tt$tip.label) > 1)
          })

### Test types of labels with phylo objects

test_that("get_study_tree returns a phylo object and ott_id for tip labels", {
              tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo",
                                   tip_label = "ott_id")
              expect_true(inherits(tt, "phylo"))
              expect_true(length(tt$tip.label) > 1)
              expect_true(grepl("^[0-9]+$", tt$tip.label[1]))
          })

test_that("get_study_tree returns a phylo object and ott_taxon_names for tip labels", {
              tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo",
                                   tip_label = "ott_taxon_name")
              expect_true(inherits(tt, "phylo"))
              expect_true(length(tt$tip.label) > 1)
              expect_true(sum(!grepl("^[A-Za-z]+(_[a-z]+)?$", tt$tip.label)) < 3)
          })

test_that("get_study_tree returns a phylo object and original labels for tip labels", {
              tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo",
                                   tip_label = "original_label")
              expect_true(inherits(tt, "phylo"))
              expect_true(length(tt$tip.label) > 1)
              expect_equal(sum(!grepl("^[A-Za-z]+_[a-z]+$", tt$tip.label)), 45)
          })

### Test types of labels with files (skipping json for now because there is no good way of doing it)

test_that("get_study_tree returns nexus file and ott_id for tip labels", {
              ff <- tempfile(fileext = ".nex")
              tt <- get_study_tree("pg_1144", "tree2324", text_format = "nexus",
                                   tip_label = "ott_id", file = ff)
              expect_true(tt)
              tr <- rncl::read_nexus_phylo(ff)
              expect_true(length(tr$tip.label) > 1)
              expect_true(grepl("^[0-9]+$", tr$tip.label[1]))
          })

test_that("get_study_tree returns a phylo object and ott_taxon_names for tip labels", {
              ff <- tempfile(fileext = ".tre")
              tt <- get_study_tree("pg_1144", "tree2324", text_format = "newick",
                                   tip_label = "ott_taxon_name", file = ff)
              expect_true(tt)
              tr <- rncl::read_newick_phylo(ff)
              expect_true(length(tr$tip.label) > 1)
              expect_true(sum(!grepl("^[A-Za-z]+(_[a-z]+)?$", tr$tip.label)) < 3)
          })



############################################################################
## get_study_subtree                                                      ##
############################################################################

test_that("get_study_subtree returns an error when study_id doesn't exist",
          expect_error(get_study_subtree("pg_55555", "tree55555", subtree_id = "node555555"),
                       "GET failure"))

test_that("get_study_subtree returns an error when tree_id doesn't exist",
          expect_error(get_study_subtree("pg_1144", "tree55555", subtree_id = "node555555"),
                       "subresource .+ not found in study"))

## API still returns object
## test_that("get_study_subtree returns an error when the subtree_id is invalid",
##           expect_error(get_study_subtree("pg_1144", "tree2324", "foobar")))

test_that("get_study_subtree returns a phylo object", {
              tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                                      object_format = "phylo")
              expect_true(inherits(tt, "phylo"))
              expect_true(length(tt$tip.label) > 1)
          })

test_that("get_study_subtree returns a nexus file", {
              ff <- tempfile(fileext = ".nex")
              tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                                      text_format = "nexus", file = ff)
              expect_true(tt)
              expect_true(grepl("^#NEXUS", readLines(ff, n = 1)))
          })

test_that("get_study_subtree returns a newick file", {
              ff <- tempfile(fileext = ".tre")
              tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                                      text_format = "newick", file = ff)
              expect_true(tt)
              expect_true(grepl("^\\(", readLines(ff, n = 1)))
          })

test_that("get_study_subtree returns a json file", {
          ff <- tempfile(fileext = ".json")
          tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                                  text_format = "json", file = ff)
          expect_true(tt)
          expect_true(grepl("^\\{", readLines(ff, n = 1)))
      })
