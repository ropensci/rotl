context("test of studies")

############################################################################
## studies_properties                                                     ##
############################################################################

test_that("studies_properties is a list with 2 elements (if breaks, need to update documentation)", {
    skip_on_cran()
    expect_true(all(names(studies_properties() %in% c("tree_properties", "study_properties"))))
})


############################################################################
## get_study                                                              ##
############################################################################

test_that("get_study returns an error when asking for a study that doesn't exist", {
    skip_on_cran()
    expect_error(get_study("tt_666666"))
})

test_that("get_study generates a phylo object", {
    skip_on_cran()
    tr <- get_study("pg_719", object_format = "phylo")
    expect_true(inherits(tr, "multiPhylo"))
    expect_equal(length(tr), 3)
    expect_true(length(tr[[1]]$tip.label) > 1)
})

test_that("get_study returns an error if file is specied but file_format is not", {
    skip_on_cran()
    expect_error(get_study("pg_719", file = "test"),
                 "must be specified")
})

test_that("get_study generates a nexml object", {
    skip_on_cran()
    tr <- get_study("pg_719", object_format = "nexml")
    expect_true(inherits(tr, "nexml"))
})

test_that("get_study generates a newick file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "newick", file = ff)
    expect_true(tr)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study generates a nexus file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "nexus", file = ff)
    expect_true(tr)
    expect_true(grepl("^#NEXUS", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study generates a nexml file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "nexml", file = ff)
    expect_true(tr)
    expect_true(grepl("^<\\?xml", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study generates a json file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "json", file = ff)
    expect_true(tr)
    expect_true(grepl("^\\{", readLines(ff, n = 1, warn = FALSE)))
})



############################################################################
## get_study_tree                                                         ##
############################################################################

test_that("get_study_tree returns error when tree doesn't exist", {
    skip_on_cran()
    expect_error(get_study_tree("2655", "tree5555"))
})

test_that("get_study_tree returns error when study doesn't exist", {
    skip_on_cran()
    expect_error(get_study_tree("5555555", "tree555555"))
})


test_that("get_study_tree generates nexus file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".nex")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "nexus",
                         file = ff)
    expect_true(tt)
    expect_true(grepl("^#NEXUS", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study_tree generates newick file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "newick",
                         file = ff)
    expect_true(tt)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study_tree generates json file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".json")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "json",
                         file = ff)
    expect_true(tt)
    expect_true(grepl("^\\{", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study_tree returns a phylo object", {
    skip_on_cran()
    tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo")
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
})

### Test types of labels with phylo objects

test_that("get_study_tree returns a phylo object and ott_id for tip labels", {
    skip_on_cran()
    tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo",
                         tip_label = "ott_id")
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
    expect_true(grepl("^[0-9]+$", tt$tip.label[1]))
})

test_that("get_study_tree returns a phylo object and ott_taxon_names for tip labels", {
    skip_on_cran()
    tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo",
                         tip_label = "ott_taxon_name")
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
    expect_true(sum(!grepl("^[A-Za-z]+(_[a-z]+)?$", tt$tip.label)) < 3)
})

test_that("get_study_tree returns a phylo object and original labels for tip labels", {
    skip_on_cran()
    tt <- get_study_tree("pg_1144", "tree2324", object_format = "phylo",
                         tip_label = "original_label")
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
    expect_equal(sum(!grepl("^[A-Za-z]+_[a-z]+$", tt$tip.label)), 45)
})

### Test types of labels with files (skipping json for now because there is no good way of doing it)

test_that("get_study_tree returns an error if file is given but file format is not", {
    skip_on_cran()
    expect_error(get_study_tree(study_id="pg_1144", tree="tree2324", file = "test"),
                 "must be specified")
})

test_that("get_study_tree returns nexus file and ott_id for tip labels", {
    skip_on_cran()
    ff <- tempfile(fileext = ".nex")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "nexus",
                         tip_label = "ott_id", file = ff)
    expect_true(tt)
    tr <- rncl::read_nexus_phylo(ff)
    expect_true(length(tr$tip.label) > 1)
    expect_true(grepl("^[0-9]+$", tr$tip.label[1]))
})

test_that("get_study_tree returns a phylo object and ott_taxon_names for tip labels", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "newick",
                         tip_label = "ott_taxon_name", file = ff)
    expect_true(tt)
    tr <- rncl::read_newick_phylo(ff)
    expect_true(length(tr$tip.label) > 1)
    expect_true(sum(!grepl("^[A-Za-z]+(_[a-z]+)?$", tr$tip.label)) < 3)
})



############################################################################
## get_study_subtree                                                      ##
############################################################################

test_that("get_study_subtree returns an error when study_id doesn't exist", {
        skip_on_cran()
        expect_error(get_study_subtree("pg_55555", "tree55555", subtree_id = "node555555"))
})

test_that("get_study_subtree returns an error when tree_id doesn't exist", {
    skip_on_cran()
    expect_error(get_study_subtree("pg_1144", "tree55555", subtree_id = "node555555"))
})

## API still returns object
## test_that("get_study_subtree returns an error when the subtree_id is invalid",
##           expect_error(get_study_subtree("pg_1144", "tree2324", "foobar")))

test_that("get_study_subtree returns a phylo object", {
    skip_on_cran()
    tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                            object_format = "phylo")
    expect_true(inherits(tt, "phylo"))
    expect_true(length(tt$tip.label) > 1)
})

test_that("get_study_subtree fails if file name is given but no file format", {
    skip_on_cran()
    expect_error(get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                                   file = "test"), "must be specified")
})

test_that("get_study_subtree returns a nexus file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".nex")
    tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                            file_format = "nexus", file = ff)
    expect_true(tt)
    expect_true(grepl("^#NEXUS", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study_subtree returns a newick file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                            file_format = "newick", file = ff)
    expect_true(tt)
    expect_true(grepl("^\\(", readLines(ff, n = 1, warn = FALSE)))
})

test_that("get_study_subtree returns a json file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".json")
    tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                            file_format = "json", file = ff)
    expect_true(tt)
    expect_true(grepl("^\\{", readLines(ff, n = 1, warn = FALSE)))
})


############################################################################
## get_study_meta                                                         ##
############################################################################

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    sm <- get_study_meta("pg_719")
}

test_that("get_study meta returns a study_meta object", {
    skip_on_cran()
    expect_true(inherits(sm, "study_meta"))
})

test_that("get_tree_ids method for study_meta", {
    skip_on_cran()
    expect_equal(get_tree_ids(sm), c("tree1294", "tree1295", "tree1296"))
})

test_that("get_publication method for study_meta", {
    skip_on_cran()
    expect_equal(attr(get_publication(sm), "DOI"), "http://dx.doi.org/10.1600/036364411X605092")
})

test_that("candidate_for_synth method for study_meta", {
    skip_on_cran()
    expect_true(candidate_for_synth(sm) %in% get_tree_ids(sm))
})

test_that("get_study_year method for study_meta", {
     skip_on_cran()
     expect_equal(get_study_year(sm), 2011)
 })

############################################################################
## tol_about                                                              ##
############################################################################

test_that("tol_about returns class tol_summary", {
    skip_on_cran()
    expect_true(inherits(tol_about(), "tol_summary"))
})

test_that("study_about", {
    skip_on_cran()
    ta <- source_list(tol_about(TRUE))
    expect_true(inherits(ta, "data.frame"))
    expect_true(nrow(ta) > 100)
    expect_equal(names(ta), c("study_id","tree_id", "git_sha"))
})

############################################################################
## studies_find_studies                                                   ##
############################################################################

test_that("single study detailed=TRUE", {
              skip_on_cran()
              res <- studies_find_studies(property = "ot:studyId",
                                          value = "ot_248", detailed = TRUE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "matched_studies"))
              expect_true(all(names(res) %in% c("study_ids", "n_trees", "tree_ids",
                                                "candidate", "study_year", "title",
                                                "study_doi")))
              expect_true(nrow(res) >= 1L)
              expect_equal(res[["study_ids"]], "ot_248")
              expect_equal(res[["n_trees"]], "1")
              expect_equal(res[["candidate"]], "Tr76302")
              expect_equal(res[["study_year"]], "2014")
              expect_equal(res[["study_doi"]], "http://dx.doi.org/10.1016/j.cub.2014.06.060")
              expect_equal(res[["title"]], "'Phylogenomic Resolution of the Class Ophiuroidea Unlocks a Global Microfossil Record'")
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
})

test_that("single study detailed=FALSE", {
              skip_on_cran()
              res <- studies_find_studies(property = "ot:studyId",
                                          value = "ot_248", detailed = FALSE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "study_ids"))
              expect_true(inherits(res, "matched_studies"))
              expect_match(attr(res, "found_trees"), "list of the trees associated")
              expect_equal(names(res), "study_ids")
              expect_equal(res[1, 1], "ot_248")
              expect_equal(nrow(res), 1L)
              expect_equal(ncol(res), 1L)
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })

test_that("multiple studies detailed=TRUE", {
              skip_on_cran()
              res <- studies_find_studies(property = "ot:focalCladeOTTTaxonName",
                                          value = "Aves", detailed = TRUE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "matched_studies"))
              expect_true(all(names(res) %in% c("study_ids", "n_trees", "tree_ids",
                                                "candidate", "study_year",
                                                "title", "study_doi")))
              expect_true(nrow(res) >= 8L)
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })

test_that("multiple studies detailed=FALSE", {
              skip_on_cran()
              res <- studies_find_studies(property = "ot:focalCladeOTTTaxonName",
                                          value = "Aves", detailed = FALSE)
              expect_true(inherits(res, "study_ids"))
              expect_true(inherits(res, "matched_studies"))
              expect_true(inherits(res, "data.frame"))
              expect_equal(ncol(res), 1L)
              expect_true(nrow(res) >= 8)
              expect_equal(names(res), "study_ids")
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })


############################################################################
## studies_find_trees                                                     ##
############################################################################

test_that("studies_find_trees single study detailed=FALSE", {
              skip_on_cran()
              res <- studies_find_trees(property = "ot:studyId",
                                        value = "ot_248", detailed = FALSE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "matched_studies"))
              expect_match(attr(res, "found_trees")[[1]], "Tr76302")
              expect_equal(names(res), c("study_ids",
                                         "n_matched_trees",
                                         "match_tree_ids"))
              expect_equal(res[1, 1], "ot_248")
              expect_equal(nrow(res), 1L)
              expect_equal(ncol(res), 3L)
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })

test_that("studies_find_trees single study detailed=TRUE", {
              skip_on_cran()
              res <- studies_find_trees(property = "ot:studyId",
                                        value = "ot_248", detailed = TRUE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "matched_studies"))
              expect_equal(names(res), c("study_ids", "n_trees",
                                         "tree_ids", "candidate",
                                         "study_year", "title",
                                         "study_doi",
                                         "n_matched_trees",
                                         "match_tree_ids"))
              expect_equal(nrow(res), 1L)
              expect_equal(res[["study_ids"]], "ot_248")
              expect_equal(res[["n_trees"]], "1")
              expect_equal(res[["candidate"]], "Tr76302")
              expect_equal(res[["study_year"]], "2014")
              expect_equal(res[["study_doi"]], "http://dx.doi.org/10.1016/j.cub.2014.06.060")
              expect_equal(res[["title"]], "'Phylogenomic Resolution of the Class Ophiuroidea Unlocks a Global Microfossil Record'")
              expect_equal(res[["tree_ids"]], "Tr76302")
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })

test_that("studies_find_trees multiple studies detailed=TRUE", {
              skip_on_cran()
              res <- studies_find_trees(property = "ot:ottTaxonName",
                                        value = "Echinodermata", detailed = TRUE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "matched_studies"))
              expect_equal(names(res), c("study_ids", "n_trees",
                                         "tree_ids", "candidate",
                                         "study_year", "title",
                                         "study_doi",
                                         "n_matched_trees",
                                         "match_tree_ids"))
              expect_true(nrow(res) >= 5L)
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })

test_that("studies_find_trees multiple studies detailed=FALSE", {
              skip_on_cran()
              res <- studies_find_trees(property = "ot:ottTaxonName",
                                        value = "Echinodermata", detailed = FALSE)
              expect_true(inherits(res, "data.frame"))
              expect_true(inherits(res, "matched_studies"))
              expect_equal(names(res), c("study_ids",
                                         "n_matched_trees",
                                         "match_tree_ids"))
              expect_true(nrow(res) >= 5L)
              expect_true(length(attr(res, "metadata")) > 0)
              expect_true(length(attr(res, "found_trees")) > 0)
          })


############################################################################
## list_trees                                                             ##
############################################################################

test_that("list_trees with studies_find_studies and detailed = FALSE", {
              skip_on_cran()
              expect_match(list_trees(studies_find_studies(
                                          property = "ot:focalCladeOTTTaxonName",
                                          value = "Aves", detailed = FALSE)),
                           "If you want to get a list of the trees associated with the studies")
          })

test_that("list_trees with studies_find_studies and detailed = TRUE",  {
              skip_on_cran()
              res <- studies_find_studies(property = "ot:focalCladeOTTTaxonName",
                                          value = "Aves", detailed = TRUE)
              expect_true(inherits(list_trees(res), "list"))
              expect_true(length(list_trees(res)) >= 8)
              expect_true(sum(names(list_trees(res)) %in% c("pg_435", "ot_428",
                                                            "pg_420", "ot_429",
                                                            "ot_214", "ot_117",
                                                            "ot_116", "pg_2799")) >= 8)
          })

test_that("list_trees with studies_find_trees and detailed=FALSE", {
              skip_on_cran()
              res <- studies_find_trees(property = "ot:ottTaxonName",
                                        value = "Echinodermata", detailed = FALSE)
              lt <- list_trees(res)
              expect_true(inherits(lt, "list"))
              expect_true(length(names(lt)) >=  5L)
              expect_true(all(sapply(lt, length) >=  1L))
          })

test_that("list_trees with studies_find_trees and detailed=TRUE", {
              skip_on_cran()
              res <- studies_find_trees(property = "ot:ottTaxonName",
                                        value = "Echinodermata", detailed = TRUE)
              lt <- list_trees(res)
              expect_true(inherits(lt, "list"))
              expect_true(length(names(lt)) >=  5L)
              expect_true(all(sapply(lt, length) >=  1L))
          })
