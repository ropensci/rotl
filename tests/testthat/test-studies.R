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
    expect_error(get_study("tt_666666"), "GET failure")
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
    expect_true(grepl("^\\(", readLines(ff, n = 1)))
})

test_that("get_study generates a nexus file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "nexus", file = ff)
    expect_true(tr)
    expect_true(grepl("^#NEXUS", readLines(ff, n = 1)))
})

test_that("get_study generates a nexml file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "nexml", file = ff)
    expect_true(tr)
    expect_true(grepl("^<\\?xml", readLines(ff, n = 1)))
})

test_that("get_study generates a json file", {
    skip_on_cran()
    ff <- tempfile()
    tr <- get_study("pg_719", file_format = "json", file = ff)
    expect_true(tr)
    expect_true(grepl("^\\{", readLines(ff, n = 1)))
})



############################################################################
## get_study_tree                                                         ##
############################################################################

test_that("get_study_tree returns error when tree doesn't exist", {
        skip_on_cran()
    expect_error(get_study_tree("2655", "tree5555"),
                 "not found in study")
})

test_that("get_study_tree returns error when study doesn't exist", {
    skip_on_cran()
    expect_error(get_study_tree("5555555", "tree555555"),
                 "GET failure")
})


test_that("get_study_tree generates nexus file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".nex")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "nexus",
                         file = ff)
    expect_true(tt)
    expect_true(grepl("^#NEXUS", readLines(ff, n = 1)))
})

test_that("get_study_tree generates newick file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "newick",
                         file = ff)
    expect_true(tt)
    expect_true(grepl("^\\(", readLines(ff, n = 1)))
})

test_that("get_study_tree generates json file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".json")
    tt <- get_study_tree("pg_1144", "tree2324", file_format = "json",
                         file = ff)
    expect_true(tt)
    expect_true(grepl("^\\{", readLines(ff, n = 1)))
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
    expect_error(get_study_subtree("pg_55555", "tree55555", subtree_id = "node555555"),
                 "GET failure")
})

test_that("get_study_subtree returns an error when tree_id doesn't exist", {
    skip_on_cran()
    expect_error(get_study_subtree("pg_1144", "tree55555", subtree_id = "node555555"),
                 "subresource .+ not found in study")
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
    expect_true(grepl("^#NEXUS", readLines(ff, n = 1)))
})

test_that("get_study_subtree returns a newick file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".tre")
    tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                            file_format = "newick", file = ff)
    expect_true(tt)
    expect_true(grepl("^\\(", readLines(ff, n = 1)))
})

test_that("get_study_subtree returns a json file", {
    skip_on_cran()
    ff <- tempfile(fileext = ".json")
    tt <- get_study_subtree("pg_1144", "tree2324", subtree_id = "ingroup",
                            file_format = "json", file = ff)
    expect_true(tt)
    expect_true(grepl("^\\{", readLines(ff, n = 1)))
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


############################################################################
## tol_about                                                              ##
############################################################################

test_that("tol_about returns class tol_summary", {
    skip_on_cran()
    expect_true(inherits(tol_about(), "tol_summary"))
})

test_that("study_about", {
    skip_on_cran()
    ta <- study_list(tol_about(TRUE))
    expect_true(inherits(ta, "data.frame"))
    expect_true(nrow(ta) > 100)
    expect_equal(names(ta), c("tree_id", "study_id", "git_sha"))
})
