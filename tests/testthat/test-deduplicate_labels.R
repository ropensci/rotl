tr_string <- "
((A,A),A 1); ((B.1,B,C),B);
((D,D_1),D.1);
((('A 1','A 1'),A.1),'A 1');
((('A A A','A A A'),A.1),'A 1');

((((A_1:0.1,B__2:0.1)cats:0.1,(A_1:0.1,A_1:0.1)dogs:0.1)mammals:0.1):0.1)fur:0.1;
"
file_dup <- tempfile()
cat(tr_string, file = file_dup, sep = "\n")

############################################################################
## parse_newick                                                           ##
############################################################################

context("parse_newick")
test_that("parse newick works correctly", {
   prsed_str <- parse_newick(file_dup)
   expect_true(is.character(prsed_str))
   expect_equal(length(prsed_str), 6L)
})

############################################################################
## deduplicate_labels                                                     ##
############################################################################

context("deduplicate_labels")

test_that("deduplicate labels works on made up example", {
   expect_warning(dedup_tr <- deduplicate_labels(file_dup),
                  "Some tip labels were duplicated")
   expect_true(file.exists(dedup_tr))
   phylo_tr <- rncl::read_newick_phylo(file = dedup_tr)
   expect_true(inherits(phylo_tr, "multiPhylo"))
   expect_equal(phylo_tr[[6]]$tip.label, c("A_1_1", "B__2", "A_1_2", "A_1"))
})


test_that("deduplicate labels works on a OTL study", {
   skip_on_cran()
   expect_warning(get_study_tree(study_id="pg_710", tree_id="tree1277", tip_label='ott_taxon_name'),
                  "Some tip labels were duplicated")
})

unlink(file_dup)
