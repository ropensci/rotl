context("Study external data")
all_sources <- c("doi", "pubmed_id", "external_data_url", "popset_ids", "nucleotide_ids")
all_data <- study_external_IDs("pg_1940")


test_that("We can recover dois, pmids, NCBI IDs", { 
    expect_that(all_data, is_a("study_external_data"))
    expect_named(all_data)
})

test_that("We can handle studies with missing external IDs", {    
   expect_warning(
        missing_data <- study_external_IDs("ot_97"), "skipping NCBI"
    )
    expect_named(missing_data)
    expect_that(missing_data, is_a("study_external_data"))
    expect_equal( sum(is.na(match(all_sources, names(missing_data)))), 2) #we really skipped the NCBI        
})

test_that("The print functions for external data objects work", {
    missing_data <- study_external_IDs("ot_91")
    expect_output(print(all_data), "External data identifiers for study")
    expect_output(print(missing_data), "External data identifiers for study")
})
    

context("Taxon external data")

test_that("We can recover external IDs for Open Tree taxa", {
    gibbon_IDs <- taxon_external_IDs(712902)
    expect_that(gibbon_IDs, is_a("data.frame"))
    expect_equal(names(gibbon_IDs), c("source", "id"))
})

