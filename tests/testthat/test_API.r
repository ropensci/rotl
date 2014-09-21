##Making use of the shared OpenTree testing architecture
##
##The R, Python and Ruby wrappers for the Open Tree share a very similar design, 
##allowing them to make use of a single test suite (thus, the tests both check
## an individual library works and that the libraries stay in line). 
##
##This joint testing arhitecture is recorded in a series of JSON files that
## contain a series of tests, each of which contains one of several different 
## expectations. This code makes it possible to translate the JSON tests in to 
## testthat expectations/tests. Each of the low-level APIs is then tested in its
## own file in the 'testthat' folder, which reads the appropriate JSON file. 
##
##NOTE: At present the JSON files are stored locally, it might make more sense
## to always pull them down from the python bindings repo, or manually keep the
## library up to date between releases.




##
##Create custom expectation to map the JSON specificatoi

#functionals that start with a response
contains <- function(key_name){
    function(x){
        expectation(key_name %in% names(x), sprintf("Missing key name: %s", key_name))
    }
}

key_has_value <- function(key, value){
    function(x){
        if(length(value) == 0){
            expectation(length(x[[key]]) == 0, 
                               paste("Key", key, "is not empty"))
        }
        else{
            expectation(x[[key]] == value, 
                        paste("Key", key, "doesn't have value", value))
        }
    }
}

value_is_longer_than <- function(key, len){
    function(x){
        expectation(length(x[[key]]) > len, 
                    paste("Value for key", key, "is shorter than", len))
    }
}

value_is_error <- function(key_name){
    function(x){
        expectation(x[[key_name]] == 'error', 
                       sprintf("Key %s is not 'error'",key_name))
    }
}

## Functions to test entire test blocks with the above expectations

test_contains <- function(response, test_block){
    key_names <- test_block[,1]
    sapply(key_names, function(k) expect_that(response, contains(k)))
}

test_equals <- function(response, test_block){
    kv_pairs <- sapply(test_block, "[[", 1)
    apply(kv_pairs, 2,function(k) 
          expect_that(response, key_has_value( k[[1]], k[[2]] )))
    
}

test_of_type <- function(response, test_block){
    rtype <- type_map(test_block[[1]])
    expect_that(response, is_a(rtype))
}

test_deep_equals <- function(response, test_block){ 
    cat("*")
   expect_true(TRUE) 
}


test_length_greater_than <- function(response, test_block){
    vl_pairs <- sapply(test_block, "[[", 1)
    apply(vl_pairs, 2, function(v)
          expect_that(response, value_is_longer_than(v[[1]], v[[2]])))
}

test_contains_error <- function(response, test_block){
    errs <- test_block[,1]
    sapply(errs, function(e) expect_that(reponse, contains_error(e)))
}

##convience functions 
obj_map <- function(input){
    if(is.character(input) & length(input)==1){
        switch(tolower(input), 
               "true" = TRUE,
               "false" = FALSE,
               "null"  = NULL,
               input)
    }
    else{
        input
    }
}

json_to_r <- function(test_input){
    if(length(test_input) == 0){
       return(test_input)
    }
    return(lapply(test_input, obj_map))
}

type_map <- function(json_type){
    switch(json_type, 
           "dict" = "list",
           stop(sprintf("unknown json type in testing file: %s", json_type))
          )
}


test_map <- function(test_type){
    switch(test_type,
           "contains"    = test_contains,
           "equals"      = test_equals,
           "deep_equals" = test_deep_equals,
           "error"       = stop("Error tests shoul be handled first"),
           "length_greater_than" = test_length_greater_than,
           "of_type"     = test_of_type,
           stop(sprintf("Unkown error type in JSON test: %s", test_type))
           )
}

make_request <- function(json_test){
    test_fxn <- paste0(".", json_test$test_function)
    do.call(what=test_fxn, args=json_to_r(json_test$test_input))

}


testthat_json_test <- function(test_obj, test_name){
    tests_to_run <- names(test_obj[[test_name]]$tests)
    if(length(tests_to_run)==1){
        if( grepl("error", tests_to_run)){
        expect_error( make_request(test_obj[[test_name]]) )
        }
    }
    else{
        response <- make_request(test_obj[[test_name]])
        for(i in 1:length(tests_to_run)){
            test_block <- test_obj[[test_name]]$tests[[ tests_to_run[i] ]]
            test_fxn <- test_map(tests_to_run[i]) 
            test_fxn(response, test_block)
        }
    }
}

run_shared_test <- function(json_obj){
   all_tests <- names(json_obj)
   for(i in 1:length(all_tests)){
       test_that(all_tests[i],
          testthat_json_test(json_obj, all_tests[i])
          )
   }
}


context("Graph of life API")

    test_description <- jsonlite::fromJSON("https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/graph_of_life.json",
                                           unicode=TRUE)
    run_shared_test(test_description)

context("Studies API")
    test_description <- jsonlite::fromJSON("https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/studies.json",
                                          unicode=TRUE)                           
    run_shared_test(test_description)

context("Taxonomy life API")

    test_description <- jsonlite::fromJSON('https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/taxonomy.json',
                                           unicode=TRUE)
    run_shared_test(test_description)

context("Taxonomic name resolution API")

    test_description <- jsonlite::fromJSON("https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/tnrs.json",
                                           unicode=TRUE)
    run_shared_test(test_description)

context("Tree of life API")

    test_description <-jsonlite::fromJSON(httr::content(httr::GET('https://github.com/dwinter/shared-api-tests/raw/master/tree_of_life.json')))
                                     
    run_shared_test(test_description)
