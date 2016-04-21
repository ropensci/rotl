####
## Making use of the shared OpenTree testing architecture
####
## The R, Python and Ruby wrappers for the Open Tree share a very similar design,
## allowing them to make use of a single test suite for the low-level functions
## (thus, the tests both checkvan individual library works as expected, and that
## the different libraries stay in line with each other).
##
## This file pulls the current version of the test from a github repo
## (https://github.com/OpenTreeOfLife/shared-api-tests) and translates the json
## files into tests that run in testthat. This takes a considerable amount of
## infrastructure so I'll briefly described the rational here.
##
## The JSON test-specificaton is defined at the github repo linked above, to
## translate these tests I have created custom testthat expectation-functionals
## (contains(), (key_has_value()... ). Because many of the test blocks in the
## JSON files have multiple expectiatoins (i.e. many key-value pairs for
## test_equals) there are functions starting with `test_` that run an entire
## test block for a given expectation. Since many of these tests require
## translation between R-objects and JSON encoded strings there is a set of
## convienence functions to automate that step and a function "test_map" that
## returns the appropriate test_* function for r given JSON test block.
##
## Finally, testthat_json_test uses the above functions to runs an entire test
## from a JSON object, and run_shared_tests() runs every tests in a JSON file.




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
        else if(length(value)==1){
            expectation(x[[key]] == value,
                        paste("Key", key, "doesn't have value", value))
        }
        else{
            expectation(all(x[[key]] %in% value),
                        paste("Key", key, "doesn't contain all of", value))
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
    for(i in 1:length(kv_pairs)){
        expect_that(response, key_has_value(kv_pairs[[1]], kv_pairs[[2]]))
    }
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
           "error"       = stop("Error tests should be handled first"),
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
   for(i in 1:length(all_tests)) {
       test_that(all_tests[i], {
           skip_on_cran()
           testthat_json_test(json_obj, all_tests[i])
       })
   }
}


## if (identical(Sys.getenv("NOT_CRAN"), "true")) {
##     base_url <- "https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/"
##     apis <- c("graph_of_life",
##               "studies",
##               "taxonomy",
##               "tree_of_life",
##               "tnrs"
##               )
##     for(i in 1:length(apis)){
##         context( paste(apis[i], "API") )
##         test_text <- httr::GET(paste0(base_url, apis[i], ".json"))
##         test_description <- jsonlite::fromJSON(httr::content(test_text))
##         run_shared_test(test_description)
##     }
## }
