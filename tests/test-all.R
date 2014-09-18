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


##Map JSON object types to R-equivalent
type_map <- function(json_type){
    switch(json_type, 
           "dict" = "list",
           stop(sprintf("unknown json type in testing file: %s", json_type))
          )
}


run_expectations <- function(test_type, response, test_obj){
    test_block <- test_obj[test_type]
    switch(test_type, 
     "of_type" = expect_is(response, type_map(test_block[1])),
     "equals"  = expect_equal(response[ test_block[1] ], is_equal(test_block[2])),
     "contains" = apply(test_block, 1, function(r) expectation( r[[1]] %in% names(response),
                                                                fail_msg=r[[2]]) ),
     stop(sprintf("unkown test-type in json file: ", test_type))
     )
}


build_test <- function(json_test, desc){
    tests_to_run <- names(json_test$tests)
    if(identical("error", tests_to_run)){
        test_that(desc,
            expect_error(do.call(json_test$test_function, 
                                 args=json_test$test_input)))
    }
    else{
        response <- do.call(json_test$test_function, args=json_test$test_input)
        test_that(desc, 
            sapply(tests_to_run, run_expectations, response, json_test$tests)
        )
    }
}

