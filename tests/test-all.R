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
##Create custom expectation to map the JSON specificatoin
## In each case they take a complete test block, set up the requirement to run
## test. They should be run with response as the first value in expect_that.
##
## expect_that(response, contains(test_block))

#functionals that start with a response
contains_key <- function(key_name){
    function(x){
        expectation(key_name %in% names(x), sprintf("Missing key name: %s", key_name))
    }
}

key_has_value <- function(key, value){
    function(x){
        expectation(x[key] == value, paste("Key", key, "doesn't have value", value))
    }
}

value_is_longer_than <- function(key, value, len){
    function(x){
        expectation(length(x[key]) > len, 
                    paste("Value for key", key, "is shorter than", len))
    }
}

make_request <- function(json_test){
    do.call(what=json_test$test_function, args=json_test$test_input)
}

expect_map <- function(test_type){
    switch(test_type,
           "contains"    = contains,
           "equals"      = key_has_value,
           "deep_equals" = stop("Not there yet!"),
           "error"       = stop("Error tests shoul be handled first")
           "length_greater_than" = value_is_longer_than,
           "of_type"     = is_a
           stop(sprintf("Unkown error type in JSON test: %s", test_type))
           )
}
}

#functionals that start with a complete test block

test_that_json_test <- function(test_obj, test_name){
    tests_to_run <- names(test_obj[test_name][tests])
    
    if(identical(tests_to_run, "error")){
        test_that(test_name, expect_error(make_request(test_obj[test_name])))
    }
    else{
        test_that(test_name,
        response <- make_request(test_obj[test_name])
        for(i in 1:length(tests_to_run)){
            expect_fxn <- expect_map(tests_to_run[i])


           


        }
    }
}






##Map JSON object types to R-equivalent
test_of_type <- function(response, test_obj){
    rtype <- test_obj[
    expect_is(response, type_map[test_type]





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

