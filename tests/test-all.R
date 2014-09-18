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
        expectation(x[key] == value, paste("Key", key, "doesn't have value", value))
    }
}

value_is_longer_than <- function(key, value, len){
    function(x){
        expectation(length(x[key]) > len, 
                    paste("Value for key", key, "is shorter than", len))
    }
}

value_is_error <- function(key_name){
    function(x){
        expectation(x$key_name == 'error', 
                       sprintf("Key %s is not 'error'",key_name)
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
    expect_that(response, is_a(test_block[[1]]))
}

test_deep_equals <- function(response, test_block) #stub {
}


test_length_greater_than <- function(response, test_block){
    vl_pairs <- sapply(test_block, "[[", 1)
    apply(vl_pairs, 2, function(v)
          expect_that(response, value_is_longer_than(vl[[1]], vl[[2]]))
}

test_contains_error <- function(response, test_block){
    errs <- test_block[,1]
    sapply(errs, function(e) expect_that(reponse, contains_error(e)))
}



make_request <- function(json_test){
    do.call(what=json_test$test_function, args=json_test$test_input)
}




expect_map <- function(test_type){
    switch(test_type,
           "contains"    = test_contains,
           "equals"      = test_equals
           "deep_equals" = stop("Not there yet!"),
           "error"       = stop("Error tests shoul be handled first")
           "length_greater_than" = test_length_greater_than,
           "of_type"     = test_of_type,
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

