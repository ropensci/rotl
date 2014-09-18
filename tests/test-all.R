## To run the low-level tests we need to parse out the json

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

