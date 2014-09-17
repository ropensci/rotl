## To run the low-level tests we need to parse out the json
contains <- function(name){
    function(x) {
        expectation(name %in% names(x), 
                    paste("name '", name, "' not found", sep=""))
    }
}


json_contains <- function(response, test_block){
    apply(test_block, 1, function(r)
                    expectation( r[[1]] %in% names(response), r[[2]]) )
}

json_of_type <- function(response, test_block){
    expect_is(response, type_map(test_block[1]))
}

json_equals <- function(response, test_block){
    expect
}







#JSON equals tests
json_equals <- function(response, row){
    apply(
    resonse[row[1]]


##Map JSON object types to R-equivalent
type_map <- function(json_type){
    switch(json_type, 
           "dict" = "list",
           stop(sprintf("unknown json type in testing file: %s", 
                        json_type))
           )
}

##Map test-types to testthat expectations
test_map <- function(test_type){
    switch(test_type,
           "of_type" = is_a,
           "equals"  = is_equal
           "contains" = contains
           "error" =  throws_error,
           stop())
}

run_expectations <- function(json_test, response, test_type)
    expectations <- json_test[test_type]
    if(length(expectations) == 1){
        expect_that(response, test_map(test_type)(json_test[test_type][1]))
    }
    else{
        sapply(test_ma
    }
}



build_test <- function(json_test){
    response <- httr::content(do.call(json_test$test_function,
                                      args=json_test$test_input))
    tests_to_run <- names(json_test$tests)
    sapply(tests_to_run, function(t){
           t


}

   
