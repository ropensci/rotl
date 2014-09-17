## To run the low-level tests we need to parse out the json
contains <- function(name){
    function(x) {
        expectation(name %in% names(x), 
                    paste("name '", name, "' not found", sep=""))
    }
}


##Map JSON object types to R-equivalent
type_map <- function(json_type){
    switch(json_type, 
           "dict" = "list",
           stop()
           )
}

##Map test-types to testthat expectations
test_map <- function(test_type){
    switch(test_type,
           "of_type" = is_a,
           "equals"  = is_true,
           "contains" = contains,
           "error" =  throws_error,
           stop())
}

json_expectation <- function(test){

build_a_test <- function(json_test){
    response <- httr::content(do.call(json_test$test_function,
                                      args=json_test$test_input))


}

   
