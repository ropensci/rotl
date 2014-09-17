##Functions to convet tests defined in json files to a testthat test
##

input_to_R <- function(x){
    if(substring(mrca_test$test_input,1,1) == "["){
        #input is string-encoded list, make into a vector
        return(strsplit(gsub("\\[(.*?)\\]", "\\1", x), ", ")[[1]])
    }
    return(x)
}

type_map <- function(json_type){
    switch(json_type, 
           "dict" = "list",
           )
}

        
