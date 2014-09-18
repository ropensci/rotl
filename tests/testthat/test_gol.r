
context("Graph of life API")

test_description <-
    jsonlite::fromJSON("https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/graph_of_life.json")
test_file(test_description)
