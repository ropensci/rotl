
context("Taxonomy life API")

test_description <-
    jsonlite::fromJSON('https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/taxonomy.json')
test_file(test_description)
