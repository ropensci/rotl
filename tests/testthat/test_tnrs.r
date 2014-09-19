
context("Taxonomic name resolution API")

test_description <- jsonlite::fromJSON("https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/tnrs.json")
test_file(test_description)
