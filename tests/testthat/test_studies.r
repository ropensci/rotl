
context("Studies API")

test_description <- jsonlite::fromJSON("https://raw.githubusercontent.com/OpenTreeOfLife/shared-api-tests/master/studies.json")

test_file(test_description)
