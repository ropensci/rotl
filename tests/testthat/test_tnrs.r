
context("tnrs")

test_description <- fromJSON(fetch_json("tnrs.json"))

test_names <- names(test_description)

vapply(test_names, function(x) build_tests(test_description[x],x))
