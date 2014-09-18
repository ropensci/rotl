## Test graph of life wrapping functions work
context("gol")

    test_description <- fromJSON(fetch_json("gol.json"))
    test_names <- names(test_description)
    vapply(test_names, function(x) build_tests(test_description[x],x))
