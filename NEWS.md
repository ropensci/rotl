## rotl 0.5.0

* Added arguments `include_lineage` and `list_terminal_descendants` to
  `taxonomy_taxon()`

* Improve warning and format of the result if one of the taxa requested doesn't
  match anything `tnrs_match_names`.

* In the data frame returned by `tnrs_match_names`, the columns
  `approximate_match`, `is_synonym` and `is_deprecated` are now `logical`
  (instead of `character`) [issue #54]

* New utility function `strip_ott_ids` removes OTT id information from
  a character vector, making it easier to match tip labels in trees returned by
  `tol_induced_subtree` to taxonomic names in other data sources.

* New method `list_trees` returns a list of tree ids associated with
  studies. The function takes the output of `studies_find_studies` or
  `studies_find_trees`.

* `studies_find_studies` and `studies_find_trees` gain argument `detailed`
  (default set to `TRUE`), that produces a data frame summarizing information
  (title of the study, year of publication, DOI, ids of associated trees, ...)
  about the studies matching the search criteria.

* `get_study_tree` gains argument `deduplicate`. When `TRUE`, if the tree
  returned for a given study contains duplicated tip labels, they will be made
  unique before being parsed by NCL by appending a suffix (`_1`, `_2`, `_3`,
  etc.). (#46, reported by @bomeara)

* New method `get_study_year` for objects of class `study_meta` that returns the
  year of publication of the study.

* A more robust approach is used by `get_tree_ids` to identify the tree ids in
  the metadata returned by the API

## rotl 0.4.1

* Initial CRAN release on July, 24th 2015
