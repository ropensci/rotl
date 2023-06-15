# rotl 3.1.0

## Breaking change

* `tnrs_match_names` now returns the taxon with the highest matching score
   instead of the one with the lowest OTT id (issues #127 and #143 reported by
   @ajrominger).

# rotl 3.0.14

* Make sure that the package fails gracefully when there is no
  internet connection.
* The vignettes are now pre-computed.

# rotl 3.0.12

* The default argument `context_name` for the function `tnrs_match_names` was
  changed from `NULL` to `All life`. This changes is made to address what could
  lead to unexpected results. Previously, the context was inferred based on the
  first match when several names were provided (see #134 reported by @LunaSare,
  and https://github.com/OpenTreeOfLife/feedback/issues/528)/
* The "Suggest" dependency `fulltext` was removed following its archival from
  CRAN.

# rotl 3.0.11

## Other changes

* When none of the names provided to `tnrs_match_names` had a match in the Open
  Tree of Life, an error was thrown and nothing was returned. To make the
  behavior of the function more consistent with other situations, when none of
  the names provided have a match, a tibble is returned and a warning is issued.

## Bug Fix

* When attempting to match a name that did not exist in the tree, an error was
  thrown (bug #128, PR #129, @daijiang)

# rotl 3.0.10

* Small fixes following updates to the Open Tree of Life API (no visible change
  for users).
* Updated documentation to reflect new value in output of `tol_node_info()`.

# rotl 3.0.9

* Small fixes following updates to the Open Tree of Life API.

# rotl 3.0.7

* Minor update to vignette to address change to TNRS endpoint (underscores can't
  be included in the taxon names anymore).

# rotl 3.0.6

* Minor update to address warnings seen on CRAN.

# rotl 3.0.5

## New features

* The data types in the data frame returned by `tnrs_match_names` are
  consistent, and remain the same even after using `update()`.
  
## Other changes

* Small internal changes that reflect changes in the data structures returned by
  the API.

# rotl 3.0.4

## New features

* To improve stability of results across releases of the Open Tree Taxonomy, the
  TNRS match with the lowest OTT id is returned instead of the first one in case
  a name is shared across multiple domains (related to #88)
* A warning is issued when users attempt to use TNRS on duplicated names.

## Other changes

* Fix typos and workaround broken API to retrieve supplementary materials in
  vignette.

# rotl 3.0.3

## New features

* The function `get_study_subtree` gains the argument `tip_label` to control the
  formatting of the tip labels, #90, reported by @bomeara
* The new function `is_in_tree` takes a list of OTT ids (i.e., the output of
  `ott_id()`), and returns a vector of logical indicating whether they are
  included in the synthetic tree (workaround #31).

## Bug fixes

* The function `get_study_subtree` ignored the argument `subtree_id`, #89
  reported by @bomeara

## Other chaanges

* `citation("rotl")` now includes the reference to the Open Tree of Life
  publication.
* The "How to use rotl?" vignette was updated to document the behavior of v3 of
  the OTL API which returns an HTTP error code 400 when the request for induced
  subtree includes taxa that are not in the synthetic tree (fix #84)

# rotl 3.0.1

* Fix tests and vignette to reflect changes accompanying release 6.1 of the
  synthetic tree

* Add section in vignette "How to use rotl?" about how to get the higher
  taxonomy from a given taxon.

* Add `CITATION` file with MEE manuscript information (#82)

# rotl 3.0.0

* `rotl` now interacts with v3.0 of the Open Tree of Life APIs. The
  documentation has been updated to reflect the associated changes. More
  information about the v3.0 of the Open Tree of Life APIs can be found
  [on their wiki](https://github.com/OpenTreeOfLife/germinator/wiki/Open-Tree-of-Life-Web-APIs).


## New features

* New methods: `tax_sources`, `is_suppressed`, `tax_rank`, `unique_name`,
  `name`, `ott_id`, for objects returned by `tnrs_match_names()`,
  `taxonomy_taxon_info()`, `taxonomy_taxon_mrca()`, `tol_node_info()`,
  `tol_about()`, and `tol_mrca()`. Each of these methods have their own class.

* New method `tax_lineage()` to extract the higher taxonomy from an object
  returned by `taxonomy_taxon_info()` (initially suggested by Matt Pennell, #57).

* New method `tol_lineage()` to extract the nodes towards the root of the tree.

* New print methods for `tol_node_info()` and `tol_mrca()`.

* New functions `study_external_IDs()` and `taxon_external_IDs()` that return
  the external identifiers for a study and associated trees (e.g., DOI, TreeBase
  ID); and the identifiers of taxon names in taxonomic databases. The vignette
  "Data mashup" includes an example on how to use it.

* The function `strip_ott_id()` gains the argument `remove_underscores` to remove
  underscores from tips in trees returned by OTL.

## Changes

* Rename method `ott_taxon_name()` to `tax_name()` for consistency.

* Rename method `synth_sources()` and `study_list()` to `source_list()`.

* Refactor how result of query is checked and parsed (invisible to the user).

## Bug fixes

* Fix bug in `studies_find_studies()`, the arguments `verbose` and `exact` were
  ignored.

* The argument `only_current` has been dropped for the methods associated with
  objects returned by `tnrs_match_names()`

* The print method for `tnrs_context()` duplicated some names.

* `inspect()`, `update()` and `synonyms()` methods for `tnrs_match_names()` did
  not work if the query included unmatched taxa.


# rotl 0.5.0

* New vignette: `meta-analysis`

* Added arguments `include_lineage` and `list_terminal_descendants` to
  `taxonomy_taxon()`

* Improve warning and format of the result if one of the taxa requested doesn't
  match anything `tnrs_match_names`.

* In the data frame returned by `tnrs_match_names`, the columns
  `approximate_match`, `is_synonym` and `is_deprecated` are now `logical`
  (instead of `character`) [issue #54]

* New utility function `strip_ott_ids` removes OTT id information from
  a character vector, making it easier to match tip labels in trees returned by
  `tol_induced_subtree` to taxonomic names in other data sources. This function
  can also remove underscores from the taxon names.

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

# rotl 0.4.1

* Initial CRAN release on July, 24th 2015
