## rotl 0.4.1.999

* `get_study_tree` gains argument `deduplicate`. When `TRUE`, if the tree
  returned for a given study contains duplicated tip labels, they will be made
  unique before being parsed by NCL by appending a suffix (`_1`, `_2`, `_3`,
  etc.). (#46, reported by @bomeara)

* New method `get_study_year` for objects of class `study_meta` that returns the
  year of publication of the study.

## rotl 0.4.1

* Initial CRAN release on July, 24th 2015
