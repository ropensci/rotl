
[![Build Status](https://travis-ci.org/ropensci/rotl.svg?branch=master)](https://travis-ci.org/ropensci/rotl)
[![Build status](https://ci.appveyor.com/api/projects/status/5y8rxmehag512d9j?svg=true)](https://ci.appveyor.com/project/fmichonneau/rotl-i7l3q)
[![Coverage Status](https://coveralls.io/repos/ropensci/rotl/badge.svg?branch=master&service=github)](https://coveralls.io/github/ropensci/rotl?branch=master)
[![](http://www.r-pkg.org/badges/version/rotl)](http://www.r-pkg.org/pkg/rotl)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/rotl)](http://www.r-pkg.org/pkg/rotl)
[![Research software impact](http://depsy.org/api/package/cran/rotl/badge.svg)](http://depsy.org/package/r/rotl)

# An R interface to Open Tree API

`rotl` is an R package to interact with the Open Tree of Life data APIs. It was
initially developed as part of the
[NESCENT/OpenTree/Arbor hackathon](http://blog.opentreeoflife.org/2014/06/11/apply-for-tree-for-all-a-hackathon-to-access-opentree-resources/).

Client libraries to interact with the Open Tree of Life API also exists for
[Python](https://github.com/OpenTreeOfLife/pyopentree)
and [Ruby](https://github.com/SpeciesFileGroup/bark).


## Installation

The current stable version is available from CRAN, and can be installed by
typing the following at the prompt in R:


```r
install.packages("rotl")
```

If you want to test the development version, you first need to install
[devtools](https://github.com/hadley/devtools).


```r
install.packages("devtools")
```

Then you can install `rotl` using:


```r
library(devtools)
install_github("ropensci/rotl", dependencies = TRUE, build_vignette=TRUE)
```

To build the vignettes with the development version, you will need to have
pandoc 1.12.3 or higher installed. If you use
[RStudio](https://www.rstudio.com/products/rstudio/download/), make sure you
have v0.99 (as it comes with pandoc 1.13.1); if you don't use RStudio, you can
find pandoc for your operating system [here](http://pandoc.org/installing.html)

## Vignettes

There are two vignettes:

- start by checking out the "How to use `rotl`?" by typing:
  `vignette("how-to-use-rotl", package="rotl")` after installing the
  package.

- then explore how you can use `rotl` with other packages to combine your data
  with trees from the Open Tree of Life project by typing:
  `vignette("data_mashups", package="rotl")`.

The vignettes are also available from CRAN:
[How to use `rotl`?](https://cran.r-project.org/web/packages/rotl/vignettes/how-to-use-rotl.html)
and [Data mashups](https://cran.r-project.org/web/packages/rotl/vignettes/data_mashups.html)

## Quick start

### Get a little bit of the big Open Tree tree

Taxonomic names are represented in the Open Tree by numeric identifiers, the
`ott_ids` (Open Tree Taxonomy identifiers). To extract a portion of a tree from
the Open Tree, you first need to find `ott_ids` for a set of names using the
`tnrs_match_names` function:


```r
library(rotl)
apes <- c("Pan", "Pongo", "Pan", "Gorilla", "Hylobates", "Hoolock", "Homo")
(resolved_names <- tnrs_match_names(apes))
```

```
##   search_string                      unique_name approximate_match  ott_id
## 1           pan                              Pan             FALSE  417957
## 2         pongo                            Pongo             FALSE  417949
## 3           pan                              Pan             FALSE  417957
## 4       gorilla                          Gorilla             FALSE  417969
## 5     hylobates Hylobates (genus in Protostomia)             FALSE 5333940
## 6       hoolock                          Hoolock             FALSE  712902
## 7          homo                             Homo             FALSE  770309
##   is_synonym is_deprecated number_matches
## 1      FALSE         FALSE              1
## 2      FALSE         FALSE              1
## 3      FALSE         FALSE              1
## 4      FALSE         FALSE              1
## 5      FALSE         FALSE              2
## 6      FALSE         FALSE              1
## 7      FALSE         FALSE              1
```

Now get the tree with just those tips:


```r
tr <- tol_induced_subtree(ott_ids=resolved_names$ott_id)
```

```
## Warning in tol_induced_subtree(ott_ids = resolved_names$ott_id): ott ids:
## 5333940 not in graph.
```

```r
plot(tr)
```

![plot of chunk get_tr](http://i.imgur.com/wUcpWFD.png)

### Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you
agree to abide by its terms.

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
