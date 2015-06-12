
[![Build Status](https://travis-ci.org/fmichonneau/rotl.svg)](https://travis-ci.org/fmichonneau/rotl)
[![Coverage Status](https://coveralls.io/repos/fmichonneau/rotl/badge.svg)](https://coveralls.io/r/fmichonneau/rotl)

# An R interface to Open Tree API

`rotl` is an R package to interact with the Open Tree of Life data APIs. It was
initially developed as part of the
[NESCENT/OpenTree/Arbor hackathon](http://blog.opentreeoflife.org/2014/06/11/apply-for-tree-for-all-a-hackathon-to-access-opentree-resources/).

Client libraries to interact with the Open Tree of Life API also exists for
[Python](https://github.com/OpenTreeOfLife/opentree-interfaces/tree/master/python)
and [Ruby](https://github.com/SpeciesFileGroup/bark).


## Installation

If you want to install this package, you first need to install
[devtools](https://github.com/hadley/devtools), as `rotl` is not yet available
on CRAN.

`rotl` uses [rncl](https://github.com/fmichonneau/rncl) to parse trees, so you
will also need to install that package.


```r
install.packages(c("devtools", "rncl"))
```

Then you can intall `rotl` using:


```r
library(devtools)
install_github("fmichonneau/rotl")
```

## Vignettes

There are two vignettes:
- start by checking out the "How to use `rotl`?" by typing:
  `vignette("how-to-use-rotl", package="rotl")` after installing the package.
- explore in a little more detail how to use `rotl` with other packages with
  [this vignette](http://dwinter.github.io/rotl-vignette/)

## Simple examples

Note: the library is still in active development and behaviour of the following
functions may well change in the future:

### Get a little bit of the big Open Tree tree

First find ott ids for a set of names:


```r
library(rotl)
```

```
## Warning in get(Info[i, 1], envir = env): internal error -3 in R_decompress1
```

```
## Error: package or namespace load failed for 'rotl'
```

```r
apes <- c("Pan", "Pongo", "Pan", "Gorilla", "Hylobates", "Hoolock", "Homo")
(resolved_names <- tnrs_match_names(apes))
```

```
## Error in eval(expr, envir, enclos): could not find function "tnrs_match_names"
```
Now get open tree to return a tree with just those tips.



```r
tr <- tol_induced_subtree(ott_ids=resolved_names$ott_id)
```

```
## Error in eval(expr, envir, enclos): could not find function "tol_induced_subtree"
```

```r
plot(tr)
```

![plot of chunk get_tr](http://i.imgur.com/7HQMAJ0.png) 


### Find trees focused on my favourite taxa


```r
furry_studies <- studies_find_studies(property="ot:focalCladeOTTTaxonName", value="Mammalia")
```

```
## Error in eval(expr, envir, enclos): could not find function "studies_find_studies"
```

```r
( furry_ids <- unlist(furry_studies$matched_studies) )
```

```
## ot:studyId 
##  "pg_2550"
```

### Get a specific study tree

```r
library(ape)
furry_metadata <-get_study_meta(2647)
```

```
## Error in eval(expr, envir, enclos): could not find function "get_study_meta"
```

```r
furry_metadata$nexml$treesById
```

```
## $trees2647
## $trees2647$treeById
## $trees2647$treeById$tree6170
## NULL
## 
## $trees2647$treeById$tree6169
## NULL
## 
## 
## $trees2647$`^ot:treeElementOrder`
## $trees2647$`^ot:treeElementOrder`[[1]]
## [1] "tree6169"
## 
## $trees2647$`^ot:treeElementOrder`[[2]]
## [1] "tree6170"
## 
## 
## $trees2647$`@otus`
## [1] "otus2647"
```

```r
furry_tr <- get_study_tree(study_id="2647", tree_id="tree6169")
```

```
## Error in eval(expr, envir, enclos): could not find function "get_study_tree"
```

```r
plot(furry_tr)
```

![plot of chunk tree](http://i.imgur.com/dUYwv5z.png) 

### Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you
agree to abide by its terms.
