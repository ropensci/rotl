
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
install_github("fmichonneau/rotl", build_vignette=TRUE)
```

## Vignettes

There are two vignettes:
- start by checking out the "How to use `rotl`?" by typing:
  `vignette("how-to-use-rotl", package="rotl")` after installing the
  package.
- then explore in a little more detail how to combine `rotl` with other packages
  with [this vignette](http://dwinter.github.io/rotl-vignette/)

## Quick start

### Get a little bit of the big Open Tree tree

First find ott ids for a set of names:


```r
library(rotl)
apes <- c("Pan", "Pongo", "Pan", "Gorilla", "Hylobates", "Hoolock", "Homo")
(resolved_names <- tnrs_match_names(apes))
```

```
##   search_string                             unique_name approximate_match
## 1           pan      Pan (genus in subfamily Homininae)             FALSE
## 2         pongo     Pongo (genus in subfamily Ponginae)             FALSE
## 3           pan      Pan (genus in subfamily Homininae)             FALSE
## 4       gorilla                                 Gorilla             FALSE
## 5     hylobates Hylobates (genus in family Hylobatidae)             FALSE
## 6       hoolock                                 Hoolock             FALSE
## 7          homo                                    Homo             FALSE
##   ott_id number_matches is_synonym is_deprecated
## 1 417957              2      FALSE         FALSE
## 2 417949              2      FALSE         FALSE
## 3 417957              2      FALSE         FALSE
## 4 417969              3      FALSE         FALSE
## 5 166552              1      FALSE         FALSE
## 6 712902              1      FALSE         FALSE
## 7 770309              1      FALSE         FALSE
```

Now get the tree with just those tips:


```r
tr <- tol_induced_subtree(ott_ids=resolved_names$ott_id)
plot(tr)
```

![plot of chunk get_tr](http://i.imgur.com/KOSzti4.png) 

### Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you
agree to abide by its terms.
