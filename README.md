
[![Build Status](https://travis-ci.org/ropensci/rotl.svg?branch=master)](https://travis-ci.org/ropensci/rotl) [![Build status](https://ci.appveyor.com/api/projects/status/qr4k9a8wlrjl65rp?svg=true)](https://ci.appveyor.com/project/ropensci/rotl) [![codecov.io](https://codecov.io/github/ropensci/rotl/coverage.svg?branch=master)](https://codecov.io/github/ropensci/rotl?branch=master) [![](http://www.r-pkg.org/badges/version/rotl)](http://www.r-pkg.org/pkg/rotl) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/rotl)](http://www.r-pkg.org/pkg/rotl) [![Research software impact](http://depsy.org/api/package/cran/rotl/badge.svg)](http://depsy.org/package/r/rotl)
[![](https://badges.ropensci.org/17_status.svg)](https://github.com/ropensci/onboarding/issues/17)

An R interface to Open Tree API
===============================

`rotl` is an R package to interact with the Open Tree of Life data APIs. It was initially developed as part of the [NESCENT/OpenTree/Arbor hackathon](http://blog.opentreeoflife.org/2014/06/11/apply-for-tree-for-all-a-hackathon-to-access-opentree-resources/).

Client libraries to interact with the Open Tree of Life API also exists for [Python](https://github.com/OpenTreeOfLife/pyopentree) and [Ruby](https://github.com/SpeciesFileGroup/bark).

Installation
------------

The current stable version is available from CRAN, and can be installed by typing the following at the prompt in R:

``` r
install.packages("rotl")
```

If you want to test the development version, you first need to install [ghit](https://github.com/cloudyr/ghit) (`ghit` is a more lightweight version of [devtools](https://github.com/hadley/devtools) if your sole purpose is to install packages that are hosted on GitHub).

``` r
install.packages("ghit")
```

Then you can install `rotl` using:

``` r
library(ghit) # or library(devtools)
install_github("ropensci/rotl")
```

Vignettes
---------

There are three vignettes:

-   Start by checking out the "How to use `rotl`?" by typing: `vignette("how-to-use-rotl", package="rotl")` after installing the package.

-   Then explore how you can use `rotl` with other packages to combine your data with trees from the Open Tree of Life project by typing: `vignette("data_mashups", package="rotl")`.

-   The vignette "Using the Open Tree Synthesis in a comparative analsysis" demonstrates how you can reproduce an analysis of a published paper by downloading the tree they used, and data from the supplementary material: `vignette("meta-analysis", package="rotl")`.

The vignettes are also available from CRAN: [How to use `rotl`?](https://cran.r-project.org/package=rotl/vignettes/how-to-use-rotl.html), [Data mashups](https://cran.r-project.org/package=rotl/vignettes/data_mashups.html), and [Using the Open Tree synthesis in a comparative analysis](https://cran.r-project.org/package=rotl/vignettes/meta-analysis.html).

Quick start
-----------

### Get a little bit of the big Open Tree tree

Taxonomic names are represented in the Open Tree by numeric identifiers, the `ott_ids` (Open Tree Taxonomy identifiers). To extract a portion of a tree from the Open Tree, you first need to find `ott_ids` for a set of names using the `tnrs_match_names` function:

``` r
library(rotl)
apes <- c("Pongo", "Pan", "Gorilla", "Hoolock", "Homo")
(resolved_names <- tnrs_match_names(apes))
```

    ##   search_string unique_name approximate_match ott_id is_synonym flags
    ## 1         pongo       Pongo             FALSE 417949      FALSE      
    ## 2           pan         Pan             FALSE 417957      FALSE      
    ## 3       gorilla     Gorilla             FALSE 417969      FALSE      
    ## 4       hoolock     Hoolock             FALSE 712902      FALSE      
    ## 5          homo        Homo             FALSE 770309      FALSE      
    ##   number_matches
    ## 1              2
    ## 2              2
    ## 3              1
    ## 4              1
    ## 5              1

Now we can get the tree with just those tips:

``` r
tr <- tol_induced_subtree(ott_ids=ott_id(resolved_names))
plot(tr)
```

![](http://i.imgur.com/hfR1DRi.png)

The code above can be summarized in a single pipe:

``` r
library(magrittr)
## or expressed as a pipe:
c("Pongo", "Pan", "Gorilla", "Hoolock", "Homo") %>%
    tnrs_match_names %>%
    ott_id %>%
    tol_induced_subtree %>%
    plot
```

![](http://i.imgur.com/TM9nzPI.png)

Citation and Manuscript
-----------------------

To cite `rotl` in publications pleases use:

> Michonneau, F., Brown, J. W. and Winter, D. J. (2016), rotl: an R package to interact with the Open Tree of Life data. Methods in Ecology and Evolution. 7(12):1476-1481. doi: [10.1111/2041-210X.12593](https://doi.org/10.1111/2041-210X.12593)

You may also want to cite the paper for the Open Tree of Life

> Hinchliff, C. E., et al. (2015). Synthesis of phylogeny and taxonomy into a comprehensive tree of life. Proceedings of the National Academy of Sciences 112.41 (2015): 12764-12769 doi: [10.1073/pnas.1423041112](https://doi.org/10.1073/pnas.1423041112)

The manuscript in *Methods in Ecology and Evolution* includes additional examples on how to use the package. The manuscript and the code it contains are also hosted on GitHub at: <https://github.com/fmichonneau/rotl-ms>

Versioning
----------

Starting with v3.0.0 of the package, the major and minor version numbers (the first 2 digits of the version number) will be matched to those of the API. The patch number (the 3rd digit of the version number) will be used to reflect bug fixes and other changes that are independent from changes to the API.

`rotl` can be used to access other versions of the API (if they are available) but most likely the high level functions will not work. Instead, you will need to parse the output yourself using the "raw" returns from the unexported low-level functions (all prefixed with a `.`). For instance to use the `tnrs/match_names` endpoint for `v2` of the API:

``` r
rotl:::.tnrs_match_names(c("pan", "pango", "gorilla", "hoolock", "homo"), otl_v="v2")
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
