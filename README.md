
<!-- badges: start -->

[![R build
status](https://github.com/ropensci/rotl/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/rotl/actions)
[![codecov.io](https://codecov.io/github/ropensci/rotl/coverage.svg?branch=master)](https://codecov.io/github/ropensci/rotl?branch=master)
[![](https://www.r-pkg.org/badges/version/rotl)](https://www.r-pkg.org/pkg/rotl)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/rotl)](https://www.r-pkg.org/pkg/rotl)
[![](https://badges.ropensci.org/17_status.svg)](https://github.com/ropensci/software-review/issues/17)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

# An R interface to Open Tree API

`rotl` is an R package to interact with the Open Tree of Life data APIs.
It was initially developed as part of the [NESCENT/OpenTree/Arbor
hackathon](https://blog.opentreeoflife.org/2014/06/11/apply-for-tree-for-all-a-hackathon-to-access-opentree-resources/).

Client libraries to interact with the Open Tree of Life API also exists
for [Python](https://github.com/OpenTreeOfLife/pyopentree) and
[Ruby](https://github.com/SpeciesFileGroup/bark).

## Installation

The current stable version is available from CRAN, and can be installed
by typing the following at the prompt in R:

``` r
install.packages("rotl")
```

If you want to test the development version, you first need to install
the `remotes` package.

``` r
install.packages("remotes")
```

Then you can install `rotl` using:

``` r
remotes::install_github("ropensci/rotl")
```

## Vignettes

There are three vignettes:

  - Start by checking out the “How to use `rotl`?” by typing:
    `vignette("rotl", package="rotl")` after installing the package.

  - Then explore how you can use `rotl` with other packages to combine
    your data with trees from the Open Tree of Life project by typing:
    `vignette("data_mashups", package="rotl")`.

  - The vignette “Using the Open Tree Synthesis in a comparative
    analsysis” demonstrates how you can reproduce an analysis of a
    published paper by downloading the tree they used, and data from the
    supplementary material: `vignette("meta-analysis", package="rotl")`.

The vignettes are also available from CRAN: [How to use
`rotl`?](https://cran.r-project.org/package=rotl/vignettes/rotl.html),
[Data
mashups](https://cran.r-project.org/package=rotl/vignettes/data_mashups.html),
and [Using the Open Tree synthesis in a comparative
analysis](https://cran.r-project.org/package=rotl/vignettes/meta-analysis.html).

## Quick start

### Get a little bit of the big Open Tree tree

Taxonomic names are represented in the Open Tree by numeric identifiers,
the `ott_ids` (Open Tree Taxonomy identifiers). To extract a portion of
a tree from the Open Tree, you first need to find `ott_ids` for a set of
names using the `tnrs_match_names` function:

``` r
library(rotl)
apes <- c("Pongo", "Pan", "Gorilla", "Hoolock", "Homo")
(resolved_names <- tnrs_match_names(apes))
```

    ##   search_string unique_name approximate_match ott_id is_synonym          flags
    ## 1         pongo       Pongo             FALSE 417949      FALSE               
    ## 2           pan         Pan             FALSE 417957      FALSE sibling_higher
    ## 3       gorilla     Gorilla             FALSE 417969      FALSE sibling_higher
    ## 4       hoolock     Hoolock             FALSE 712902      FALSE               
    ## 5          homo        Homo             FALSE 770309      FALSE sibling_higher
    ##   number_matches
    ## 1              2
    ## 2              1
    ## 3              1
    ## 4              1
    ## 5              1

Now we can get the tree with just those tips:

``` r
tr <- tol_induced_subtree(ott_ids = ott_id(resolved_names))
```

    ## Warning in collapse_singles(tr, show_progress): Dropping singleton nodes with
    ## labels: mrcaott83926ott6145147, mrcaott83926ott3607728, mrcaott83926ott3607876,
    ## mrcaott83926ott3607873, mrcaott83926ott3607687, mrcaott83926ott3607716,
    ## mrcaott83926ott3607689, mrcaott83926ott3607732, mrcaott770295ott3607719,
    ## mrcaott770295ott3607692, Ponginae ott1082538, Hylobatidae ott166544

``` r
plot(tr)
```

![](https://i.imgur.com/5Fdb927.png)<!-- -->

The code above can be summarized in a single pipe:

``` r
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following objects are masked from 'package:testthat':
    ## 
    ##     equals, is_less_than, not

``` r
## or expressed as a pipe:
c("Pongo", "Pan", "Gorilla", "Hoolock", "Homo") %>%
  tnrs_match_names() %>%
  ott_id() %>%
  tol_induced_subtree() %>%
  plot()
```

    ## Warning in collapse_singles(tr, show_progress): Dropping singleton nodes with
    ## labels: mrcaott83926ott6145147, mrcaott83926ott3607728, mrcaott83926ott3607876,
    ## mrcaott83926ott3607873, mrcaott83926ott3607687, mrcaott83926ott3607716,
    ## mrcaott83926ott3607689, mrcaott83926ott3607732, mrcaott770295ott3607719,
    ## mrcaott770295ott3607692, Ponginae ott1082538, Hylobatidae ott166544

![](https://i.imgur.com/43LgNKf.png)<!-- -->

## Citation and Manuscript

To cite `rotl` in publications pleases use:

> Michonneau, F., Brown, J. W. and Winter, D. J. (2016), rotl: an R
> package to interact with the Open Tree of Life data. Methods in
> Ecology and Evolution. 7(12):1476-1481. doi:
> [10.1111/2041-210X.12593](https://doi.org/10.1111/2041-210X.12593)

You may also want to cite the paper for the Open Tree of Life

> Hinchliff, C. E., et al. (2015). Synthesis of phylogeny and taxonomy
> into a comprehensive tree of life. Proceedings of the National Academy
> of Sciences 112.41 (2015): 12764-12769 doi:
> [10.1073/pnas.1423041112](https://doi.org/10.1073/pnas.1423041112)

The manuscript in *Methods in Ecology and Evolution* includes additional
examples on how to use the package. The manuscript and the code it
contains are also hosted on GitHub at:
<https://github.com/fmichonneau/rotl-ms>

## Versioning

Starting with v3.0.0 of the package, the major and minor version numbers
(the first 2 digits of the version number) will be matched to those of
the API. The patch number (the 3rd digit of the version number) will be
used to reflect bug fixes and other changes that are independent from
changes to the API.

`rotl` can be used to access other versions of the API (if they are
available) but most likely the high level functions will not work.
Instead, you will need to parse the output yourself using the “raw”
returns from the unexported low-level functions (all prefixed with a
`.`). For instance to use the `tnrs/match_names` endpoint for `v2` of
the API:

``` r
rotl:::.tnrs_match_names(c("pan", "pango", "gorilla", "hoolock", "homo"), otl_v = "v2")
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/ropensci/rotl/blob/master/CONDUCT.md). By
participating in this project you agree to abide by its terms.

[![](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
