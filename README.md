[![Build Status](https://travis-ci.org/fmichonneau/rotl.svg)](https://travis-ci.org/fmichonneau/rotl.svg)

# An R interface to Open Tree API

This is the bleedingly-Alpha developmental version of an R package wrapping the
Open Tree of Life data APIs, which being developed as part of the
[NESCENT/OpenTree/Arbor
hackathon](http://blog.opentreeoflife.org/2014/06/11/apply-for-tree-for-all-a-hackathon-to-access-opentree-resources/).

Check out the sister repos for
[Python](https://github.com/OpenTreeOfLife/opentree-interfaces/tree/master/python)
and [Ruby](https://github.com/SpeciesFileGroup/bark). 

##Installation

If you want to play with these functions you can, via
[devtools](https://github.com/hadley/devtools).

```r
library(devtools)

install_github("fmichonneau/rotl")
```

##Examples 

Our first goal has been to impliment low-level functions that wrap the Open Tree
APIs and return the complete response. These are now done (though we still need
more tests and documentation), so we will shift our focus on developing higher 
level functions that automate common use cases and provide the results in the most
approriate R object (often trees). 

If you want to get started with open tree the existing functions can be used.
Note: it's quite possible that function names or behaviours will change, so 
treat these as something to play with for now:

###Find trees focused on my favourite taxon

```r
furry_studies <- studies_find_studies(property="ot:focalCladeOTTTaxonName", value="Mammalia")
> ( furry_ids <- unlist(furry_list$matched_studies) )
    ##ot:studyId ot:studyId ot:studyId ot:studyId ot:studyId 
    ##  "2647"     "1428"     "2582"     "2550"     "2812" 

```

###Get a tree into memory

First, use the studies_metadata to find the tree ID, then fetch a tree

```r
furry_metadata <- httr::content(get_study_meta(2647))
furry_metadata$nexml$treesById
    ##$trees2647
    ##$trees2647$treeById
    ##$trees2647$treeById$tree6169
    ##NULL
    ##
    ##
    ##$trees2647$`^ot:treeElementOrder`
    ##$trees2647$`^ot:treeElementOrder`[[1]]
    ##[1] "tree6169"
    ##
    ##
    ##$trees2647$`@otus`
    ##[1] "otus2647"
    ##
tr_string <- get_study_tree(study="2647", tree="tree6169",format="newick")

library(ape)
tr <- read.tree(text=httr::content(tr_string))
plot(tr)

```

### Get a subtree from the Big Tree

```r

( some_birds <- tol_induced_subtree(ott_ids=c(292466, 501678, 267845, 666104)))
    #Response [http://devapi.opentreeoflife.org/v2/tree_of_life/induced_subtree]
    #  Status: 200
    #  Content-type: application/json; charset=UTF-8
    #{
    #  "subtree" : "((Stellula_ott501678,(Dendroica_ott666104,Cinclus_ott267845))Neognathae_ott241846,Struthio_ott292466)Aves_ott81461;",
    #  "ott_ids_not_in_tree" : [ ],
    #  "ott_ids_not_in_graph" : [ ],
    #  "node_ids_not_in_graph" : [ ],
    #  "node_ids_not_in_tree" : [ ]
    #} 
library(ape)
( tr <- read.tree(text=httr::content(some_birds)$subtree) )
    #
    #Phylogenetic tree with 4 tips and 3 internal nodes.
    #
    #Tip labels:
    #[1] "Stellula_ott501678"  "Dendroica_ott666104" "Cinclus_ott267845"  
    #[4] "Struthio_ott292466" 
    #Node labels:
    #[1] "Aves_ott81461"        "Neognathae_ott241846" ""                    
    #
    #Rooted; no branch lengths.
```


##Data Mashups

The main motivation for the `rotl` is to allow you to use the Open Tree data
trees  perform analyses on data stored in other places, or recorded in your 
own experiments. Here's one example how that might play out:

### Why are river-resident Galaxiids so small. 

There are a group of freshwater fish in New Zealand called galaxxids. Some
species swim out as soon as they're born and come back as tasty, tasty
whitebait, others spend their whole lives in streams. The stream-resident
species mainly smaller than their sea-faring cousins, let's find out if that
difference can be explained by phylogegeny, environment of both. 

We can start by getting some data on the fish themselves from `fishbase`. The
first step is narrowing down to the 20 or so species of galxiids from New
Zealand:


```r
library(rfishbase)
data(fishbase)

galaxiids <- which_fish("Galaxias", using="Genus")
nz_fish <- which_fish("New\\ Zealand", using="distribution")
nz_galaxiids <- fish.data[galaxiids & nz_fish]
length(nz_galaxiids)
    ## [1] 18
```

We can now turn to the Open Tree of Life to see if these species are represented. 
`nz_galaxiids` is a named list, so we can pull out the `ScientificName` from
each list element and as Open Tree taxon resolution surface what it makes of it

```r
(spp_names <- sapply(nz_galaxiids, "[[", "ScientificName"))
    # [1] "Galaxias brevipinnis"    "Galaxias maculatus"     
    # [3] "Galaxias gracilis"       "Galaxias postvectis"    
    # [5] "Galaxias argenteus"      "Galaxias divergens"     
    # [7] "Galaxias fasciatus"      "Galaxias paucispondylus"
    # [9] "Galaxias vulgaris"       "Galaxias prognathus"    
    #[11] "Galaxias anomalus"       "Galaxias rekohua"       
    #[13] "Galaxias depressiceps"   "Galaxias pullus"        
    #[15] "Galaxias eldoni"         "Galaxias cobitinis"     
    #[17] "Galaxias gollumoides"    "Galaxias macronasus"  
    #

ott_resolved_names <- tnrs_match_names(spp_names)
head(ott_resolved_names)
##    
##             search_string          unique_name approximate_match ott_id
##    1 galaxias brevipinnis Galaxias brevipinnis             FALSE 561130
##    2   galaxias maculatus   Galaxias maculatus             FALSE  77073
##    3    galaxias gracilis    Galaxias gracilis             FALSE  53697
##    4  galaxias postvectis  Galaxias postvectis             FALSE 516938
##    5   galaxias argenteus   Galaxias argenteus             FALSE 456604
##    6   galaxias divergens   Galaxias divergens             FALSE 776566
```

Thankfully, this set of names is fully resolved! We can go ahead at grab our
tree:

```r
tr <- tol_induced_subtree(ott_resolved_names$ott_id
```

