## Summary information about the OpenTree Taxaonomy (OTT)
.taxonomy_about <- function () {
    res <- otl_POST(path="/taxonomy/about", body=list())
    cont <- httr::content(res)
    return(cont)
}


## Information about an OpenTree Taxonomy (OTT) taxon
.taxonomy_taxon <- function (ott_id=NULL) {
	if (is.null(ott_id)) {
		stop("Must supply an \'ott_id\' argument")
	} else if (length(ott_id) > 1) {
		stop("Must only supply one \'ott_id\' argument")
	}
	q <- list(ott_id=jsonlite::unbox(ott_id))
    res <- otl_POST(path="/taxonomy/taxon", body=q)
    cont <- httr::content(res)
    return(cont)
}


## Get a subtree from the OpenTree Taxonomy (OTT) taxonomic tree
.taxonomy_subtree <- function (ott_id=NULL) {
	if (is.null(ott_id)) {
		stop("Must supply an \'ott_id\' argument")
	} else if (length(ott_id) > 1) {
		stop("Must only supply one \'ott_id\' argument")
	}
	q <- list(ott_id=jsonlite::unbox(ott_id))
    res <- otl_POST(path="/taxonomy/subtree", body=q)
    cont <- httr::content(res)
    return(cont)
}


## Get the least inclusive common ancestor (LICA) from nodes in the OpenTree Taxonomy (OTT) 
.taxonomy_lica <- function (ott_ids=NULL) {
	if (is.null(ott_ids)) {
		stop("Must supply an \'ott_ids\' argument")
	}
	q <- list(ott_ids=ott_ids)
    res <- otl_POST(path="/taxonomy/lica", body=q)
    cont <- httr::content(res)
    return(cont)
}
