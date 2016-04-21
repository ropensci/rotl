##' Get external identifiers for data associated with an Open Tree study
##'
##' Data associated with studies contributing to the Open Tree synthesis may
##' be available from other databases. In particular, trees and alignments 
##' may be available from treebase and DNA sequences and bibliographic
##' information associated with a given study may be available from the NCBI.
##' This function retrieves that information for a given study.
##'  
##' @param study_id An open tree study ID
##' @return A study_external_data object (which inherits from a list) which
##' contains some of the following.
##' @return doi, character, the DOI for the paper describing this study
##' @return external_data_url, character, a URL to an external data repository 
##' (e.g. a treebase entry) if one exists.
##' @return pubmed_id character, the unique ID for this study in the NCBI's pubmed database
##' @return popset_ids character, vector of IDs for the NCBI's popset database
##' @return nucleotide_ids character, vector of IDs for the NCBI's nucleotide database 
##' @seealso studies_find_studies (used to discover study IDs)
##' @importFrom httr parse_url
##' @importFrom rentrez entrez_search
##' @importFrom rentrez entrez_link
##' @examples
##' \dontrun{
##' flies <- studies_find_studies(property="ot:focalCladeOTTTaxonName", value="Drosophilidae")
##' study_external_IDs(flies[2,]$study_ids)
##' }
##' @export

study_external_IDs <- function(study_id){
    meta <- get_study_meta(study_id)
    data_deposit <- meta[["nexml"]][["^ot:dataDeposit"]][["@href"]]
    url <- attr(get_publication(meta), "DOI")
    doi <- parse_url(url)$path    
    pmid <- get_pmid(doi, study_id)
    res <- list( doi = doi, 
                 pubmed_id = pmid, 
                 external_data_url = data_deposit)
    if(!is.null(pmid)){
        res$popset_ids <- entrez_link(dbfrom="pubmed", db="popset", id=pmid)[["links"]][["pubmed_popset"]]
        res$nucleotide_ids <- entrez_link(dbfrom="pubmed", db="nuccore", id=pmid)[["links"]][["pubmed_nuccore"]]
    }
    structure(res, class=c("study_external_data", "list"), id=study_id)
}

##' Get external identifiers for data associated with an Open Tree taxon
##'
##' The Open Tree taxonomy is a synthesis of multiple reference taxonomies. This
##' function retrieves identifiers to external taxonomic records that have
##' contributed the rank, position and definition of a given Open Tree taxon.
##'
##' @param taxon_id An open tree study ID
##' @return a data.frame in which each row represents a unique record in an
##' external databse. The column "source" provides and abbreviated name for the 
##' database, and "id" the unique ID for the record.
##' @seealso tnrs_matchnames, which can be used to search for taxa by name.
##' @seealso taxonomy_taxon, for more information about a given taxon.
##' @examples
##' \dontrun{
##'    gibbon_IDs <- taxon_external_IDs(712902) 
##' }
##' @export

taxon_external_IDs <- function(taxon_id){
    taxon_info <- taxonomy_taxon_info(taxon_id)
    srcs <- taxon_info[[1]][["tax_sources"]]
    res <- do.call(rbind.data.frame, strsplit(unlist(srcs), ":"))
    names(res) <- c("source", "id")
    res
}

#'@export
print.study_external_data <- function(x, ...){
    cat("External data identifiers for study", attr(x, "study_id"), "\n")
    cat(" $doi: ", x[["doi"]], "\n")
    if(!is.null(x$pubmed_id)){
        cat(" $pubmed_id: ", x[["pubmed_id"]], "\n")
    }
    if(!is.null(x$popset_ids)){
        cat(" $popset_ids: vector of",  length(x[["popset_ids"]]), "IDs \n")
    }
    if(!is.null(x$nucleotide_ids)){
        cat(" $nucleotide_ids: vector of", length(x[["nucleotide_ids"]]), "IDs\n")
    }
    if(nchar(x[["external_data_url"]]) > 0){
        cat(" $external_data_url", x[["external_data_url"]], "\n")
    }
    cat("\n")
}

##Maybe include these functions to get summary information about a 
## set of linked sequences?
#summarize_nucleotide_data <- function(id_vector){
#    summs <- entrez_summary(db="nuccore", id=id_vector)
#    interesting <- extract_from_esummary(summs, c("uid", "title", "slen", "organism", "completeness"), simplify=FALSE)
#    do.call(rbind.data.frame, interesting) 
#}
#
#summarize_popset_data <- function(id_vector){
#    summs <- entrez_summary(db="popset", id=id_vector)
#    interesting <- extract_from_esummary(summs, c("uid", "title"), simplify=FALSE)
#    do.call(rbind.data.frame, interesting) 
#}
#

#Un-exported function to convert doi->pmid. Also takes study_id as an argument in
#order to provide a helpful error message when 0 or >1 pmids are returned.
get_pmid <- function(doi, study_id){
    pubmed_search <- entrez_search(db="pubmed", term=paste0(doi, "[DOI]"))
    if(length(pubmed_search$ids) == 0){
        warning("Could not find PMID for study'", study_id, "', skipping NCBI data")
        return(NULL)
    }
    if(length(pubmed_search$ids) > 1){
        warning("Found more than one PMID matching study'", study_id, "', skipping NCBI data")
        return(NULL)
    }    
    pubmed_search$ids
}

