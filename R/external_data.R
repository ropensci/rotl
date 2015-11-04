study_external_IDs <- function(study_id){
    meta <- get_study_meta(study_id)
    data_deposit <- meta[["nexml"]][["^ot:dataDeposit"]][["@href"]]
    url <- meta[["nexml"]][["^ot:studyPublication"]][["@href"]]
    doi <- sub("http://dx.doi.org/", "", url)    
    pmid <- get_pmid(study_id)
    pops <- entrez_link(dbfrom="pubmed", db="popset", id=pmid)[["links"]][["pubmed_popset"]]
    nuc <- entrez_link(dbfrom="pubmed", db="nuccore", id=pmid)[["links"]][["pubmed_nuccore"]]
    res <- list( doi = doi, 
                 pubmed_id = pmid, 
                 popset_ids = pops, 
                 nucleotide_ids   = nuc, 
                 external_data_url = data_deposit)
    structure(res, class=c("external_data", "list"), study_id=study_id)
}

print.external_data <- function(x, ...){
    cat("External data identifiers for study", attr(x, "study_id"), "\n")
    cat(" $doi: ", x[["doi"]], "\n")
    cat(" $pubmed_id: ", x[["pubmed_id"]], "\n")
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

summarize_nucleotide_data <- function(id_vector){
    summs <- entrez_summary(db="nuccore", id=id_vector)
    interesting <- extract_from_esummary(summs, c("uid", "title", "slen", "organism", "completeness"), simplify=FALSE)
    do.call(rbind.data.frame, interesting) 
}

summarize_popset_data <- function(id_vector){
    summs <- entrez_summary(db="popset", id=id_vector)
    interesting <- extract_from_esummary(summs, c("uid", "title"), simplify=FALSE)
    do.call(rbind.data.frame, interesting) 
}



get_pmid <- function(study_id){
    url <- attr(get_publication(get_study_meta(study_id)), "DOI")
    doi <- sub("http://dx.doi.org/", "", url)
    pubmed_search <- entrez_search(db="pubmed", term=paste0(doi, "[DOI]"))
    if(length(pubmed_search$ids) == 0){
        warning("Could not find PMID for study'", study_id, "', returning NULL")
        return(NULL)
    }
    if(length(pubmed_search$ids) > 1){
        warning("Could found more than one PMID matching study'", study_id, "', returning NULL")
        return(NULL)
    }    
    pubmed_search$ids
}

