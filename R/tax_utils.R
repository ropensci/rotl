## all extended-taxon-descriptors have:
## - ott_id
## - name
## - rank
## - unique_name
## - tax_sources
## and they may have
## - flags
## - synonyms
## - is_suppressed

## builds the functions to access the content of the taxon descriptors.
## slot: the name of the list element we need to access
## flatten: if the list element is a list, make it a vector
## optional: is the slot found in all taxon descriptors or only in some
tax_access_factory <- function(slot, flatten, optional) {
    function(tax) {
        if ((!exists(slot, tax))) {
            if (optional) {
                warning("This object doesn't have ", sQuote(slot), call. = FALSE)
                return(NULL)
            } else {
                stop("Invalid taxon object", call. = FALSE)
            }
        } else {
            if (flatten) {
                unlist(tax[[slot]])
            } else {
                tax[[slot]]
            }
        }
    }
}

.tax_ott_id <- tax_access_factory("ott_id", flatten = FALSE, optional = FALSE)

.tax_name <- tax_access_factory("name", flatten = FALSE, optional = FALSE)

.tax_rank <- tax_access_factory("rank", flatten = FALSE, optional = FALSE)

.tax_sources <- tax_access_factory("tax_sources", flatten = TRUE,
                                  optional = FALSE)

.tax_unique_name <- tax_access_factory("unique_name", flatten = FALSE,
                                      optional = FALSE)


## optional
.tax_flags <- tax_access_factory("flags", flatten = TRUE, optional = TRUE)

.tax_is_suppressed <- tax_access_factory("is_suppressed", flatten = FALSE,
                                        optional = TRUE)

.tax_synonyms <- tax_access_factory("synonyms", flatten = TRUE, optional = TRUE)

## Does the slot element represent a taxon?
is_taxon <- function(slot) {
    if (all(c("ott_id", "name", "rank", "tax_sources",
              "unique_name") %in% names(slot))) {
        TRUE
    } else {
        FALSE
    }
}

### adds a class to the objects returned by the methods
add_otl_class <- function(res, .f) {
    ## we need a prefix to avoid class name conflict
    ## apparently the class "name" already exists
    class(res) <- c(paste0("otl_", as.list(environment(.f))[["slot"]]),
                    class(res))
    res
}
