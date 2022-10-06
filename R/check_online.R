is_otl_online <- function() {
  !is.null(curl::nslookup("api.opentreeoflife.org", error = FALSE))
}
