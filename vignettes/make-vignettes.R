make_vignettes <- function() {
  vignettes <- list.files("vignettes", pattern = ".Rmd.orig", full.names = TRUE)
  purrr::walk2(
    vignettes,
    file.path("vignettes", gsub("\\.orig", "", basename(vignettes))),
    function(x, y) {
      knitr::knit(x, output = y)
    }
  )
  ## move the figures
  figures <- list.files(".", pattern = "fig-vignettes-.+\\.png$", full.names = TRUE)
  res_mv <- file.rename(figures, file.path("vignettes", figures))
  stopifnot(all(res_mv))
  TRUE
}
