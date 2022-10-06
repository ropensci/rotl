make_vignettes <- function() {
  vignettes <- list.files("vignettes", pattern = ".Rmd.orig", full.names = TRUE)
  purrr::walk2(
    vignettes,
    file.path("vignettes", gsub("\\.orig", "", basename(vignettes))),
    function(x, y) {
      knitr::knit(x, output = y)
    }
  )
  ## clean figure directory if it exists
  if (dir.exists("vignettes/figure")) {
    unlink("vignettes/figure/", recursive = TRUE, force = TRUE)
  }
  ## recreate the directory
  dir.create("vignettes/figure")
  ## move the figures
  figures <- list.files("figure", pattern = "\\.png$", full.names = TRUE)
  res_mv <- file.rename(figures, file.path("vignettes", figures))
  stopifnot(all(res_mv))
  
  ## remove base directory
  unlink("figure", recursive = TRUE, force = TRUE)

  ## returns TRUE is everything worked
  invisible(!dir.exists("figure"))
}
