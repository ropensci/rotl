This submission should remove the warnings that one of the vignettes generates
with R-devel as indicated by Kurt Hornik to me on January 15.

## Test environments

- local Ubuntu 15.10, R 3.2.3
- Ubuntu 12.04 (travis-ci), R 3.2.3
- Windows with win-builder (R 3.2.3 and R-devel r70055)
- local Debian, R-devel r69918

## R CMD check results

- There were no ERRORs or WARNINGs

- There was 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Francois Michonneau <francois.michonneau@gmail.com>’

License components with restrictions and base license permitting such:
  BSD_2_clause + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Francois Michonneau, Joseph W. Brown, David Winter

Possibly mis-spelled words in DESCRIPTION:
  phylogenetic (10:5, 12:60)

## CRAN Package Check Results

* WARNING seen with R-devel on Linux have been addressed.

* NOTEs seen on R-patched, R-release, R-oldrel have been addressed.

## Downstream dependencies

- None
