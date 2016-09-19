## Test environments

- local Ubuntu 16.04, R 3.3.1
- Ubuntu 12.04 (travis-ci), R 3.3.1
- Windows with win-builder (R 3.3.1 and R Under development 2016-09-18 r71304)
- local Debian, using R Under development (unstable) (2016-09-18 r71304)

## R CMD check results

- There were no ERRORs or WARNINGs

- There was 1 NOTE

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Francois Michonneau <francois.michonneau@gmail.com>'

License components with restrictions and base license permitting such:
  BSD_2_clause + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Francois Michonneau, Joseph W. Brown, David Winter

Possibly mis-spelled words in DESCRIPTION:
  API (2:45, 9:54)
  phylogenetic (10:5, 12:60)
```

## CRAN Package Check Results

* This release addresses the WARNINGs associated with the vignettes listed.

## Downstream dependencies

* taxize: no WARNING or NOTE generated.
