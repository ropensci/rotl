## Test environments

- local Ubuntu 16.04, R 3.2.3
- Ubuntu 12.04 (travis-ci), R 3.2.5
- Windows with win-builder (R 3.2.3 and 3.3.0 beta 2016-04-23 r70535)
- local Debian, using R Under development (unstable) (2016-04-24 r70544)

## R CMD check results

- There were no ERRORs or WARNINGs

- There was 1 NOTE

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Francois Michonneau <francois.michonneau@gmail.com>'

License components with restrictions and base license permitting such:
  BSD_2_clause + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Francois Michonneau, Joseph W. Brown, David Winter

Possibly mis-spelled words in DESCRIPTION:
  API (2:45, 9:54)
  phylogenetic (10:5, 12:60)
```


## CRAN Package Check Results

* WARNING and NOTES seen on Windows have been addressed (gdata dependency removed)

## Downstream dependencies

* taxize: no WARNING or NOTE generated.
