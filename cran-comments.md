## Resubmission

- This addresses the non-canoncial URL in the vignette, and the warning
  regarding the vignettes being more recent than the files found in inst/doc.

## Comments

- This submission addresses the comment by Prof Ripley from 2017-02-27. The use
  of the suggested package 'fulltext' is now conditional.


## Test environments

- local Ubuntu 16.10, R 3.3.2
- Ubuntu 12.04 (travis-ci), R 3.3.2
- Windows with R-hub (R 3.3.2 and R Under development (2017-02-28 r72286)
- local Debian, using R Under development (unstable) (2017-03-01 r72295)

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

## Downstream dependencies

* taxize: no WARNING or NOTE generated.
