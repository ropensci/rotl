## Comments

- This submission addresses the Warnings seen on the CRAN checks

## Test environments

- local Ubuntu 17.10, R 3.4.4
- Ubuntu 14.04 (travis-ci), R 3.3.3, R 3.4.4, and R devel (2018-04-02 r74501)
- Windows with R-hub (R Under development (2018-03-30 r74499)

## R CMD check results

- There were no ERRORs or WARNINGs

- There was 3 NOTEs

```
* Maintainer: ‘Francois Michonneau <francois.michonneau@gmail.com>’

* Unknown, possibly mis-spelled, fields in DESCRIPTION:
  ‘X-schema.org-keywords’ ‘X-schema.org-isPartOf’ ‘X-schema.org-relatedLink’
  
* checking DESCRIPTION meta-information ... NOTE
Authors@R field gives persons with non-standard roles:
  Scott Chamberlain [rev] (0000-0003-1444-9135): rev
```

The "unknown" fields in DESCRIPTION corresponds to the codemeta metadata standard for the package as indicated here: https://github.com/ropensci/codemetar#using-the-description-file

The role "rev" stands for reviewer and it is my understanding that it will fully supported starting with R 3.5.0


## Downstream dependencies

* taxize: no WARNING or NOTE generated.
