## Test environments
* Ubuntu Linux 16.04.6 LTS, R 4.0.0 (Travis-CI)
* Ubuntu Linux 18.04.5 LTS R 4.0.2 (local)
* win-builder (devel, release, and old release)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (R-hub)

## R CMD check results
There was 1 NOTE:

```
*  checking for future file timestamps ... NOTE
unable to verify current time
```

It appears that the worldclockapi resource (http://worldclockapi.com/) is not
currently available. This NOTE will disappear when it comes back online.

There were no ERRORs or WARNINGs.

## Downstream dependencies
I have also run R CMD CHECK on macleish, the only downstream dependency of 
clifro, without any problems.

## Resubmission

This is a resubmission. In this version, I have:

- Changed **http**://ropensci.org to **https**://ropensci.org/ in `README.md`.
- Changed https://**www.**niwa.co.nz to https://niwa.co.nz/ in the `clifro` vignette.
- Included a fully-specified URL to `CONDUCT.md`.

These changes have been made to remove the 'Found the following (possibly) 
invalid URLs' NOTE in the CRAN checks.
