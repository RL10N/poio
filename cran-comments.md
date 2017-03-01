## Release Summary

This release has a single new feature, allowing users to override metadata in 
fix_metadata().

## Test Environments

* Local macOS Sierra, R-devel
* Travis-CI, R-devel and R-release
* R-hub, R-devel, R-release and R-old-release

## R CMD check results

There were no ERRORs or WARNINGs.

R-hub has a NOTE that it found some marked UTF-8 strings. These are country names and are intended to be UTF-8.

## Downstream dependencies

msgtools modified to work with new version.
