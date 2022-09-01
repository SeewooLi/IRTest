## Test environments
Tests were conducted on github 
using the function "usethis::use_github_action_check_standard()".
* macOS-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1)
* ubuntu-latest (release)
* windows-latest (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.
