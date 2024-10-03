## Submission for package enhancement

### Major changes

* In addition to dichotomous and polytomous responses, IRT analysis for continuous item responses is now available.
  - `IRTest_Cont` has been added.
  - Other utility functions of IRTest are applicable to an output of `IRTest_Cont`.
  
* Weighted likelihood estimation (WLE) is available for ability parameter estimation.

* `adaptive_testing` is added to expedite ability parameter estimation in adaptive testing.

### Minor changes

* Modified estimation functions to be utilized in ability estimation of adaptive testing.

  
## Test environments

* macOS Monterey 12.7.3 (release), R 4.3.3

* Ubuntu 22.04.4 (oldrel-1), R 4.2.3

* Ubuntu 22.04.4 (release), R 4.3.3

* win-builder (devel and release), R 4.3.3


## R CMD check results

There were no ERRORs or WARNINGs.

0 ERRORs | 0 WARNINGs | 1 NOTEs

* checking CRAN incoming feasibility ... [13s] NOTE
Maintainer: 'Seewoo Li <seewooli@g.ucla.edu>'

New maintainer:
  Seewoo Li <seewooli@g.ucla.edu>
Old maintainer(s):
  Seewoo Li <cu@yonsei.ac.kr>
