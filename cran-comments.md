## Submission for package enhancement

### Major changes

* Developed an option for graded response model.
* Made MLE available for all model-fitting functions.
* Developed `latent_distribution`.
* Developed `inform_f_item` and `inform_f_test`.

### Minor changes

* Outputs reflect item names in an input file.
* Enhanced algorithm convergence for model-fitting functions.
* `reliability` calculates a coefficient on the $\theta$ scale.
* `reliability` uses the raw value in a given data for calculating the reliability coefficient.
* `reliability` returns both test reliability coefficient and item reliability coefficients.

  
## Test environments

* macOS 12.6.8 (release), R 4.3.1

* Ubuntu 22.04.3 (oldrel-1), R 4.2.3

* Ubuntu 22.04.3 (release), R 4.3.1

* win-builder (devel and release)


## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

0 ERRORs | 0 WARNINGs | 0 NOTEs
