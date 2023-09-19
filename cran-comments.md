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

* macOS 11.5.2 (release), R 4.2.1

* Ubuntu 20.04.4 (oldrel-1), R 4.1.3

* Ubuntu 20.04.4 (release), R 4.2.1

* win-builder (devel and release)


## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

0 ERRORs | 0 WARNINGs | 0 NOTEs
