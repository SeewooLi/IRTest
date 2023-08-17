## Submission for package enhancement

In this version I have:

* Made the `PCM` model for polytomous data available.

* Developed the `MLE` option for ability parameter estimation for polytomous data.

* Revised the estimation functions to permit NA values.

* Made automatic generation of starting values available, which rendered the estimation functions more user-friendly.

* Developed functions (`item_fit`, `plot_item`, `reliability`) for practical usages of IRT.

* Enhanced algorithm convergence when there exist one or more negative slope parameters.

* Freed the constraint on the Rasch-type analysis to have the same slope parameters across all items but not fixed to 1.

* Revised `IRTest_Poly` to accept dichotomous items.

* Made log-linear smoothing method (`latent_dist="LLS"`) available.

  
## Test environments

* macOS 11.5.2 (release), R 4.2.1

* Ubuntu 20.04.4 (oldrel-1), R 4.1.3

* Ubuntu 20.04.4 (release), R 4.2.1

* win-builder (devel and release)


## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

0 ERRORs | 0 WARNINGs | 0 NOTEs
