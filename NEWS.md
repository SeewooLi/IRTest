# IRTest 2.0.0

# IRTest 1.12.1

# IRTest 1.12.0

* Developed functions for medel comparison and model selection.
* Modified `GHc` as an implicit object.
* Provided examples for functions.
* Changed from `any(class())` to `inherits()`.
* Provided more S3 methods.
* Updated the manual.
* Added `coef_se` that returns S.E. of item parameter estimates.

# IRTest 1.11.0

* Graded response model (`"GRM"`) is now available.
* Developed `MLE` for mixed-format analysis.

# IRTest 1.10.0

* Outputs reflect item names in an input file.
* Enhanced algorithm convergence for `IRTest_Dich` when there exist one or more negative slope parameters.
* Error in calculating MLE in the presence of missing data is resolved.
* `summary` returns the HQ criterion.
* Error in `DataGeneration`'s `model_D` argument is fixed.
* `plot` function employs `latent_distribution`.

# IRTest 1.9.1

* Minor errors have been corrected such as the calculation of $\rho_{\theta\theta^{'}}$ with discrete latent distribution and the calculation of $sd$ in the Davidian-cure method.
* Vignette file has been updated.

# IRTest 1.9.0

* `latent_distribution` has been developed. This function evaluates an estimated latent density function.
* `reliability` calculates a coefficient on the $\theta$ scale.

# IRTest 1.8.1

* Convergence of the estimation algorithm was enhanced for `"PCM"` by inserting latest item parameters instead of inserting starting values repeatedly.

# IRTest 1.8.0

* `inform_f_item` and `inform_f_test` are developed.
* Convergence for `"DC"` and `"LLS"` was enhanced by updating density parameters every iteration.

# IRTest 1.7.1

* Minor errors in the manual file are corrected.
* `reliability` uses the raw value in a given data for calculating the reliability coefficient.
* `reliability` returns both test reliability coefficient and item reliability coefficients.
* Every estimation function returns the raw score-categories of items.

# IRTest 1.7.0

* Log-linear smoothing method (`latent_dist = "LLS"`) can be applied to estimate a latent distribution.

# IRTest 1.6.0

* `reliability` has been developed to calculate marginal reliability coefficient of IRT.

# IRTest 1.5.2

* Some of the polytomous items could be technically dichotomous when one or more of the categories are not observed in data.
Now, these items can be analyised with the polytomous function.

# IRTest 1.5.1

* Rasch-type analysis imposes a constraint on slope parameters to be equal across all items.
Before, they were fixed to 1 regardless of the characteristics of data.

# IRTest 1.5.0

* `plot_item` has been developed to draw an item response function.

# IRTest 1.4.1

* Enhanced algorithm convergence for `IRTest_Poly` when there exist one or more negative slope parameters.

# IRTest 1.4.0

* `item_fit` evaluates item fit.

# IRTest 1.3.1

* Starting values (`initialitem`) can be automatically generated.

# IRTest 1.3.0

* `NA` values are permitted for estimation functions.

# IRTest 1.2.0

* The `MLE` option for ability parameter estimation is added for polytomous data.

# IRTest 1.1.0

* The `PCM` model for polytomous data is available.

# IRTest 1.0.0

* Unit tests are more thoroughly conducted to enhance 'test coverage.'
* S3 class generic functions (i.e., print, plot, summary) can now be used for IRTest results.
* Plotting function "plot_LD" is substituted with the generic function `plot`.
* Additional `ggplot2` arguments can be specified to `plot`.
* Badges are added to the Github main page in order for the current status of IRTest to be easily recognized.
* In the `summary`, additional information is provided; a cursory view of the latent distribution, model fit indices, and the number of parameters used.

# IRTest 0.1.0

* Ability parameter `MLE` is available for `IRTest_Dich`.
* Data Generation with fixed item and/or ability parameters is available.

# IRTest 0.0.2

* Re-submission to CRAN.

# IRTest 0.0.1

* The initial release.




