# lmabc: Linear Regression with Abundance-Based Constraints (ABCs)

lmabc provides estimation and inference for linear regression with
categorical covariates (race, sex, etc.). Common strategies, including
the defaults in lm, select a "reference" group (White, Male, etc.) and
present all results relative to this group. However, this approach
suffers from statistical inefficiencies, alarming biases, and limited
interpretability. lmabc uses abundance-based constraints (ABCs) to
resolve each of these problems. Because of the unique statistical
properties of ABCs, there is "nothing to lose" by including
categorical-categorical and categorical-continuous interactions; yet
potential gains include greater statistical power for the main effects
and discovery of heterogeneous effects. lmabc reproduces the core
functionality of lm and includes functions for generalized (logistic,
Poisson, etc.) and penalized (lasso, ridge, etc.) linear models. Details
are provided in Kowal (2026)
[doi:10.1080/01621459.2026.2635078](https://doi.org/10.1080/01621459.2026.2635078)
and Kowal (2024)
[doi:10.1101/2024.01.04.23300033](https://doi.org/10.1101/2024.01.04.23300033)
.

## See also

Useful links:

- <https://drkowal.github.io/lmabc/>

- <https://github.com/drkowal/lmabc>

## Author

**Maintainer**: Dan Kowal <daniel.r.kowal@gmail.com>

Authors:

- Prayag Gordy

- Virginia Baskin

- Jai Uparkar

Other contributors:

- Caleb Fikes \[contributor\]
