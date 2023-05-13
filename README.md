## Overview
Regression analysis commonly features both continuous predictors and categorical predictors. A particular scenario of interest occurs when the categorical predictors include race. Due to the broad and pervasive effects of race, it is often necessary and appropriate to include not only an additive term for race, but also race-specific effects for other covariates, i.e., interaction terms. However, current methods are problematic:

- Admitting race-specific effects increases the number of parameters by a large factor, especially if we include interaction terms. We often address this via some sort of regularization.
- The large number of predictors introduces challenges for interpretability of the model parameters. Most importantly, it can create fundamental (racial) inequities in the presentation of results.

We generally select the baseline category as the most abundant level in the data, which for race is typically non-Hispanic white (NHW). When we present model-based results, we typically label the slope coefficients by the $x$ variable, e.g., age. Crucially, this labeling fails to reference the baseline group. As a consequence, the results are implicitly presented through the lens of NHW participants, while other the interactions "modify" the NHW-specific effect.

Furthermore, purely from a modeling perspective, the estimation and inference depend undesirably on the baseline. This is particularly concerning in the presence of regularization, such as penalized estimation, prior specification, or variable selection, since these tools will lack symmetry in the categories. For instance, consider regularization the sparsifies or shrinks coefficients toward zero: The effect is to pull the race-specific effects toward the NHW-specific effect (and only the NHW-specific effect), rather than toward some global effect. The (racial) asymmetry is clear, and model-based inferences may differ depending on the choice of baseline.

`lmabc` introduces an implementation of linear regression with abundance-based constraints, which seeks to increase equity and interpretability in statistical analyses. `lmabc` uses an averaging approach to change the baseline from a single arbitrarily-chosen level to the global average.

## Installation
`lmabc` can be installed and loaded as follows:

```
remotes::install_github("prayaggordy/lmabc")

library("lmabc")
```

## Future work
Users can develop their own ABCs-inspired methods using the `getConstraints()` and `getFullDesign()` methods in this package. Please email the package maintainer with any issues or questions.
