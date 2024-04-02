
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `lmabc`

## Introduction

Regression analysis commonly features categorical covariates, such as
race, sex, group/experimental assignments, and many other examples.
These variables are often interacted with (or *modify*) other variables,
for example to infer group-specific effects of another variable `x` on
the response `y` (e.g., does exposure to a pollutant `x` more adversely
impact the health `y` of certain subpopulations?). However, default
numerical encodings of categorical variables suffer from statistical
inefficiencies, limited interpretability, and alarming biases,
particularly for protected groups (race, sex, religion, national origin,
etc.).

`lmabc` addresses each of these problems, outlined below. `lmabc`
provides estimation, inference, and prediction for linear regression
with categorical covariates and interactions, including methods for
penalized (lasso, ridge, etc.) and generalized (logistic, Poisson, etc.)
regression. For ease of use, `lmabc` matches the functionality of `lm`
and `glm`.

## Installation

`lmabc` is not yet on CRAN. The latest version can be installed and
loaded from GitHub. The installation should take no more than a few
seconds.

``` r
pak::pak("drkowal/lmabc")

library("lmabc")
```

## Default strategies: the problem

The predominant strategy for linear regression with categorical
covariates is *reference group encoding* (RGE), which parametrizes and
estimates the regression coefficients relative to a pre-selected
reference category. For illustration, consider `race`, where the usual
reference group is non-Hispanic White (NHW). This leads to several
serious problems:

1.  **Presentation bias**: RGE elevates a single group (e.g., NHW) above
    others. All other group-specific effects are presented relative to
    this group, which implicitly frames one group as “normal” and the
    others as “deviations from normal”. In all major statistical
    software (R, SAS, Python, MATLAB, Stata, etc.), the regression
    output for models of the form `y ~ x + race + x:race`, which is used
    to estimate group-specific `x` effects, presents the `x` effect
    without explicitly acknowledging that it actually refers to the `x`
    effect *for the reference (NHW) group*. A similar problem occurs for
    the intercept and is compounded for multiple categorical covariates
    and interactions. Alarmingly, this output can lead to misleading
    conclusions about the `x` effect for the broader population. For
    protected groups (race, sex, religion, national origin, etc.), this
    output is inequitable.
2.  **Statistical bias**: Modern statistical and machine learning
    methods commonly use regularization strategies, including penalized
    regression, variable selection, and Bayesian inference, to stabilize
    (i.e., reduce the variance of) estimators by “shrinking”
    coefficients toward zero. With RGE, the implication is that
    group-specific `x` effects are (racially) *biased* toward the
    reference (NHW) `x` effect. This bias also attenuates the estimated
    differences between each group-specific `x` effect and the reference
    (NHW) `x` effect, which undermines the ability to detect
    group-specific differences. Finally, it implies that model estimates
    and predictions are dependent on the choice of the reference group.
3.  **Statistical inefficiencies**: Adding interaction effects with
    another covariate (e.g., `x:race`) changes the estimates *and* the
    standard errors for the main `x` effect. As such, analysts may be
    reluctant to include interaction effects, especially if they lead to
    reductions in statistical power. This is usually the case: the main
    `x` effect in `y ~ x + race` is common to all (race) groups, while
    the main `x` effect in `y ~ x + race + x:race` is specific to the
    reference (NHW) group, and thus a subset of the data.
4.  **Limited interpretability**: Since RGE identifies each categorical
    covariate with a reference group, it becomes difficult to interpret
    the parameters, especially with multiple categorical covariates and
    interactions. Even the simple model `y ~ x + race + x:race` does not
    yield an obvious main (and appropriately global) `x` effect, since
    the coefficient on `x` corresponds to the `x` effect for the
    reference (NHW) group.

These problems are not solved by changing the reference category. Other
strategies, like sum-to-zero constraints, can address 1-2 but fail to
address 3-4. Omitting constraints entirely is not feasible without some
regularization, but regardless cannot solve 3-4 (and, for lasso
estimation, tends to reproduce RGE, thus 1-2 resurface).

## ABCs: the solution

`lmabc` resolves each of these problems for linear regression with
categorical covariates. Using Abundance-Based Constraints (ABCs),
`lmabc` includes estimation, inference, and prediction for penalized
(lasso, ridge, etc.) and generalized (logistic, Poisson, etc.)
regression with three key features, called “the EEI of ABCs”:

1.  **Equitability**: Presentation biases and statistical biases are
    both eliminated. For models like `y ~ x + race + x:race`, the main
    `x` effect is parametrized and estimated as a “group-averaged” `x`
    effect. No single (race) group is elevated. Instead, all
    group-specific `x` effects are presented relative to the global
    (i.e., “group-averaged”) `x` effect. This also resolves the (racial)
    biases in regularized estimation: shrinkage is toward an
    appropriately global `x` effect, not the reference (NHW) `x` effect,
    with meaningful and equitable notions of sparsity.
2.  **Efficiency**: ABCs ensure that the estimated main `x` effects for
    `y ~ x + race` and `y ~ x + race + x:race` are *identical* (if `x`
    is also categorical; they are *nearly identical* if `x` is
    continuous, under some conditions). If the interaction effect
    (`x:race`) is small, then the standard errors for `x` are also
    (nearly) identical between the two models. When the interaction
    effect is large, the standard errors for `x` *decrease* for the
    model that includes the interaction. Remarkably, with ABCs,
    including the interaction has no negative consequences: the main
    effect estimates are (nearly) unchanged and the standard errors are
    either (nearly) unchanged or smaller.
3.  **Interpretability**: The `x` and `x:race` coefficients are
    parametrized as “group-averaged” `x`-effects and “group-specific
    deviations”, respectively. This, coupled with the aforementioned
    invariance properties for estimation and inference, enables
    straightforward interpretations of both main and interaction
    effects.

While the benefits of equitability and interpretability are
self-evident, we also emphasize the importance of statistical
efficiency. For an analyst considering “main-only” models of the form
`y ~ x + race`, ABCs allow the addition of interaction effects `x:race`
“for free”: they have (almost) no impact on estimation and inference for
the main `x` effect—unless the `x:race` interaction effect is strong, in
which case the analyst gains *more power* for the main `x` effect. Yet
by including the interaction, the analyst can now investigate
group-specific `x` effects, again without negative consequences for the
main effects. This is usually *not* the case for regression analysis,
and does not occur for RGE (or other approaches).

These benefits apply to any categorical covariates. Generalizations for
multiple continuous and categorical covariates and interactions are also
available.

## Future work

Users can develop their own ABCs-inspired methods using the
`getConstraints()` and `getFullDesign()` methods in this package. Please
email the package maintainer with any issues or questions.

The current implementation of `lmabc` is slightly slower than `lm`, but
only for massive datasets. To benchmark, we constructed a 1 million row
dataset and regressed a continuous outcome on two continuous predictors,
three categorical predictors, a continuous-continuous interaction, a
continuous-categorical interaction, and a categorical-categorical
interaction. `lm` averaged 0.7 seconds, while `lmabc` averaged 3
seconds.

## Dependencies

`lmabc` requires the “base” R packages: `graphics`, `stats`, and
`utils`. Ridge regression with ABCs requires `glmnet` with at least
version 4.0, while lasso regression with ABCs requires `genlasso` with
at least version 1.6.1.

`lmabc` should work with any recent version of R, though it has been
tested exclusively with versions after 4.0.0. No additional hardware is
required to run `lmabc`.

## References

Kowal, D. (2024). Regression with race-modifiers: towards equity and
interpretability. <https://doi.org/10.1101/2024.01.04.23300033>
