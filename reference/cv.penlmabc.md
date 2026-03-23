# Fitting Lasso/Ridge Regression with Abundance-Based Constraints (ABCs)

`cv.penlmabc` fits penalized (lasso or ridge) linear models using
abundance-based constraints (ABCs). For penalized regression with
categorical covariates, ABCs eliminate harmful biases (e.g., with
respect to race, sex, religion, etc.), provide more meaningful notions
of sparsity, and improve interpretability.

## Usage

``` r
cv.penlmabc(
  formula,
  data,
  type = "lasso",
  lambda_path = NULL,
  K = 10,
  props = NULL,
  plot = FALSE
)
```

## Arguments

- formula:

  an object of class
  "[`formula()`](https://rdrr.io/r/stats/formula.html)" (or one that can
  be coerced to that class); a symbolic description of the model to be
  fitted.

- data:

  an optional data frame (or object coercible by `as.data.frame` to a
  data frame) containing the variables in the model.

- type:

  either "lasso" or "ridge"

- lambda_path:

  optional vector of tuning parameters; defaults are inherited from
  `glmnet` (for ridge) or `genlasso` (for lasso)

- K:

  number of folds for cross-validation; default is 10

- props:

  an optional named list with an entry for each named categorical
  variable in the model, specifying the proportions of each category. By
  default, `props` will be calculated from the empirical proportions in
  the data.

- plot:

  logical; if TRUE, include a plot of the cross-validated MSE across
  `lambda_path` values

## Details

`cv.penlmabc` solves the penalized least squares problem

\\\|\| y - X\beta\|\|^2 + \lambda \sum_j \|\beta_j\|^\gamma\\

for lasso (\\\gamma=1\\) or ridge (\\\gamma=2\\) regression, and
specifically using ABCs for categorical covariates and interactions.

Default strategies for categorical covariates typically use reference
group encoding (RGE), which removes the coefficient for one group for
each categorical variable (and their interactions). However, because
lasso and ridge shrink coefficients toward zero, this implies that all
other coefficients are *biased* toward their reference group. Such bias
is clearly problematic for variables such as race, sex, and other
protected groups, but also it attenuates the estimated differences among
groups, which can obscure important group-specific effects (e.g., for
`x:race`). The penalized estimates and predictions under RGE are
dependent on the choice of the reference group.

Alternatively, it is possible to fit penalized least squares with an
overparametrized model, i.e., without deleting a reference group (or
applying any types of constraints). However, the parameters are not
identifiable and thus not interpretable in general. With lasso
estimation, this approach empirically tends to select a reference group,
and therefore suffers from the same significant problems as RGE.

Instead, ABCs provide a parametrization of the main effects as
"group-averaged" effects, with interaction terms (e.g., `x:race`) as
"group-specific deviations". These estimators are not biased toward any
single group and the predictive performance does not depend on the
choice of a reference group. ABCs provide appealing estimation
invariance properties for models with or without interactions (e.g.,
`x:race`), and therefore offer a natural parametrization for sparse
(e.g., lasso) estimation with categorical covariates and their
interactions.

## Value

`cv.penlmabc` returns a list with the following elements:

- `coefficients` estimated coefficients at each tuning parameter in
  `lambda_path`

- `lambda_path` vector of tuning parameters

- `df` degrees of freedom at each tuning parameter in `lambda_path`

- `mse` cross-validated mean squared error (MSE) at each tuning
  parameter in `lambda_path`

- `se` standard error of the CV-MSE at each tuning parameter in
  `lambda_path`

- `ind.min` index of the minimum CV-MSE in `lambda_path`

- `ind.1se` index of the one-standard error rule in `lambda_path`

- `lambda.min` tuning parameter that achieves the minimum CV-MSE

- `lambda.1se` tuning parameter that achieves the one-standard error
  rule

## Examples

``` r
# Example lasso fit:
fit <- cv.penlmabc(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
names(fit)
#> [1] "coefficients" "lambda_path"  "df"           "mse"          "se"          
#> [6] "ind.min"      "ind.1se"      "lambda.min"   "lambda.1se"  

# Estimated coefficients at the one-standard error rule:
coef(fit)[,fit$ind.1se]
#>                    (Intercept)                   Petal.Length 
#>                      5.4684391                      0.5527580 
#>                  Speciessetosa              Speciesversicolor 
#>                      0.0000000                      0.0000000 
#>               Speciesvirginica     Petal.Length:Speciessetosa 
#>                      0.0000000                     -0.3454960 
#> Petal.Length:Speciesversicolor  Petal.Length:Speciesvirginica 
#>                      0.2737565                      0.0717395 
```
