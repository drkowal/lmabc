# Fitting Generalized Linear Models with Abundance-Based Constraints (ABCs)

`glmabc` is used to fit generalized linear models using abundance-based
constraints (ABCs). Like
[`stats::glm`](https://rdrr.io/r/stats/glm.html), the model is specified
by giving a symbolic description of the linear predictor and a
description of the error distribution. ABCs provide more equitable and
interpretable output for regression models with categorical covariates.

## Usage

``` r
glmabc(formula, family = stats::gaussian, data, props = NULL)
```

## Arguments

- formula:

  an object of class
  `"`[`formula`](https://rdrr.io/r/stats/formula.html)`"` (or one that
  can be coerced to that class): a symbolic description of the model to
  be fitted. The details of model specification are given under
  ‘Details’.

- family:

  a description of the error distribution and link function to be used
  in the model. For `glm` this can be a character string naming a family
  function, a family function or the result of a call to a family
  function. For `glm.fit` only the third option is supported. (See
  [`family`](https://rdrr.io/r/stats/family.html) for details of family
  functions.)

- data:

  an optional data frame (or object coercible by `as.data.frame` to a
  data frame) containing the variables in the model.

- props:

  an optional named list with an entry for each named categorical
  variable in the model, specifying the proportions of each category. By
  default, `props` will be calculated from the empirical proportions in
  the data.

## Details

A `glmabc` model is specified identically to the corresponding `glm`
model. At this time, `glmabc` only supports a single response variable.

See [`lmabc()`](https://drkowal.github.io/lmabc/reference/lmabc.md) for
details and motivation about ABCs for categorical covariates. Note that
the ABCs estimation/inference invariance properties no longer apply for
GLMs, but the arguments for equitability and interpretability remain
valid.

## Value

`glmabc` returns an object of classes "glmabc" and "lmabc." Many
generics commonly used for `glm` objects have been implemented for
`glmabc`: `summary`, `coefficients`, `plot`, `predict`, and more. See
the DESCRIPTION file for all implemented S3 methods.

## See also

[`stats::glm()`](https://rdrr.io/r/stats/glm.html) for the standard
generalized linear regression implementation in R.

## Examples

``` r
mtcars$cyl <- as.factor(mtcars$cyl)
fit <- glmabc(am ~ mpg + cyl + mpg:cyl, family = "binomial", data = mtcars)
summary(fit)
#> 
#> Call:
#> glmabc(formula = am ~ mpg + cyl + mpg:cyl, family = "binomial", 
#>     data = mtcars)
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)
#> (Intercept) -1.13572    0.91699  -1.239    0.227
#> mpg          0.40528    0.24936   1.625    0.116
#> cyl4        -0.18919    1.23255  -0.153    0.879
#> cyl6         1.03642    1.14001   0.909    0.372
#> cyl8        -0.36956    1.09408  -0.338    0.738
#> mpg:cyl4     0.07703    0.31668   0.243    0.810
#> mpg:cyl6     0.57172    0.62998   0.908    0.372
#> mpg:cyl8    -0.34638    0.27347  -1.267    0.217
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 43.230  on 31  degrees of freedom
#> Residual deviance: 27.676  on 26  degrees of freedom
#> AIC: 39.676
#> 
#> Number of Fisher Scoring iterations: 6
#> 

predict(fit, newdata = data.frame(mpg = 21, cyl = "6"), type = 'response')
#>         1 
#> 0.6876515 
```
