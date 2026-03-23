# Generating the Constraint Matrix for Abundance-Based Constraints (ABCs)

`getConstraints` is used to generate the constraint matrix for linear
regression with ABCs.

## Usage

``` r
getConstraints(formula, data, props = NULL)
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

- props:

  an optional named list with an entry for each named categorical
  variable in the model, specifying the proportions of each category. By
  default, `props` will be calculated from the empirical proportions in
  the data.

## Details

For linear regression with categorical covariates (and interactions,
etc.), constraints are needed for model identifiability.
`getConstraints()` incorporates all the necessary constraints for a
given `formula` statement within a single matrix.

`props` must include every level for all categorical covariates and all
interactions that include at least one categorical covariate. It should
be a named list of named vectors. It provides several useful options,
including alternatives to ABCs:

- ABCs using the sample proportions of each category (default);

- ABCs using population proportions of each category (if known);

- Reference group encoding (RGE), which fixes a reference category
  proportion at 1 and all others at 0 (default in
  [`lm()`](https://rdrr.io/r/stats/lm.html));

- Sum-to-zero (STZ) constraints, which use the same proportions for
  every category level.

This method is called by
[`lmabc()`](https://drkowal.github.io/lmabc/reference/lmabc.md).

## Value

`getConstraints` returns a matrix of constraints based on the given
model `formula`.

## See also

[`lmabc()`](https://drkowal.github.io/lmabc/reference/lmabc.md) for a
use case of `getConstraints`.

## Examples

``` r
getConstraints(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
#>          (Intercept) Petal.Length Speciessetosa Speciesversicolor
#> Species1           0            0     0.3333333         0.3333333
#> Species2           0            0     0.0000000         0.0000000
#>          Speciesvirginica Petal.Length:Speciessetosa
#> Species1        0.3333333                  0.0000000
#> Species2        0.0000000                  0.3333333
#>          Petal.Length:Speciesversicolor Petal.Length:Speciesvirginica
#> Species1                      0.0000000                     0.0000000
#> Species2                      0.3333333                     0.3333333
#> attr(,"pi_hat")
#> attr(,"pi_hat")$Species
#>     setosa versicolor  virginica 
#>  0.3333333  0.3333333  0.3333333 
#> 
```
