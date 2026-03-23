# Wrapper for model.frame

`model.frame` returns a data.frame object for use in
[`lmabc()`](https://drkowal.github.io/lmabc/reference/lmabc.md) and
similar functions. It reimplements the first few lines of
[`lm()`](https://rdrr.io/r/stats/lm.html) to properly format the
`formula` and `data` parameters. If only `formula` is passed, all
variables must be vectors in the environment. If `formula` and `data`
are passed, variables are first pulled from `data`, then filled from the
environment.

## Usage

``` r
model_frame(formula, data, ...)
```

## Arguments

- formula:

  formula object

- data:

  data.frame object

- ...:

  other parameters to fill in the call

## Value

a data.frame with the variables in `formula`
