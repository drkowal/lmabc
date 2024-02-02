### Overview
Regression analysis commonly features categorical covariates, such as race, sex, other protected groups (national origin, religion, etc.), and many other examples. These variables are often interacted with other (continuous or categorical) variables, for example to infer race-specific effects of a covariate `x` on the response `y`. However, default numerical encodings of categorical variables suffer from alarming (racial) biases, statistical inefficiencies, and limited interpretability. 

`lmabc` addresses each of these problems, outlined below. `lmabc` provides estimation, inference, and prediction for linear regression with categorical covariates and interactions, including methods for penalized (lasso, ridge, etc.) and generalized (logistic, Poisson, etc.) regression. For ease of use, `lmabc` matches the functionality of `lm` and `glm`. 

### Default strategies: the problem

The predominant strategy for linear regression with categorical covariates is *reference group encoding* (RGE), which parametrizes and estimates the regression coefficients relative to a pre-selected reference category. For illustration, consider `race`, where the usual reference group is non-Hispanic White (NHW). This leads to several serious problems:

1. **Presentation (racial) bias**: RGE elevates a single race group (NHW) above others. All other race-specific effects are presented relative to NHW, which implicitly frames one race group as "normal" and the others as "deviations from normal". In all major statistical software (R, SAS, Python,  MATLAB, Stata, etc.), the regression output for models of the form `y ~ x + race + x:race`---used to estimate race-specific `x` effects---presents the `x` effect without explicitly acknowledging that it actually refers to the `x` effect *for the NHW group*. A similar problem occurs for the intercept. Beyond the clear inequities, this also can lead to misleading conclusions about the `x` effect for the broader population.

2. **Statistical (racial) bias**: Statistical and machine learning commonly use regularization strategies, including penalized regression, variable selection, and Bayesian inference, to stabilize (i.e., reduce the variance of) estimators by "shrinking" coefficients toward zero. With RGE, the implication is that race-specific `x` effects are *racially biased* toward the NHW `x` effect. This bias also attenuates the estimated differences between each race-specific `x` effect and the NHW `x` effect, which undermines the ability to detect race-specific differences. 

3. **Statistical inefficiencies**: Regardless of the categorical variable, adding interaction effects with another covariate (e.g., `x:race`) changes the estimates *and* standard errors for the main `x` effect. As such, analysts may be reluctant to include interaction effects, especially if they lead to reductions in statistical power.

4. **Limited interpretability**: Since RGE identifies each categorical covariate with a reference group, it becomes difficult to interpret the parameters, especially with multiple categorical covariates and interactions. Even the simple model `y ~ x + race + x:race` does not yield an obvious main (and appropriately global) `x` effect, since the coefficient on `x` corresponds to the `x` effect for the NHW group. This issue compounds for multiple categorical covariates and interactions.  

These problems are not solved by changing the reference category. Other strategies, like sum-to-zero constraints, can address 1-2 but fail to address 3-4. Omitting constraints entirely is not feasible without some regularization, but regardless cannot solve 3-4 (and, for lasso estimation, is known to reproduce RGE, thus 1-2 resurface).  

These problems persist for any categorical covariates, not just race. 

### ABCs: the solution
`lmabc` resolves each of these problems for linear regression with categorical covariates. Using Abundance-Based Constraints (ABCs), `lmabc` includes estimation, inference, and prediction for penalized (lasso, ridge, etc.) and generalized (logistic, Poisson, etc.) regression with three key features, called "the EEI of ABCs": 

1. **Equitability**: The (racial) presentation biases and (racial) statistical biases are both eliminated. For models like `y ~ x + race + x:race`, the main `x` effect is parametrized and estimated as a "race-averaged" `x` effect. No single race group is elevated. Instead, all race-specific `x` effects are presented relative to the global (i.e., "race-averaged") `x` effect. This also resolves the (racial) biases in regularized estimation: shrinkage is toward an appropriately global `x` effect, not the NHW `x` effect, with meaningful and equitable notions of sparsity. 

2. **Efficiency**: Remarkably, ABCs ensure that the estimated main `x` effects for `y ~ x + race` and `y ~ x + race + x:race` are *identical* (when `x` is also categorical; they are *nearly identical* when `x` is continuous). If the `x:race` interaction effect is small, then the standard errors for `x` are also (nearly) identical between the two models. Finally, when the `x:race` interaction effect is large, then the standard errors for `x` *decrease* for the model that includes `x:race`. With ABCs, including the interaction term `x:race` has no negative consequences: the main effect estimates are (nearly) unchanged and the standard errors are either (nearly) unchanged or smaller. 

3. **Interpretability**: The `x` and `x:race` coefficients are parametrized as "race-averaged" `x`-effects and "race-specific deviations", respectively. This, coupled with the aforementioned invariance properties for estimation and inference, enables straightforward interpretations of both main and interaction effects with ABCs.  

While the benefits of equitability and interpretability are self-evident, we also emphasize the importance of statistical efficiency. For an analyst considering "main-only" models of the form `y ~ x + race`, ABCs allow the addition of interaction effects `x:race` "for free": they have (almost) no impact on estimation and inference for the main `x` effect---unless the `x:race` interaction effect is strong, in which case the analyst gains *more power* for the main `x` effect. Yet by including the `x:race` term, the analyst can now investigate race-specific `x` effects, again without negative consequences for the main effects. This is usually *not* the case for regression analysis, and does not occur for RGE (or other approaches).  

These benefits apply to any categorical covariates, not just race, and to generalizations that include multiple continuous and categorical covariates and interactions. 

### Installation
`lmabc` can be installed and loaded as follows:

```
remotes::install_github("prayaggordy/lmabc")

library("lmabc")
```

### Future work
Users can develop their own ABCs-inspired methods using the `getConstraints()` and `getFullDesign()` methods in this package. Please email the package maintainer with any issues or questions.

### References
Kowal, D. (2024). Regression with race-modifiers: towards equity and interpretability. <https://www.medrxiv.org/content/10.1101/2024.01.04.23300033v1>
