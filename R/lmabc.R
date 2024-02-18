#' Fitting Linear Models with Abundance-Based Constraints (ABCs)
#'
#' `lmabc` is used to fit linear models using abundance-based constraints (ABCs).
#' For regression models with categorical covariates, ABCs
#' provide 1) more equitable output, 2) better statistical efficiency,
#' and 3) more interpretable parameters, especially in the
#' presence of interaction (or modifier) effects.
#'
#' @inheritParams stats::lm
#' @param formula an object of class "[formula()]" (or one that can be coerced to that class); a symbolic description of the model to be fitted.
#' @param data a data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param props an optional named list with an entry for each named categorical variable in the model, specifying the proportions of each category. By default, `props` will be calculated from the empirical proportions in the data.
#'
#' @details
#'
#' # Details
#'
#' An `lmabc` model is specified identically to the corresponding `lm` model.
#' At this time, `lmabc` only supports a single response variable, and the data must be passed into the `data` parameter.
#'
#' # Differences from `lm`
#'
#' The default approach for linear regression with categorical covariates
#' is reference group encoding (RGE): one category is selected as the "reference group"
#' for each categorical variable, and all results are presented
#' relative to this group. However, this approach produces output that is
#' inequitable (and potentially misleading), estimators that are statistically inefficient,
#' and model parameters that are difficult to interpret.
#'
#' For example, suppose an analyst fits a model of the form `y ~ x + race + x:race`,
#' where `x` is a continuous covariate. This model allows for `race`-specific
#' coefficients on `x` (or slopes). However, RGE requires a reference group;
#' for `race`, this is typically non-Hispanic White (NHW). This creates several problems:
#' 1. All output is presented relative to the reference (NHW) group:
#' the results for `(Intercept)` and `x` refer to the intercept and slope, respectively,
#' *for the reference (NHW) group*. This can lead to misleading conclusions
#' about the overall `x` effect, since the dependence on the reference
#' group is not made clear. All other `race` and `x:race` effects
#' are presented relative to the reference (NHW) group, which
#' elevates this group above all others.
#' 2. Compared to the main-only model `y ~ x + race`, adding the
#' `x:race` interaction alters the estimates and standard errors of
#' the `x` effect. This typically results in a loss of statistical power:
#'  the `x` effect now refers to a subset of the data (the reference group).
#' 3. Since all categorical covariates and interactions are
#' anchored at a reference group, it becomes increasingly
#' difficult to interpret the model parameters in the
#' presence of multiple categorical variables (`race`, `sex`, etc.)
#' and interactions.
#'
#' `lmabc` addresses these issues. ABCs parametrize the regression model so that the main effect terms, here
#' `(Intercept)` and `x`, are *averages* of the race-specific terms.
#' The notion of "average" derives from the argument `cprob`: these can be
#' input by the user (e.g., population proportions), otherwise
#' they will be set to the sample proportions for each group. ABCs provide
#' several key advantages:
#' 1. **Equitability**: the main `x` effect is
#' parameterized as "group-averaged" effect. It does not
#' elevate any single group (e.g., NHW). All other group-specific effects
#' are relative to this global term, rather than any
#' single group.
#' 2. **Efficiency**: comparing the main-only model `y ~ x + race`
#' with the race-modified model `y ~ x + race + x:race`, ABCs
#' (with the default `props`) ensure that the main `x` effect estimates
#' are (nearly) unchanged and the standard errors are (nearly) unchanged
#' or smaller. Remarkably, there are no negative (statistical) consequences for
#' including the interaction `x:race`, even if it is irrelevant.
#' 3. **Interpretability**: The `x` and `x:race` coefficients are
#' "group-averaged" `x`-effects and "group-specific deviations", respectively.
#' Coupled with the ABCs estimation/inference invariance, this
#' yields simple interpretations of main and interaction effects.
#'
#' # Similarities to `lm`
#' `lmabc` is a reparametrization of the linear model,
#' but the fitted values, predictions, and residuals will be the same
#' as `lm` (see [cv.penlmabc()] for an example where this is no longer the case).
#' Without categorical covariates, `lmabc` output will be
#' identical to `lm`.
#'
#' # Value
#'
#' `lmabc` returns an object of class "lmabc." Many generics commonly used for `lm` objects have been implemented for `lmabc`: `summary`, `coefficients`, `plot`, `predict`, and more. See the DESCRIPTION file for all implemented S3 methods.
#'
#' @seealso [lm()] for the standard linear regression implementation in R.
#'
#' @examples
#' fit <- lmabc(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
#' summary(fit)
#'
#' predict(fit, newdata = data.frame(Petal.Length = 1.5, Species = "setosa"))
#'
#' @export
lmabc = function(formula, data, ..., props = NULL){

	# Usual fit: this is a nice baseline
	fit0 = lm(formula = formula,
						data = data, ...)

	# Check:
	if(any(is.na(coef(fit0)))){
		stop('NAs found in the OLS estimators;
       consider a different formula statement')
	}

	# Compute the constraint matrix:
	Con = getConstraints(formula, data, props = props)

	if(is.null(Con)){
		# No categorical variables, so no constraints
		# Get fit0 version of X, Con, beta_con, cov.unscaled_con, sigma_hat
		y <- data[[formula[[2]]]]
		X <- getFullDesign(formula = formula,
											 data = data,
											 center = TRUE)[,-1, drop = FALSE]

		fit0_centered <- lm(y ~ X)
		beta_con <- coef(fit0_centered)
		cov.unscaled_con <- vcov(fit0_centered)
		sigma_hat <- sigma(fit0_centered)
	} else {
		# Incorporate the constraints

		# Compute the full design matrix:
		X = getFullDesign(formula = formula,
											data = data,
											center = TRUE)
		xnames = colnames(X) # variable names

		# Number of constraints
		m = nrow(Con)

		# QR decomposition to incorporate the constraints
		Qm = qr.Q(qr(t(Con)),
							complete =  TRUE)[,-(1:m)] # final (p-m) columns of Q

		# Covariate matrix to be used in the *unconstrained* model
		data$Xuse = X%*%Qm

		# Note: this is more efficient, but more cumbersome
		# cQR = qr(t(Con)) # QR decomposition
		# Xuse = t(qr.qty(cQR, t(X))[-(1:m),])

		# Fitted model in the *unconstrained* (lower-dim) space
		fit_con = lm(stats::update(formula, ~ Xuse - 1),
								 data = data, ...)

		#y = model.frame(fit0)[,1]
		#fit_con = lm(y ~ Xuse - 1)

		# Unconstrained (lower-dim) coefficients:
		beta_0 = coef(fit_con)

		# Constrained coefs:
		beta_con = as.numeric(Qm%*%beta_0);
		names(beta_con) = xnames

		# Unscaled covariance matrix:
		#cov.unscaled_con = Qm%*%solve(crossprod(Xuse))%*%t(Qm)
		#cov.unscaled_con = crossprod(forwardsolve(t(chol(crossprod(Xuse))), t(Qm)))
		cov.unscaled_con = crossprod(t(Qm),
																 tcrossprod(summary(fit_con)$cov.unscaled,
																 					 Qm))
		colnames(cov.unscaled_con) = rownames(cov.unscaled_con) = xnames

		# Check fitted values to make sure the fits are identical:
		tol = 10^-8 # tolerance
		if(mean((stats::fitted(fit_con) - stats::fitted(fit0))^2)
			 + mean((X%*%beta_con - stats::fitted(fit0))^2) > tol){
			stop('Fitted values are not the same;
         may be an issue with the constraint matrix')
		}
		sigma_hat = summary(fit_con)$sigma # error SD
	}

	# New class:
	fit = fit0;  attr(fit, 'class') = 'lmabc'
	fit$call = match.call()  # store the function call
	fit$lm = fit0 #  store the original object
	fit$X = X # store the full design matrix
	fit$Con = Con # store the constraint matrix
	fit$coefficients = beta_con # coefficient estimates
	fit$cov.unscaled = cov.unscaled_con # covariance matrix
	fit$sigma = sigma_hat # estimated standard deviation

	return(fit) # return the lmabc object
}
