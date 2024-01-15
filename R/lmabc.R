#' Fitting Linear Models with Abundance-Based Constraints (ABCs)
#'
#' `lmabc` is used to fit linear models using abundance-based constraints (ABCs).
#' ABCs provide more equitable and interpretable output
#' for regression models with categorical covariates.
#'
#'
#' @inheritParams stats::lm
#' @param formula an object of class "[formula()]" (or one that can be coerced to that class); a symbolic description of the model to be fitted.
#' @param data a data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param cprobs an optional named list with an entry for each named categorical variable in the model, specifying the probabilities of each category, which must sum to 1. By default, `cprobs` will be calculated from the proportions in the data.
#'
#' @details
#'
#' # Details
#'
#' An `lmabc` model is specified identically to the corresponding `lm` model. At this time, `lmabc` only supports a single response variable, and the data must be passed into the `data` parameter.
#'
#' # Differences from `lm`
#'
#' The default approach for linear regression with categorical covariates
#' is reference group encoding (RGE): one category is selected as the "reference group"
#' for each categorical variable, and all results are presented
#' relative to this group. This output is inequitable and can be
#' misleading, especially for categorical covariates like race/ethnicity,
#' gender identity, religion, national origin, etc.
#'
#' For example, suppose an analyst fits a model of the form \code{y ~ x + race + x:race},
#' where \code{x} is a continuous covariate. This model allows for race-specific
#' coefficients on \eqn{x}. However, RGE requires a reference group, typically
#' White (or selected alphabetically), and the `lm` output is presented relative to this group:
#' \code{(Intercept)} refers to the intercept for the White group and
#' \code{x} refers to the coefficients on \eqn{x} for the White group. The remaining
#' race-specific parameters are all presented as differences relative to the White group.
#' Clearly, this output is inequitable: it elevates one group (White) above others,
#' and presents this group as "normal" while others are "deviations from normal".
#' The output is also unclear: that the \code{(Intercept)} and \code{x} effects
#' refer *only* to the reference (White) group is nowhere mentioned in any default
#' output.
#'
#' `lmabc` addresses these issues. ABCs parametrize the regression model so that the main effect terms, here
#' \code{(Intercept)} and \code{x}, are *averages* of the race-specific terms.
#' For instance, the coefficients on \eqn{x} represent the race-averaged effect
#' of \eqn{x}, and the race-specific coefficients on \eqn{x} represent deviations
#' from the average, rather than from the reference (White) group. The
#' notion of "average" derives from the argument \code{cprob}: these can be
#' input by the user (e.g., population proportions), otherwise
#' they will be set to the sample proportions for each group.
#'
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
lmabc = function(formula, data, ..., cprobs = NULL){

	# Usual fit: this is a nice baseline
	fit0 = lm(formula = formula,
						data  = data, ...)

	# Check:
	if(any(is.na(coef(fit0)))){
		stop('NAs found in the OLS estimators;
       consider a different formula statement')
	}

	# Compute the constraint matrix:
	Con = getConstraints(formula, data, cprobs = cprobs)

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
