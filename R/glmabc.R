#' Fitting Generalized Linear Models with Abundance-Based Constraints (ABCs)
#'
#' `glmabc` is used to fit generalized linear models using abundance-based constraints (ABCs).
#' Like [`stats::glm`], the model is specified by giving a symbolic description of the linear predictor
#' and a description of the error distribution. ABCs provide more equitable and interpretable output
#' for regression models with categorical covariates.
#
#'
#' @inheritParams stats::glm
#'
#' @param data a data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param cprobs a named list with an entry for each named categorical variable in the model, specifying the probabilities of each category.
#'
#' @details
#'
#' # Details
#'
#' A `glmabc` model is specified identically to the corresponding `glm` model.
#' At this time, `glmabc` only supports a single response variable, and the data must be passed into the `data` parameter.
#'
#' # Differences from `glm`
#'
#' The default approach for generalized linear regression with categorical covariates
#' is reference group encoding (RGE): one category is selected as the "reference group"
#' for each categorical variable, and all results are presented
#' relative to this group. This output is inequitable and can be
#' misleading, especially for categorical covariates like race/ethnicity,
#' gender identity, religion, national origin, etc.
#'
#' For example, suppose an analyst fits a generalized linear model of the form \code{y ~ x + race + x:race},
#' where \code{x} is a continuous covariate. This model allows for race-specific
#' coefficients on \eqn{x}. However, RGE requires a reference group, typically
#' White (or selected alphabetically), and the `glm` output is presented relative to this group:
#' \code{(Intercept)} refers to the intercept for the White group and
#' \code{x} refers to the coefficients on \eqn{x} for the White group. The remaining
#' race-specific parameters are all presented as differences relative to the White group.
#' Clearly, this output is inequitable: it elevates one group (White) above others,
#' and presents this group as "normal" while others are "deviations from normal".
#' The output is also unclear: that the \code{(Intercept)} and \code{x} effects
#' refer *only* to the reference (White) group is nowhere mentioned in any default
#' output.
#'
#' `glmabc` addresses these issues. ABCs parametrize the regression model so that the main effect terms, here
#' \code{(Intercept)} and \code{x}, are *averages* of the race-specific terms.
#' For instance, the coefficients on \eqn{x} represent the race-averaged effect
#' of \eqn{x}, and the race-specific coefficients on \eqn{x} represent deviations
#' from the average, rather than from the reference (White) group. The
#' notion of "average" derives from the argument \code{cprob}: these can be
#' input by the user (e.g., population proportions), otherwise
#' they will be set to the sample proportions for each group.
#'
#' # Value
#'
#' `glmabc` returns an object of classes "glmabc" and "lmabc."
#' Many generics commonly used for `glm` objects have been implemented for `glmabc`:
#' `summary`, `coefficients`, `plot`, `predict`, and more.
#' See the DESCRIPTION file for all implemented S3 methods.
#'
#' @seealso [stats::glm()] for the standard generalized linear regression implementation in R.
#'
#' @examples
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' fit <- glmabc(am ~ mpg + cyl + mpg:cyl, family = "binomial", data = mtcars)
#' summary(fit)
#'
#' predict(fit, newdata = data.frame(mpg = 21, cyl = "6"), type = 'response')
#'
#' @export
glmabc = function(formula, family = stats::gaussian, data, ..., cprobs = NULL){

	# Usual glm fit: this is a nice baseline
	fit0 = stats::glm(formula = formula,
										family = family,
										data = data, ...)

	# Check:
	if(any(is.na(coef(fit0)))){
		stop('NAs found in the OLS estimators;
       consider a different formula statement')
	}

	# Compute the constraint matrix:
	Con = getConstraints(formula, data, cprobs = cprobs)

	if(is.null(Con)){
		# No categorical variables, so no constraints
		return(fit0) # return the lm object
	} else {
		# Incorporate the constraints

		# Compute the full design matrix:
		X = getFullDesign(formula = formula,
											data = data,
											center = TRUE) #CHANGED TO BE CENTERED
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
		fit_con = stats::glm(stats::update(formula, ~ Xuse - 1),
												 family = family,
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
		if(mean((stats::fitted(fit_con) - stats::fitted(fit0))^2) > tol){
			stop('Fitted values are not the same;
         may be an issue with the constraint matrix')
		}
		#sigma_hat = summary(fit_con)$sigma # error SD
		#^this is not going to work for glm, summary.glm does not have a sigma component

		# New class:
		fit = fit0;  attr(fit, 'class') = c('glmabc', 'lmabc')
		fit$call = match.call()  # store the function call
		fit$glm = fit0 #  store the original object
		fit$X = X # store the full design matrix
		fit$Con = Con # store the constraint matrix
		fit$coefficients = beta_con # coefficient estimates
		fit$cov.unscaled = cov.unscaled_con # covariance matrix
		#fit$sigma = sigma_hat # estimated standard deviation
		# fit$residuals # already there
		#ADD: call

		return(fit) # return the glmabc object
	}
}
