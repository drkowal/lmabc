#' Generating the Full Design Matrix
#'
#' `getFullDesign` is used to generate the design matrix for linear regression with ABCs.
#'
#' @param formula an object of class "[formula()]" (or one that can be coerced to that class); a symbolic description of the model to be fitted.
#' @param data an optional data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param center Boolean, whether to center continuous predictors. `TRUE` by default for `lmabc`.
#'
#' @details
#'
#' # Details
#'
#' The full design matrix is similar to the design matrix in standard linear regression. However, instead of removing one level from each categorical variable, `getFullDesign` retains all levels by manipulating contrast matrices.
#'
#' The full design matrix is used with the constraint matrix from [getConstraints()] to fit a model with the ABCs.
#'
#' This method is called by [lmabc()]. This method is useful for implementing additional variations of the ABCs.
#'
#' # Value
#'
#' `getFullDesign` returns a matrix representing the full design matrix for a particular regression model.
#'
#' @seealso [lmabc()] for a use case of `getFullDesign`.
#'
#' @examples
#' # full design matrix
#' getFullDesign(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
#'
#' # lm's default design matrix, which removes a reference category
#' model.matrix(lm(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris))
#'
#' @export
getFullDesign = function(formula, data, center = TRUE){

	# Fill in the data argument with stats::model.frame
	data <- model_frame(formula = formula, data = data)

	# Model frame has some useful information
	mf = stats::model.frame(formula = formula,
													data  = data)

	# Names of the variables involved:
	vnames = attr(mf, 'names')

	# Covariates:
	covar = attr(attr(mf, 'terms'),
							 "term.labels")

	# Subset to the variables included in the model:
	data = data[,vnames]

	# Handle character variables:
	data = as.data.frame(lapply(data, function(k) if (is.character(k)) as.factor(k) else k))

	# If specified, center the non-categorical variables
	# note: do this *before* computing the interactions
	if(center){
		n_inds = which(sapply(data, is.numeric)) # numeric variables
		n_inds = n_inds[-1] # remove the response
		if(length(n_inds) > 0){ # if we have any numeric covariates...
			dnames = names(data)[n_inds] # store the names
			data[,n_inds] = data.frame(apply(data.frame(data[,n_inds]), 2,
																			 function(d) d - mean(d, na.rm=TRUE))) # extra data.frame() statements in case there is only one variable
			names(data)[n_inds] = dnames # restore the names
		}
	}

	# Handle the categorical variables:
	f_inds = which(sapply(data, is.factor)) # factor indices

	if(length(f_inds) > 0){
		cdat = data.frame(data[,f_inds]) # data frame
		cnames = names(cdat) = names(data)[f_inds] # correct the names

		# Form the FULL (overparametrized) design  matrix:
		# Note: same order as covar!
		X = stats::model.matrix(object = formula,
														data = data,
														contrasts.arg =
															lapply(cdat, stats::contrasts, contrasts=FALSE))
	} else {
		# No categorical variables, so just return the usual value:
		X = stats::model.matrix(object = formula,
														data = data)
	}

	return(X)
}
