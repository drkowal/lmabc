#' Title
#'
#' @param formula
#' @param data
#' @param center
#'
#' @return
#' @export
#'
#' @examples
getFullDesign = function(formula, data, center = TRUE){

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
