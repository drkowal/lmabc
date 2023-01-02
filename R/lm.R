#' Fitting Linear Models with Abundance-Based Constraints
#'
#' `lm_abc` is used to fit linear models using abundance-based constraints.
#'
#' @inheritParams stats::lm
#'
#' @param data a data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param cprobs a named list with an entry for each named categorical variable in the model, specifying the probabilities of each category.
#'
#' @details
#'
#' # Details
#'
#' An `lmabc` model is specified identically to the corresponding `lm` model. At this time, `lmabc` only supports a single response variable, and the data must be passed into the `data` parameter.
#'
#' # Differences from `lm`
#'
#' Standard linear regression models chose one level for each categorical factor and make it a "baseline," which is necessary to not over-parameterize the model. The coefficients on the continuous \eqn{X} variables are specifically for the baseline category and do not represent a global effect. Similarly, the coefficients on the non-baseline levels are in comparison to the baseline level.
#'
#' This framework can lead to biases in interpretation. Suppose a researcher includes `age` and `race` as explanatory variables. `lm` will report the coefficient for `age`, which is really the baseline-specific coefficient for `age`; the baseline is often chosen to be the most prevalent category, often non-Hispanic White (NHW). Thus, the coefficient for NHW is implicitly represented as the global effect. The coefficient for any other race dummy is the expected change in the \eqn{Y} variable compared to the baseline—again, not the true effect of identifying as that race.
#'
#' `lmabc` introduces abundance-based constraints (ABCs). Each "baseline" coefficient now represents the group-averaged coefficient with weights given by the sample proportions or by `cprobs`. That is, the coefficient on `age` is the weighted average of each race-specific effect.
#'
#' # Value
#'
#' `lmabc` returns an object of class "lmabc." Many generics commonly used for `lm` objects have been implemented for `lmabc`: `summary`, `coefficients`, `plot`, `predict`, and more. See the DESCRIPTION file for all implemented S3 methods.
#'
#' @seealso [stats::lm()] for the standard linear regression implementation in R.
#'
#' @examples
#' fit <- lm_abc(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
#' summary(fit)
#'
#' predict(fit, newdata = data.frame(Petal.Length = 1.5, Species = "setosa"))
#'
#' @export
lm_abc = function(formula, data, ..., cprobs = NULL){

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
		return(fit0) # return the lm object
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

		# New class:
		fit = fit0;  attr(fit, 'class') = 'lmabc'
		fit$call = match.call()  # store the function call
		fit$lm = fit0 #  store the original object
		fit$X = X # store the full design matrix
		fit$Con = Con # store the constraint matrix
		fit$coefficients = beta_con # coefficient estimates
		fit$cov.unscaled = cov.unscaled_con # covariance matrix
		fit$sigma = sigma_hat # estimated standard deviation
		# fit$residuals # already there
		#add call


		return(fit) # return the lmabc object
	}

}

#' @export
getConstraints = function(formula, data, cprobs = NULL){

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

	# Handle the categorical variables:
	f_inds = which(sapply(data, is.factor)) # factor indices

	if(length(f_inds) > 0){
		cdat = data.frame(data[,f_inds]) # data frame
		cnames = names(cdat) = names(data)[f_inds] # correct the names

		# Compute the full design matrix:
		X = getFullDesign(formula = formula,
											data = data,
											center = TRUE)
		xnames = colnames(X) # variable names

		# Compute the ABCs:
		if(is.null(cprobs)){
			# Categorical proportions:
			pi_hat = lapply(cdat, function(k){
				sapply(levels(k), function(g) mean(k==g))}
			)
		} else {
			# Check that cprobs is proper:
			if(any(is.na(match(cnames, names(cprobs))))){
				stop('cprobs must be a named list with an entry for each named categorical variable in the model')
			}
			# Match with the correct variable names:
			pi_hat = lapply(cnames, function(cn)
				cprobs[[match(cn, names(cprobs))]])
			names(pi_hat) = cnames
		}

		# One more check:
		if(any(unlist(lapply(pi_hat, function(p) any(p < 0))))){
			stop('Entries of cprob must be nonnegative')
		}

		# matrix of formula terms (shows interactions)
		terms_mx <- attr(terms(formula), "factors")

		# Check for categorical-categorical pairs
		# (these will be handled separately below)
		inds_catcat = match(
			sapply(cnames, function(x) paste(x,  ':', cnames[cnames!=x], sep='')),
			covar)
		inds_catcat = inds_catcat[!is.na(inds_catcat)]
		if(length(inds_catcat) > 0){
			covar_all = covar; covar = covar[-inds_catcat]
			terms_mx <- terms_mx[,-inds_catcat]
		}

		# Indices of the categorical variables/interactions w/in 'covar' (excluding cat-cat)
		c_inds = lapply(cnames, function(na) unname(which(terms_mx[na,] == 1)))
		names(c_inds) = cnames

		# Some dimensions:
		p = ncol(X) # number of covariates
		m = length(unlist(c_inds)) # number of constraints (excluding cat-cat)

		# Constraint matrix (excluding cat-cat interactions)
		Con = array(0,
								c(m, p),
								dimnames = list(names(unlist(c_inds)), xnames))

		j = 1 # index for rows (note: this is not a large loop)
		for(k in 1:length(c_inds)){ # for each categorical variable

			for(ell in 1:length(covar[c_inds[[k]]])){# for each appearance

				# Iterate through the covariates:
				v_kl = covar[c_inds[[k]]][ell]

				# Update the names to match the model.matrix labeling
				# Replace the variable name with the name *and* level
				names_kl = sapply(paste(cnames[k], names(pi_hat[[k]]), sep=''),
													function(g) gsub("(^:)|(:$)", "", gsub(paste0("(^", cnames[k], "$)|(^", cnames[k], ":)|(:", cnames[k], "$)"), paste0(":", g, ":"), v_kl)))

				# Add the constraint at the right indices:
				inds_kl = match(names_kl, xnames)
				Con[j,inds_kl] = pi_hat[[k]]

				# And increment:
				j = j+1
			}
		}

		# Now augment with the cat-cat constraints, if any:
		if(length(inds_catcat) > 0){
			Con_cat = NULL
			for(j  in inds_catcat){
				# access the two variables:
				v1 = sub(":.*", "", covar_all[j])
				v2 = sub(".*:", "", covar_all[j])

				# add the constraints for variable 1:
				Con1 = array(0, c(length(pi_hat[[v2]]), p),
										 dimnames = list(paste(v1, ":",v2, names(pi_hat[[v2]]), sep=''), xnames))
				for(k in 1:length(pi_hat[[v2]])){
					# Update the names to match the model.matrix labeling
					names_kl = paste(v1, names(pi_hat[[v1]]),
													 ":",
													 v2, names(pi_hat[[v2]])[k],
													 sep='')
					# Add the constraint at the right indices:
					inds_kl = match(names_kl, xnames)
					Con1[k,inds_kl] = pi_hat[[v1]]
				}
				# add the constraints for variable 2:
				Con2 = array(0, c(length(pi_hat[[v1]]), p),
										 dimnames = list(paste(v1, names(pi_hat[[v1]]),":",v2, sep=''), xnames))
				for(k in 1:length(pi_hat[[v1]])){
					# Update the names to match the model.matrix labeling
					names_kl = paste(v1, names(pi_hat[[v1]])[k],
													 ":",
													 v2, names(pi_hat[[v2]]),
													 sep='')
					# Add the constraint at the right indices:
					inds_kl = match(names_kl, xnames)
					Con2[k,inds_kl] = pi_hat[[v2]]
				}
				# Combine:
				Con_cat = rbind(Con_cat,
												Con1,
												Con2)

				# remove one entry (overconstrained):
				Con_cat = Con_cat[-nrow(Con_cat),]
			}
			# Update Con:
			Con = rbind(Con, Con_cat)
		}

	} else Con = NULL # no factor variables

	return(Con)
}

#' @export
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
