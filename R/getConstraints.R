construct_empty_con <- function(formula, data) {
	ter <- terms(formula)

	mf = stats::model.frame(formula = formula,
													data = data)

	# Names of the variables involved:
	vnames = attr(mf, 'names')

	# Covariates:
	covar = attr(ter, "term.labels")

	# Subset to the variables included in the model:
	# should be able to remove this once we merge optional-data-arg
	data = data[,vnames]

	# Handle character variables:
	data = as.data.frame(lapply(data, function(k) if (is.character(k)) as.factor(k) else k))

	# Handle the categorical variables:
	f_inds = which(sapply(data, is.factor))

	if (length(f_inds) == 0) {
		return(NULL)
	}

	factors <- attr(ter, "factors")

	# add a check: each non-interaction column sums to 1, each interaction column sums to 2, all column values are 1 (ensures main effects are present)

	Con_rows <- unlist(
		lapply(names(f_inds),
					 function(i) {
					 	n <- sum(factors[i,])
					 	if (n == 1) {
					 		i
					 	} else {
					 		paste0(i, seq(n))
					 	}
					 })
	)

	factor_names <- lapply(names(f_inds),
												 function(i) {

												 })

	# find interactions with at least one cat variable
	interaction_names <- attr(ter, "term.labels")[which(attr(ter, "order") == 2)]
	interactions <- lapply(interaction_names,
												 function(nam) {
												 	rnames <- names(which(factors[, nam] == 1))

												 	if (length(intersect(rnames, names(f_inds))) == 0) {
												 		return(NULL)
												 	}

												 	if (length)

												 	rnames
												 })
	names(interactions) <- interaction_names
	interactions <- Filter(Negate(is.null), interactions)

	interactions
}



#' Generating the Constraint Matrix for Abundance-Based Constraints (ABCs)
#'
#' `getConstraints` is used to generate the constraint matrix for linear regression with ABCs.
#'
#' @param formula an object of class "[formula()]" (or one that can be coerced to that class); a symbolic description of the model to be fitted.
#' @param data a data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param props an optional named list with an entry for each named categorical variable in the model, specifying the proportions of each category. By default, `props` will be calculated from the empirical proportions in the data.
#'
#' @details
#'
#' # Details
#'
#' For linear regression with categorical covariates (and interactions, etc.),
#' constraints are needed for model identifiability. `getConstraints()` incorporates
#' all the necessary constraints for a given `formula` statement within a single matrix.
#'
#' `props` must include every level for all categorical covariates and all interactions that include at least one categorical covariate.
#' It should be a named list of named vectors.
#' It provides several useful options, including
#' alternatives to ABCs:
#' \itemize{
#' \item ABCs using the sample proportions of each category (default);
#' \item ABCs using population proportions of each category (if known);
#' \item Reference group encoding (RGE), which fixes a reference
#' category proportion at 1 and all others at 0 (default in `lm()`);
#' \item Sum-to-zero (STZ) constraints, which use the same
#' proportions for every category level.
#' }
#'
#' This method is called by [lmabc()].
#'
#' # Value
#'
#' `getConstraints` returns a matrix of constraints based on the given model `formula`.
#'
#' @seealso [lmabc()] for a use case of `getConstraints`.
#'
#' @examples
#' getConstraints(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
#'
#' @export
getConstraints = function(formula, data, props = NULL){

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

	if(length(f_inds) > 0){ # if cat variables, need constrains (o/w NULL)
		cdat = data.frame(data[,f_inds]) # data frame
		cnames = names(cdat) = names(data)[f_inds] # correct the names

		# Compute the full design matrix:
		X = getFullDesign(formula = formula,
											data = data,
											center = TRUE)
		xnames = colnames(X) # variable names

		# Compute the ABCs:
		if(is.null(props)){
			# Categorical proportions:
			pi_hat = lapply(cdat, function(k){
				sapply(levels(k), function(g) mean(k==g))}
			)
		} else {
			# Check that props is proper:
			if(any(is.na(match(cnames, names(props))))){
				stop('props must be a named list with an entry for each named categorical variable in the model')
			}
			# Match with the correct variable names:
			pi_hat = lapply(cnames, function(cn)
				props[[match(cn, names(props))]])
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
					#Con1[k,inds_kl] = pi_hat[[v1]]
					Con1[k,inds_kl] = colMeans(X[,inds_kl]) # joint probs (FIXME: generalize!)
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
					#Con2[k,inds_kl] = pi_hat[[v2]]
					Con2[k,inds_kl] = colMeans(X[,inds_kl]) # joint probs (FIXME: generalize!)
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
