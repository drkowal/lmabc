#' Fitting Lasso/Ridge Regression with Abundance-Based Constraints (ABCs)
#'
#' `cv.penlmabc` fits penalized (lasso or ridge) linear models
#'  using abundance-based constraints (ABCs). For penalized regression
#'  with categorical covariates, ABCs eliminate harmful biases
#'  (e.g., with respect to race, sex, religion, etc.), provide
#'  more meaningful notions of sparsity, and improve interpretability.
#'
#' @param formula an object of class "[formula()]" (or one that can be coerced to that class); a symbolic description of the model to be fitted.
#' @param data an optional data frame (or object coercible by `as.data.frame` to a data frame) containing the variables in the model.
#' @param type either "lasso" or "ridge"
#' @param lambda_path optional vector of tuning parameters;
#' defaults are inherited from `glmnet` (for ridge) or `genlasso` (for lasso)
#' @param K number of folds for cross-validation; default is 10
#' @param props an optional named list with an entry for each named categorical variable in the model, specifying the proportions of each category. By default, `props` will be calculated from the empirical proportions in the data.
#' @param plot logical; if TRUE, include a plot of the cross-validated
#' MSE across `lambda_path` values
#'
#' @details
#'
#' # Details
#' `cv.penlmabc` solves the penalized least squares problem
#'
#' \eqn{|| y - X\beta||^2 + \lambda \sum_j |\beta_j|^\gamma}
#'
#' for lasso (\eqn{\gamma=1}) or ridge (\eqn{\gamma=2}) regression,
#' and specifically using ABCs for categorical covariates and interactions.
#'
#' Default strategies for categorical covariates typically
#' use reference group encoding (RGE), which removes
#' the coefficient for one group for each
#' categorical variable (and their interactions).
#' However, because lasso and ridge shrink coefficients
#' toward zero, this implies that all other coefficients are
#' *biased* toward their reference group. Such bias is
#' clearly problematic for variables such as race, sex, and other
#' protected groups, but also it attenuates the estimated differences
#' among groups, which can obscure important group-specific
#' effects (e.g., for `x:race`). The penalized estimates and predictions
#' under RGE are dependent on the choice of the reference group.
#'
#' Alternatively, it is possible to fit penalized least squares
#' with an overparametrized model, i.e., without deleting a
#' reference group (or applying any types of constraints). However,
#' the parameters are not identifiable and thus not interpretable
#' in general. With lasso estimation, this approach empirically tends
#' to select a reference group, and therefore suffers
#' from the same significant problems as RGE.
#'
#' Instead, ABCs provide a parametrization of the main
#' effects as "group-averaged" effects, with interaction terms
#' (e.g., `x:race`) as "group-specific deviations".
#' These estimators are not biased toward any single group
#' and the predictive performance does not depend on the
#' choice of a reference group. ABCs provide appealing estimation
#' invariance properties for models with or without interactions
#' (e.g., `x:race`), and therefore offer a natural parametrization
#' for sparse (e.g., lasso) estimation with categorical covariates
#' and their interactions.
#'
#' # Value
#'
#' `cv.penlmabc` returns a list with the following elements:
#' \itemize{
#'	\item `coefficients` estimated coefficients at each tuning
#'	parameter in `lambda_path`
#'	\item `lambda_path` vector of tuning parameters
#'	\item `df` degrees of freedom at each tuning
#'	parameter in `lambda_path`
#'	\item `mse` cross-validated mean squared error (MSE) at each tuning
#'	parameter in `lambda_path`
#'	\item `se` standard error of the CV-MSE at each tuning
#'	parameter in `lambda_path`
#'	\item `ind.min` index of the minimum CV-MSE in `lambda_path`
#'	\item `ind.1se` index of the one-standard error rule in `lambda_path`
#'	\item `lambda.min` tuning parameter that achieves the minimum CV-MSE
#'	\item `lambda.1se` tuning parameter that achieves the one-standard error rule
#' }
#'
#' @examples
#' # Example lasso fit:
#' fit <- cv.penlmabc(Sepal.Length ~ Petal.Length + Species + Petal.Length*Species, data = iris)
#' names(fit)
#'
#' # Estimated coefficients at the one-standard error rule:
#' coef(fit)[,fit$ind.1se]
#'
# #' @importFrom glmnet glmnet
# #' @importFrom genlasso genlasso
#' @export
cv.penlmabc = function(formula, data,
											 type = 'lasso',
											 lambda_path = NULL,
											 K = 10,
											 props = NULL,
											 plot = FALSE) {

	# Fill in the data argument with stats::model.frame
	data <- model_frame(formula = formula, data = data, props = props,
											type = type, lambda_path = lambda_path, K = K, plot = plot)

	# Quick check: ridge or lasso
	if(!(type == 'ridge' | type == 'lasso')){
		stop('type must be "ridge" or "lasso"')
	}

	# Get constraint matrix:
	Con = getConstraints(formula, data, props)
	m = nrow(Con)

	# Full design matrix:
	X = getFullDesign(formula, data)

	# QR decomposition to incorporate the constraints
	Qm = qr.Q(qr(t(Con)),
						complete =  TRUE)[,-(1:m)] # final (p-m) columns of Q

	# Covariate matrix to be used in the *unconstrained* model
	Xuse = X%*%Qm

	# Response variable:
	y = stats::model.frame(formula, data)[,1]

	# Compute the matrix to reweight by ABCs
	# (so that we apply shrinkage equally to all groups)
	penwt = apply(X, 2, sd) # for the continuous variables, standardize by sd
	#penwt = colSums(Con)/colSums(Con !=0) # ABC reweighting
	#penwt[colSums(Con !=0)==0] = apply(X[,colSums(Con !=0)==0], 2, sd) # for the continuous variables, standardize by sd
	penwt["(Intercept)"] = 1 # for the intercept

	# Estimated coefficients for the full dataset along the lambda path:

	# Ridge version:
	if(type == 'ridge') {

		if (!requireNamespace("glmnet", quietly = TRUE)) {
			stop(
				"Package \"glmnet\" must be installed to use this function.",
				call. = FALSE
			)
		}

		# Get the lambda path, if unspecified
		if(is.null(lambda_path)){
			lambda_path = glmnet::glmnet(x = X[,-1],
																	 y = y,
																	 family ="gaussian",
																	 alpha = 0)$lambda
		}
		# Recurring term:
		DtDridge = crossprod(sqrt(penwt)*Qm)
		XtX = crossprod(Xuse); Xty = crossprod(Xuse,y)

		# Estimated coefficients along the path:
		beta_path = sapply(lambda_path, function(lambda){
			Qm%*%chol2inv(chol(XtX + lambda*DtDridge))%*%Xty
		})
		rownames(beta_path) = colnames(X); colnames(beta_path) = round(lambda_path, 3)

		# Degrees of freedom:
		df = rep(ncol(Xuse), length(lambda_path))
	}

	# Lasso version:
	if(type == 'lasso') {

		if (!requireNamespace("genlasso", quietly = TRUE)) {
			stop(
				"Package \"genlasso\" must be installed to use this function.",
				call. = FALSE
			)
		}

		# Recurring term:
		Dlasso = penwt*Qm

		# Fit the model to the full dataset:
		fit_lasso = genlasso::genlasso(y = y,
																	 X = Xuse,
																	 D = Dlasso)

		# Get the lambda path, if unspecified
		if(is.null(lambda_path)) lambda_path = fit_lasso$lambda

		# Estimated coefficients along the path:
		beta_path = sapply(lambda_path, function(lambda){
			crossprod(t(Qm), coef(fit_lasso, lambda = lambda)$beta)
		})
		rownames(beta_path) = colnames(X); colnames(beta_path) = round(lambda_path, 3)

		# Threshold:
		tol = 10^-8
		beta_path[(abs(beta_path) < tol)] = 0

		# Degrees of freedom:
		df = fit_lasso$df
	}

	# Dimensions:
	n = nrow(X)
	L = length(lambda_path)

	# Indices of the kth *holdout* set, k=1:K
	I_k = split(sample(1:n),
							1:K)

	mse_all = matrix(0, nrow = K, ncol = L)
	for(k in 1:K){

		# Response variable: in-sample and out-of-sample
		y_in = y[-I_k[[k]]]
		y_out = y[I_k[[k]]]

		# In-sample (lower-dim) covariate for fitting:
		Xuse_in = Xuse[-I_k[[k]],]

		# Out-of-sample covariates:
		n_out = length(I_k[[k]]) # number of out-of-sample (testing) points
		X_out = matrix(X[I_k[[k]],],
									 nrow = n_out) # in case there is only one point
		XtX_out = crossprod(X_out) # reused for evaluation along the path

		# Ridge estimator:
		if(type=='ridge'){
			# In-sample quantities:
			XtX_in = crossprod(Xuse_in)
			Xty_in = crossprod(Xuse_in, y_in)

			# Estimates along the path:
			beta_path_in = sapply(lambda_path, function(lambda){
				Qm%*%chol2inv(chol(XtX_in + lambda*DtDridge))%*%Xty_in
			})
		}
		# Lasso estimator:
		if(type=='lasso'){
			# Fit the model:
			fit_lasso = genlasso::genlasso(y = y_in,
																		 X = Xuse_in,
																		 D = Dlasso)
			# Estimates along the path:
			beta_path_in = sapply(lambda_path, function(lambda){
				crossprod(t(Qm), coef(fit_lasso, lambda = lambda)$beta)
			})
		}

		# Now evaluate the paths:
		mse_all[k,]= apply(beta_path_in, 2, function(beta_ell){
			#mean((X_out%*%beta_ell - y_out)^2)
			mean(y_out^2) +
				1/n_out*as.numeric(crossprod(beta_ell, XtX_out)%*%beta_ell) -
				2/n_out*crossprod(y_out, X_out%*%beta_ell)
		})
	}
	# MSE for each lambda value:
	mse = colMeans(mse_all)

	# Standard error:
	se = apply(mse_all, 2, sd)/sqrt(K)

	# Index of minimum CV-MSE:
	ind.min = which.min(mse)

	# Value of lambda that gives minimum CV-MSE:
	lambda.min = lambda_path[ind.min]

	# Largest value of lambda such that error is within 1 SE of min CV-MSE:
	lambda.1se = max(lambda_path[which(abs(mse - mse[ind.min]) < se)])

	# Index of 1-se lambda:
	ind.1se = which(lambda_path == lambda.1se)

	# Add a plot?
	if(plot){
		graphics::plot(log(lambda_path), mse, ylim = range(mse + se, mse - se),
									 type = 'n', main = paste(K, '-fold CV: ', type, ' regression with ABCs', sep=''))
		graphics::arrows(log(lambda_path), mse - se,
										 log(lambda_path), mse + se,
										 length=0.05, angle=90, code=3, lwd=1, col='gray')
		graphics::lines(log(lambda_path), mse, type='p', pch = 16, col='red')
		graphics::abline(v = log(lambda.1se), lty =2)
		graphics::abline(v = log(lambda.min), lty =2)
	}

	# Return:
	return(list(
		coefficients = beta_path,
		lambda_path = lambda_path,
		df = df,
		mse = mse,
		se = se,
		ind.min = ind.min,
		ind.1se = ind.1se,
		lambda.min = lambda.min,
		lambda.1se = lambda.1se
	))

}
