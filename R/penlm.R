#' @export
cv.penlm = function(formula,
										data,
										lambda_path = NULL,
										type = 'lasso',
										K = 10,
										plot = FALSE){

	# Quick check: ridge or lasso
	if(!(type == 'ridge' | type == 'lasso')){
		stop('type must be "ridge" or "lasso"')
	}

	# Get constraint matrix:
	Con = getConstraints(formula, data)
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
