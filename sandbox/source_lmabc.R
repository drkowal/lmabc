#### Source files for Linear Regression w/ ABCs ####

# Libraries we need:
library(genlasso) # for lasso estimation
library(glmnet) # for ridge regression (note: only needed to get the lambda sequence)

# Function to compute the linear regression with ABCs:
lm.abc = function(formula, data, ..., cprobs = NULL){

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
                      data = data)
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
    fit_con = lm(update(formula, ~ Xuse - 1),
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
    if(mean((fitted(fit_con) - fitted(fit0))^2)
       + mean((X%*%beta_con - fitted(fit0))^2) > tol){
      stop('Fitted values are not the same;
         may be an issue with the constraint matrix')
    }
    sigma_hat = summary(fit_con)$sigma # error SD

    # New class:
    fit = fit0;  attr(fit, 'class') = 'lm.abc'
    fit$lm = fit0 #  store the original object
    fit$X = X # store the full design matrix
    fit$Con = Con # store the constraint matrix
    fit$coefficients = beta_con # coefficient estimates
    fit$cov.unscaled = cov.unscaled_con # covariance matrix
    fit$sigma = sigma_hat # estimated standard deviation
    # fit$residuals # already there

    return(fit) # return the lm.abc object
  }

}

# Function to compute the generalized linear regression with ABCs:
glm.abc = function(formula, family = gaussian, data, ..., cprobs = NULL){

  # Usual glm fit: this is a nice baseline
  fit0 = glm(formula = formula,
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
                      data = data)
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
    fit_con = glm(update(formula, ~ Xuse - 1),
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
    if(mean((fitted(fit_con) - fitted(fit0))^2) > tol){
      stop('Fitted values are not the same;
         may be an issue with the constraint matrix')
    }
    sigma_hat = summary(fit_con)$sigma # error SD

    # New class:
    fit = fit0;  attr(fit, 'class') = 'glm.abc'
    fit$glm = fit0 #  store the original object
    fit$X = X # store the full design matrix
    fit$Con = Con # store the constraint matrix
    fit$coefficients = beta_con # coefficient estimates
    fit$cov.unscaled = cov.unscaled_con # covariance matrix
    fit$sigma = sigma_hat # estimated standard deviation
    # fit$residuals # already there

    return(fit) # return the glm.abc object
  }
}

# Function to compute the constraint matrix:
getConstraints = function(formula, data, cprobs = NULL){

  # Model frame has some useful information
  mf = model.frame(formula = formula,
                   data  = data)

  # Names of the variables involved:
  vnames = attr(mf, 'names')

  # Covariates:
  covar = attr(attr(mf, 'terms'),
               "term.labels")

  # Subset to the variables included in the model:
  data = data[,vnames]

  # Handle the categorical variables:
  f_inds = which(sapply(data, is.factor)) # factor indices

  if(length(f_inds) > 0){
    cdat = data.frame(data[,f_inds]) # data frame
    cnames = names(cdat) = names(data)[f_inds] # correct the names

    # Compute the full design matrix:
    X = getFullDesign(formula = formula,
                      data = data)
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

    # Check for categorical-categorical pairs
    # (these will be handled separately below)
    inds_catcat = match(
      sapply(cnames, function(x) paste(x,  ':', cnames[cnames!=x], sep='')),
      covar)
    inds_catcat = inds_catcat[!is.na(inds_catcat)]
    if(length(inds_catcat) > 0){
      covar_all = covar; covar = covar[-inds_catcat]
    }

    # Indices of the categorical variables/interactions w/in 'covar' (excluding cat-cat)
    c_inds = lapply(cnames, function(na) grep(na, covar)); names(c_inds) = cnames

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
                          function(g) gsub(cnames[k], g, v_kl))

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

# Compute the full (overparametrized) design matrix X
getFullDesign = function(formula, data, center = TRUE){

  # Model frame has some useful information
  mf = model.frame(formula = formula,
                   data  = data)

  # Names of the variables involved:
  vnames = attr(mf, 'names')

  # Covariates:
  covar = attr(attr(mf, 'terms'),
               "term.labels")

  # Subset to the variables included in the model:
  data = data[,vnames]

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
    X = model.matrix(object = formula,
                     data = data,
                     contrasts.arg =
                       lapply(cdat, contrasts, contrasts=FALSE))
  } else {
    # No categorical variables, so just return the usual value:
    X = model.matrix(object = formula,
                     data = data)
  }

  return(X)
}

# Some updates:
vcov.lm.abc = function(fit){
  fit$sigma^2*fit$cov.unscaled
}
AIC.lm.abc = function(fit,...){
  AIC(fit$lm,...)
}
BIC.lm.abc = function(fit,...){
  BIC(fit$lm,...)
}
predict.lm.abc = function(fit,...){
  predict.lm(fit$lm,...)
}

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
  y = model.frame(formula, data)[,1]

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
      lambda_path = glmnet(x = X[,-1],
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
    fit_lasso = genlasso(y = y,
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
      fit_lasso = genlasso(y = y_in,
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
    plot(log(lambda_path), mse, ylim = range(mse + se, mse - se),
         type = 'n', main = paste(K, '-fold CV: ', type, ' regression with ABCs', sep=''))
    arrows(log(lambda_path), mse - se,
           log(lambda_path), mse + se,
           length=0.05, angle=90, code=3, lwd=1, col='gray')
    lines(log(lambda_path), mse, type='p', pch = 16, col='red')
    abline(v = log(lambda.1se), lty =2)
    abline(v = log(lambda.min), lty =2)
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

# FIXME!!!!
  # get the right updates for these functions
  # also include updates for glms!

# confint(fit) # this works!
summary.lm.abc = function(fit){
  # fixme!
  ses = sqrt(diag(vcov(fit)))
}
# not quite right...
plot.lm.abc = function(fit,...){
  plot(fit$lm,...)
}


# Function to compute rank of matrix
mrank = function(M, tol = 10^-8) sum(svd(M)$d^2 > tol)
