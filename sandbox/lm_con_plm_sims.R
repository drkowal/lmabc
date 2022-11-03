# --------------------------------------------
# Regression with categorical variables: example with penalized regression
rm(list = ls()); saveplots = FALSE
# --------------------------------------------
# Simulations for penalized regression:
# --------------------------------------------
# Number of simulations:
Niters = 500

# Specs:
n = 250 # sample size
SNR = 1 # signal-to-noise ratio

# Type of categorical proportions:
prop_type = "symmetric"  # imbalanced but symmetric
#prop_type = "uniform"  # uniform

# For the baseline models: is the baseline "truly" zero?
# This should advantage the "baseline" methods
true_base = TRUE
#-------------------------------------------------------------
# Recurring terms:
#-------------------------------------------------------------
# Formula statement: 10 continuous variables and 1 categorical modifier
f0 = formula(y ~ (x1 + x2 + x3 + x4 + x5 +
                    x6 + x7 + x8 + x9 + x10)*k_1)

# Categorical variable:
groups_1 = c("Asian", "Hisp", "NHB", "NHW") # groups
if(prop_type == "symmetric") pi_1 = c(0.10, 0.40, 0.10, 0.40)
if(prop_type == "uniform") pi_1 = c(0.25, 0.25, 0.25, 0.25)
# NOT INTERPRETABLE: # if(prop_type == "imbalanced") pi_1 = c(0.10, 0.15, 0.20, 0.55)
K_1 = length(groups_1)
#-------------------------------------------------------------
# Storage:
#-------------------------------------------------------------
# Mean squared errors:
mse_y = mse_beta = mse_group_slopes =
  array(NA, c(Niters, 8),
        dimnames = list(1:Niters,
                        c('ols(abc)',
                          'ols(base)',
                          'lasso(abc)',
                          'lasso(base)',
                          'lasso(over)',
                          'ridge(abc)',
                          'ridge(base)',
                          'ridge(over)')))

# True positive/negative rates and model sizes:
TPR_beta = TNR_beta =
  array(NA, c(Niters, 3),
        dimnames = list(1:Niters,
                        c('lasso(abc)',
                          'lasso(base)',
                          'lasso(over)')))

# Coverage probability and width for beta:
ci_cover_beta = ci_width_beta =
  array(NA, c(Niters, 2),
        dimnames = list(1:Niters,
                        c('ols(abc)',
                        'ols(base)')))
#-------------------------------------------------------------
# Run the simulations:
#-------------------------------------------------------------
for(ni in 1:Niters){
  #-------------------------------------------------------------
  # For reproducibility:
  set.seed(ni)
  #-------------------------------------------------------------
  # Simulate the data:
  #-------------------------------------------------------------
  # Simulate the covariates:
  k_1 = factor(sample(groups_1,
                      size  = n, replace = TRUE,
                      prob  = pi_1),
               levels = c("NHW", "Asian", "Hisp", "NHB")) # force NHW as baseline

  # Continuous variables:
  x1 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x2 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x3 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x4 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x5 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x6 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x7 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x8 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x9 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)
  x10 = rnorm(n = n, mean = rnorm(1, sd=2), sd = 1)

  # Force some to be dependent on the categorical variable:
  for(klev in levels(k_1)) x3[k_1 == klev] = rnorm(n = sum(k_1 == klev), mean = rnorm(1, sd=2), sd = 1)
  for(klev in levels(k_1)) x7[k_1 == klev] = rnorm(n = sum(k_1 == klev), mean = rnorm(1, sd=2), sd = 1)

  # # Expected response:
  # Ey = 1 +
  #   1*x1 +
  #   -1*x2 +
  #   1*x3 +
  #   1*x4 +
  #   1*x5 +
  #   1*I(k_1 == "NHW") + -1*I(k_1 == "Hisp") + 1*I(k_1 == "Asian") + -1*I(k_1 == "NHB") +
  #   -0.5*x1*I(k_1 == "NHW") + 0.5*x1*I(k_1 == "Hisp") + -0.5*x1*I(k_1 == "Asian") + 0.5*x1*I(k_1 == "NHB") +
  #   -0.5*x3*I(k_1 == "NHW") + 0.5*x3*I(k_1 == "Hisp") + -0.5*x3*I(k_1 == "Asian") + 0.5*x3*I(k_1 == "NHB")

  # Full (overparametrized) design matrix:
  X = getFullDesign(formula = f0,
                    data = data.frame(y = rep(0, n), # placeholder
                                      x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, k_1))
  # True regression coefficients:
  beta_true = rep(0, ncol(X)); names(beta_true) = colnames(X)
  beta_true[match("(Intercept)", colnames(X))] = 1
  beta_true[match("x1", colnames(X))] = 1
  beta_true[match("x2", colnames(X))] = -1
  beta_true[match("x3", colnames(X))] = 1
  beta_true[match("x4", colnames(X))] = 1
  beta_true[match("x5", colnames(X))] = 1
  if(!true_base){
    beta_true[match("k_1NHW", colnames(X))] = 1
    beta_true[match("k_1Hisp", colnames(X))] = -1
  }
  beta_true[match("k_1Asian", colnames(X))] = 1
  beta_true[match("k_1NHB", colnames(X))] = -1
  if(!true_base){
    beta_true[match("x1:k_1NHW", colnames(X))] = -0.5
    beta_true[match("x1:k_1Hisp", colnames(X))] = 0.5
  }
  beta_true[match("x1:k_1Asian", colnames(X))] = -0.5
  beta_true[match("x1:k_1NHB", colnames(X))] = 0.5
  if(!true_base){
    beta_true[match("x3:k_1NHW", colnames(X))] = -0.5
    beta_true[match("x3:k_1Hisp", colnames(X))] = 0.5
  }
  beta_true[match("x3:k_1Asian", colnames(X))] = -0.5
  beta_true[match("x3:k_1NHB", colnames(X))] = 0.5

  # Expected response:
  Ey = X%*%beta_true

  # Observed data:
  y  = Ey + sd(Ey)/sqrt(SNR)*rnorm(n=n)

  if(ni %% 10 == 0) plot(y, Ey, main = ni) # shows the strength of the signal in the data

  # Data frame of the variables:
  dat = data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, k_1)

  # Model matrix for baseline deletion:
  X0 = X[,-grep('NHW', colnames(X))] # remove all NHW terms
  # --------------------------------------------
  # OLS ESTIMATORS
  # --------------------------------------------
  # OLS ABC: estimates and CIs
  fit_abc = lm.abc(formula = f0, data  = dat)
  beta_hat_abc = coef(fit_abc)
  ci_beta = confint(fit_abc)

  # OLS baseline: estimates and CIs
  fit_base = lm(y ~ X0 - 1, data  = dat)
  beta_hat_base = rep(0, ncol(X))
  beta_hat_base[match(colnames(X0), colnames(X))] = coef(fit_base)
  ci_beta_base = 0*ci_beta
  ci_beta_base[match(colnames(X0), colnames(X)),] = confint(fit_base)
  # --------------------------------------------
  # LASSO ESTIMATORS
  # --------------------------------------------
  # Lasso ABC:
  fit_lasso = cv.penlm(formula = f0,
                       data = dat,
                       type = 'lasso')
  beta_hat_lasso = coef(fit_lasso)[,fit_lasso$ind.1se]

  # Lasso baseline:
  fit_lasso_base = cv.glmnet(x = X0[,-1],
                             y = y,
                             family ="gaussian", alpha = 1)
  beta_hat_base_lasso = rep(0, ncol(X))
  beta_hat_base_lasso[match(colnames(X0), colnames(X))] = as.numeric(coef(fit_lasso_base, fit_lasso_base$lambda.1se))

  # Lasso overparametrized:
  fit_lasso_over = cv.glmnet(x = X[,-1],
                             y = y,
                             family ="gaussian", alpha = 1)
  beta_hat_over_lasso = coef(fit_lasso_over,
                             fit_lasso_over$lambda.1se)
  # --------------------------------------------
  # RIDGE ESTIMATORS
  # --------------------------------------------
  # Ridge ABC:
  fit_ridge = cv.penlm(formula = f0,
                       data = dat,
                       type = 'ridge')
  beta_hat_ridge = coef(fit_ridge)[,fit_ridge$ind.1se]

  # Ridge baseline:
  fit_ridge_base = cv.glmnet(x = X0[,-1],
                             y = y,
                             family ="gaussian", alpha = 0)
  beta_hat_base_ridge = rep(0, ncol(X))
  beta_hat_base_ridge[match(colnames(X0), colnames(X))] = as.numeric(coef(fit_ridge_base, fit_ridge_base$lambda.1se))

  # Ridge overparametrized:
  fit_ridge_over = cv.glmnet(x = X[,-1],
                             y = y,
                             family ="gaussian", alpha = 0)
  beta_hat_over_ridge = coef(fit_ridge_over,
                             fit_ridge_over$lambda.1se)
  # --------------------------------------------
  # Record the results:
  # --------------------------------------------
  # MSE for Ey:
  mse_y[ni,1] = mean((Ey - X%*%beta_hat_abc)^2)
  mse_y[ni,2] = mean((Ey - X%*%beta_hat_base)^2)
  mse_y[ni,3] = mean((Ey - X%*%beta_hat_lasso)^2)
  mse_y[ni,4] = mean((Ey - X%*%beta_hat_base_lasso)^2)
  mse_y[ni,5] = mean((Ey - X%*%beta_hat_over_lasso)^2)
  mse_y[ni,6] = mean((Ey - X%*%beta_hat_ridge)^2)
  mse_y[ni,7] = mean((Ey - X%*%beta_hat_base_ridge)^2)
  mse_y[ni,8] = mean((Ey - X%*%beta_hat_over_ridge)^2)

  # MSE for beta (NOTE: only interpretable under ABCs!)
  mse_beta[ni,1] = mean((beta_true - beta_hat_abc)^2)
  mse_beta[ni,2] = mean((beta_true - beta_hat_base)^2)
  mse_beta[ni,3] = mean((beta_true - beta_hat_lasso)^2)
  mse_beta[ni,4] = mean((beta_true - beta_hat_base_lasso)^2)
  mse_beta[ni,5] = mean((beta_true - beta_hat_over_lasso)^2)
  mse_beta[ni,6] = mean((beta_true - beta_hat_ridge)^2)
  mse_beta[ni,7] = mean((beta_true - beta_hat_base_ridge)^2)
  mse_beta[ni,8] = mean((beta_true - beta_hat_over_ridge)^2)

  # True positive/negative rates:
  sigs = (beta_true != 0) # true signals

  TPR_beta[ni,1] = sum((beta_hat_lasso != 0) & sigs)/sum(sigs)
  TPR_beta[ni,2] = sum((beta_hat_base_lasso != 0) & sigs)/sum(sigs)
  TPR_beta[ni,3] = sum((beta_hat_over_lasso != 0) & sigs)/sum(sigs)

  TNR_beta[ni,1] = sum(!(beta_hat_lasso != 0) & !sigs)/sum(!sigs)
  TNR_beta[ni,2] = sum(!(beta_hat_base_lasso != 0) & !sigs)/sum(!sigs)
  TNR_beta[ni,3] = sum(!(beta_hat_over_lasso != 0) & !sigs)/sum(!sigs)

  # Coverage and width:
  ci_cover_beta[ni, 1] = mean((beta_true >= ci_beta[,1]) & (beta_true <= ci_beta[,2]))
  ci_cover_beta[ni, 2] = mean((beta_true >= ci_beta_base[,1]) & (beta_true <= ci_beta_base[,2]))

  ci_width_beta[ni, 1] = mean(ci_beta[,2] - ci_beta[,1])
  ci_width_beta[ni, 2] = mean(ci_beta_base[,2] - ci_beta_base[,1])
  # --------------------------------------------
  # Group slopes:
  # --------------------------------------------
  xnames = colnames(X)
  get_group_slopes = function(beta_hat){
    sapply(paste('x', 1:10, sep=''), function(x){
      sub_x = match(x, xnames)
      sub_ind = grep(paste(x, ":", sep=''), xnames)
      beta_group = beta_hat[sub_x] + beta_hat[sub_ind]
      names(beta_group) = xnames[sub_ind]
      return(beta_group)
    })
  }
  beta_true_group = get_group_slopes(beta_true)

  # MSE for group-specific slopes:
  mse_group_slopes[ni,1] = mean((beta_true_group - get_group_slopes(beta_hat_abc))^2)
  mse_group_slopes[ni,2] = mean((beta_true_group - get_group_slopes(beta_hat_base))^2)
  mse_group_slopes[ni,3] = mean((beta_true_group - get_group_slopes(beta_hat_lasso))^2)
  mse_group_slopes[ni,4] = mean((beta_true_group - get_group_slopes(beta_hat_base_lasso))^2)
  mse_group_slopes[ni,5] = mean((beta_true_group - get_group_slopes(beta_hat_over_lasso))^2)
  mse_group_slopes[ni,6] = mean((beta_true_group - get_group_slopes(beta_hat_ridge))^2)
  mse_group_slopes[ni,7] = mean((beta_true_group - get_group_slopes(beta_hat_base_ridge))^2)
  mse_group_slopes[ni,8] = mean((beta_true_group - get_group_slopes(beta_hat_over_ridge))^2)
}
# --------------------------------------------
# Summarize the results:
# MSE for Ey:
boxplot(sqrt(mse_y[,-(1:2)]), main = 'RMSE for E(y | X)')

# MSE for beta:
boxplot(sqrt(mse_beta[,-(1:2)]), main = 'RMSE for beta')

# MSE for group slopes:
boxplot(sqrt(mse_group_slopes[,-(1:2)]), main = 'RMSE for group slopes')
# --------------------------------------------
# Save the results:
# --------------------------------------------
# File name for saving:
fname = paste("n=", n,
              "_SNR=", SNR,
              "_",prop_type,
              ifelse(true_base, '_truebase', ''),
              sep='')

# RMSEs for Ey:
if(saveplots) pdf(file = paste("~/Dropbox/Projects/PRIME/Categorical variables/Plots/rmse_y_", fname,".pdf", sep=''), width = 13, height = 8)
par(mai = c(.5, 3, 1, 0.25), las = 1);
bp = boxplot(sqrt(mse_y[1:ni,]),
             main = paste('Root mean squared error: E(y | X) \n n = ',
                          n, ', SNR = ', SNR, ifelse(true_base, ', baseline: true 0', ''), sep = ''),
             horizontal = TRUE,
             col = 'gray',
             cex.lab = 2.25, cex.main = 3, cex.axis = 2.85, notch = TRUE, outline = FALSE);
if(saveplots) dev.off()

# RMSEs for coefficients:
if(saveplots) pdf(file = paste("~/Dropbox/Projects/PRIME/Categorical variables/Plots/rmse_beta_", fname,".pdf", sep=''), width = 13, height = 8)
par(mai = c(.5, 3, 1, 0.25), las = 1);
bp = boxplot(sqrt(mse_beta[1:ni,]),
             main = paste('Root mean squared error: coefficients \n n = ',
                          n, ', SNR = ', SNR, ifelse(true_base, ', baseline: true 0', ''), sep = ''),
             horizontal = TRUE,
             col = 'gray',
             cex.lab = 2.25, cex.main = 3, cex.axis = 2.85, notch = TRUE, outline = FALSE);
if(saveplots) dev.off()

# RMSEs for group slopes:
if(saveplots) pdf(file = paste("~/Dropbox/Projects/PRIME/Categorical variables/Plots/rmse_groups_", fname,".pdf", sep=''), width = 13, height = 8)
par(mai = c(.5, 3, 1, 0.25), las = 1);
bp = boxplot(sqrt(mse_group_slopes[1:ni,]),
             main = paste('Root mean squared error: group slopes \n n = ',
                          n, ', SNR = ', SNR, ifelse(true_base, ', baseline: true 0', ''), sep = ''),
             horizontal = TRUE,
             col = 'gray',
             cex.lab = 2.25, cex.main = 3, cex.axis = 2.85, notch = TRUE, outline = FALSE);
if(saveplots) dev.off()

# Mean interval widths: linear coefficients
if(saveplots) pdf(file = paste("~/Dropbox/Projects/PRIME/Categorical variables/Plots/mciw_", fname,".pdf", sep=''), width = 13, height = 8)
par(mai = c(.5, 3, 1, 0.25), las = 1);
bp0 = boxplot(ci_width_beta[1:ni,], plot = FALSE)
bp = boxplot(ci_width_beta[1:ni,],
             main = paste('Mean interval widths: linear coefficients \n n = ',
                          n, ', SNR = ', SNR, ifelse(true_base, ', baseline: true 0', ''), sep = ''),
             ylim = range(1.15*bp0$stats[5,], .95*bp0$stats[2,], na.rm=TRUE),
             horizontal = TRUE,
             col = 'gray',
             cex.lab = 2.25, cex.main = 3, cex.axis = 3.85, notch = TRUE, outline = FALSE);
text(bp$stats[5,], 1:ncol(ci_width_beta[1:ni,]), labels = paste(' ', round(100*apply(ci_cover_beta[1:ni,], 2, mean, na.rm=TRUE)), '%', sep=''), cex = 3.5, adj = c(0,NA), col='blue')
if(saveplots) dev.off()
# --------------------------------------------
# Print some results:
# --------------------------------------------
sort(sqrt(colMeans(mse_y, na.rm=TRUE)))
sort(sqrt(colMeans(mse_beta, na.rm=TRUE)))
sort(sqrt(colMeans(mse_group_slopes, na.rm=TRUE)))

# TPR/TNR
colMeans(TPR_beta, na.rm=TRUE)
colMeans(TNR_beta, na.rm=TRUE)

# CIs:
colMeans(ci_cover_beta, na.rm=TRUE)
colMeans(ci_width_beta, na.rm=TRUE)

# Summarize the simulation:
print(paste('n =',n)); print(paste('SNR =', SNR)); print(paste('categorical proportions =', prop_type)); print(paste('true baseline =', true_base))
