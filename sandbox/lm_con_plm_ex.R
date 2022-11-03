# --------------------------------------------
# Regression with categorical variables: example with penalized regression
rm(list = ls())
# --------------------------------------------
# Simulate some data:
n = 250 # sample size
SNR = 1 # signal-to-noise ratio

# Categorical variable:
groups_1 = c("Asian", "Hisp", "NHB", "NHW") # groups
pi_1 = c(0.10, 0.40, 0.10, 0.40) # for simplicity, assume symmetry
K_1 = length(groups_1)

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

# Expected response:
Ey = 1 +
  1*x1 +
  -1*x2 +
  1*x3 +
  1*x4 +
  1*x5 +
  1*I(k_1 == "NHW") + -1*I(k_1 == "Hisp") + 1*I(k_1 == "Asian") + -1*I(k_1 == "NHB") +
  -0.5*x1*I(k_1 == "NHW") + 0.5*x1*I(k_1 == "Hisp") + -0.5*x1*I(k_1 == "Asian") + 0.5*x1*I(k_1 == "NHB") +
  -0.5*x3*I(k_1 == "NHW") + 0.5*x3*I(k_1 == "Hisp") + -0.5*x3*I(k_1 == "Asian") + 0.5*x3*I(k_1 == "NHB")

# Observed data:
y  = Ey + sd(Ey)/sqrt(SNR)*rnorm(n=n)

plot(y, Ey) # shows the strength of the signal in the data
# --------------------------------------------
# data frame of the variables:
dat = data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, k_1)

# Formula statement:
f0 = formula(y ~ (x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10)*k_1)
# --------------------------------------------
# LASSO ESTIMATORS
# --------------------------------------------
# Lasso estimator w/ ABCs:
fit_lasso_cv = cv.penlm(formula = f0,
                        data = dat,
                        type = 'lasso', K = 10, plot = TRUE)
# Coefficients along the lambda path, and the lambda path:
beta_path_lasso = coef(fit_lasso_cv)
lambda_path_lasso = fit_lasso_cv$lambda_path

# 1-se rule:
beta_hat_lasso = beta_path_lasso[,fit_lasso_cv$ind.1se]
# --------------------------------------------
# Lasso estimator for overparametrized model:
X = getFullDesign(formula = f0,
                  data = dat)
fit_lasso_full = cv.glmnet(x = X[,-1],
                           y = y,
                           family ="gaussian", alpha = 1)
# Coefficients along the lambda path, and the lambda path:
beta_path_full_lasso = coef(fit_lasso_full$glmnet.fit)
lambda_path_full_lasso = fit_lasso_full$lambda

# 1-se rule:
beta_hat_full_lasso = coef(fit_lasso_full,
                           fit_lasso_full$lambda.1se)
# --------------------------------------------
# Using the "incorrect" baseline:
X0 = X[,-grep('NHW', colnames(X))] # remove all NHW terms
fit_lasso_base = cv.glmnet(x = X0[,-1],
                           y = y,
                           family ="gaussian", alpha = 1)
# Coefficients along the lambda path, and the lambda path:
lambda_path_base_lasso = fit_lasso_base$lambda
beta_path_base_lasso = matrix(0, ncol(X), length(lambda_path_base_lasso))
beta_path_base_lasso[match(colnames(X0), colnames(X)),] = as.matrix(coef(fit_lasso_base$glmnet.fit))

# 1-se rule:
beta_hat_base_lasso = rep(0, ncol(X))
beta_hat_base_lasso[match(colnames(X0), colnames(X))] = as.numeric(coef(fit_lasso_base, fit_lasso_base$lambda.1se))
# --------------------------------------------
# Plot the coefficient paths:
# --------------------------------------------
xnames = row.names(beta_path_lasso)
sub_ind = c(match("x1", xnames),
  grep("x1:", xnames))

# Plot the results: ABCs
plot(log(lambda_path_lasso), beta_path_lasso[sub_ind[1],], type='n',
     ylim = range(beta_path_lasso[sub_ind,]),
     xlim = range(log(lambda_path_lasso), log(lambda_path_lasso) - 1.5),
     xlab = 'log(lambda)', ylab = 'Coefficients', main = paste('Lasso paths with ABCs:\n', paste(xnames[sub_ind], collapse = ', ')))
abline(h = 0, lty =6)
for(j in 1:length(sub_ind)){
  lines(log(lambda_path_lasso), beta_path_lasso[sub_ind[j],], lwd=3, col = j)
  text(log(lambda_path_lasso[length(lambda_path_lasso)]) - 1,
       beta_path_lasso[sub_ind[j],length(lambda_path_lasso)], xnames[sub_ind[j]])
}
abline(v = log(fit_lasso_cv$lambda.min))
abline(v = log(fit_lasso_cv$lambda.1se), lty=6)

# Plot the results: overparametrized model
plot(log(lambda_path_full_lasso), beta_path_full_lasso[sub_ind[1],], type='n',
     ylim = range(beta_path_full_lasso[sub_ind,]),
     xlim = range(log(lambda_path_full_lasso), log(lambda_path_full_lasso) - 1.5),
     xlab = 'log(lambda)', ylab = 'Coefficients', main = paste('Lasso path with overparametrized model:\n', paste(xnames[sub_ind], collapse = ', ')))
abline(h = 0, lty =6)
for(j in 1:length(sub_ind)){
  lines(log(lambda_path_full_lasso), beta_path_full_lasso[sub_ind[j],], lwd=3, col = j)
  text(log(lambda_path_full_lasso[length(lambda_path_full_lasso)]) - 1,
       beta_path_full_lasso[sub_ind[j],length(lambda_path_full_lasso)], xnames[sub_ind[j]])
}
abline(v = log(fit_lasso_full$lambda.min))
abline(v = log(fit_lasso_full$lambda.1se), lty=6)

# Plot the results: baseline model
plot(log(lambda_path_base_lasso), beta_path_base_lasso[sub_ind[1],], type='n',
     ylim = range(beta_path_base_lasso[sub_ind,]),
     xlim = range(log(lambda_path_base_lasso), log(lambda_path_base_lasso) - 1.5),
     xlab = 'log(lambda)', ylab = 'Coefficients', main = paste('Lasso path with baseline model:\n', paste(xnames[sub_ind], collapse = ', ')))
abline(h = 0, lty =6)
for(j in 1:length(sub_ind)){
  lines(log(lambda_path_base_lasso), beta_path_base_lasso[sub_ind[j],], lwd=3, col = j)
  text(log(lambda_path_base_lasso[length(lambda_path_base_lasso)]) - 1,
       beta_path_base_lasso[sub_ind[j],length(lambda_path_base_lasso)], xnames[sub_ind[j]])
}
abline(v = log(fit_lasso_base$lambda.min))
abline(v = log(fit_lasso_base$lambda.1se), lty=6)

# --------------------------------------------
# Plot the coefficient paths for the *group-specific* slopes
# --------------------------------------------
x = "x1";

# Select the coefficients you want:
beta_path = beta_path_lasso; lambda_path = lambda_path_lasso; lambda.min = fit_lasso_cv$lambda.min; lambda.1se = fit_lasso_cv$lambda.1se; method = 'ABCs'
#beta_path = beta_path_full_lasso; lambda_path = lambda_path_full_lasso; lambda.min = fit_lasso_full$lambda.min; lambda.1se = fit_lasso_full$lambda.1se; method = 'overparametrized model'
#beta_path = beta_path_base_lasso; lambda_path = lambda_path_base_lasso; lambda.min = fit_lasso_base$lambda.min; lambda.1se = fit_lasso_base$lambda.1se; method = 'baseline model'

sub_x = match(x, xnames)
sub_ind = grep(paste(x, ":", sep=''), xnames)
beta_x = beta_path[sub_x,]
beta_group = t(apply(beta_path[sub_ind,], 1, function(b) b + beta_x))

# Plot the results: ABCs
plot(log(lambda_path), beta_group[1,], type='n',
     ylim = range(beta_group),
     xlim = range(log(lambda_path), log(lambda_path) - 1.5),
     xlab = 'log(lambda)', ylab = 'Coefficients', main = paste('Lasso paths with', method,'\n', paste(xnames[sub_ind], collapse = ', ')))
abline(h = 0, lty =6)
for(j in 1:length(sub_ind)){
  lines(log(lambda_path), beta_group[j,], lwd=3, col = j+1)
  text(log(lambda_path[length(lambda_path)]) - 1,
       beta_group[j,length(lambda_path)], xnames[sub_ind[j]])
}
abline(v = log(lambda.min))
abline(v = log(lambda.1se), lty=6)
# --------------------------------------------
# Selected variables:
# --------------------------------------------
# Estimates:
sqrt(mean((Ey - X%*%beta_hat_lasso)^2))
sqrt(mean((Ey - X%*%as.numeric(beta_hat_full_lasso))^2))
sqrt(mean((Ey - X%*%beta_hat_base_lasso)^2))

print('ABC:'); sum(beta_hat_lasso!=0); names(which(beta_hat_lasso!=0));
print('overparametrized:'); sum(beta_hat_full_lasso!=0); colnames(X)[which(as.numeric(beta_hat_full_lasso)!=0)];
print('baseline:'); sum(beta_hat_base_lasso!=0); colnames(X)[which(as.numeric(beta_hat_full_lasso)!=0)]
