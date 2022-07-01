# --------------------------------------------
# Example script for categorical regression
# using baseline coding vs. ABCs

rm(list = ls())

# Source functions:
# source("source_lmabc.R")
# --------------------------------------------
# Simulate some data:
n = 200

# Categorical variable: race
groups_1 = c("Asian", "Hisp", "NHB", "NHW") # groups
pi_1 = c(0.10, 0.15, 0.20, 0.55) # population proprtions
K_1 = length(groups_1) # number of levels
k_1 = factor(sample(groups_1,
                    size  = n, replace = TRUE,
                    prob  = pi_1),
             levels = c("NHW", "Asian", "Hisp", "NHB")) # force NHW as baseline

# Summarize:
table(k_1)/n

# Simulate  continuous covariates:
x1 = rnorm(n = n, mean = 5,  sd = 2) #  not standardized
x2 = x1 + rnorm(n = n, mean = -5,  sd = 1) #  not standardized
cor(x1, x2) # x1, x2 correlated

# Expected value of the response:
Ey = 1 + 2*x1 + -2*x2 +
  3*I(k_1==groups_1[1]) -.5*I(k_1==groups_1[2]) +
  .5*I(k_1==groups_1[2])*x1

# Observed y-value:
y  = Ey + rnorm(n=n)

# check signal strength:
plot(Ey, y)
# --------------------------------------------
# Now fit the models:
# --------------------------------------------
# Shared formula statement across models:
f0 = formula(y ~ x1 + x2 + x1*k_1)

# Data frame w/ variables needed:
dat = data.frame(y, x1, x2, k_1)

# Fit the linear model using NHW as the reference group:
fit_base = lm(f0)

# Fit the linear model w/ ABCs:
fit_abc = lm_abc(f0, data = dat)
# --------------------------------------------
# Compare the outputs:
# note: the parameters have different interpretations!
# --------------------------------------------
# Coefficient estimates:
coef(fit_base)
coef(fit_abc)

# Note: these have different lengths
length(coef(fit_base)) # deletes (= 0) reference group for (race) main effects + interactions
length(coef(fit_abc)) # includes all groups (w/ constraints)

# Confidence intervals:
confint(fit_base)
confint(fit_abc)

# Fitted values: these are the same!
sum((fitted(fit_base) - fitted(fit_abc))^2)

# AIC/BIC:
AIC(fit_base); AIC(fit_abc) # same
BIC(fit_base); BIC(fit_abc) # same

# Covariance of coefficient estimates:
vcov(fit_base)
vcov(fit_abc)

# Diagnostic plots:
# plot(fit_base)
# plot(fit_abc)

# Summaries:
summary(fit_base)
summary(fit_abc) # not done!

# predict() should also work
# --------------------------------------------
# Check the interpretations:
# --------------------------------------------
# Sample means + intercepts:
mean(y) # sample mean
mean(fitted(fit_abc)) # sample mean of the fitted values
coef(fit_base)["(Intercept)"]; # original intercept
coef(fit_abc)["(Intercept)"]; # ABC intercept: almost mean(y)...slightly different because of x,k corr

# Proportions:
pi_hat_1 = table(k_1)/n

# Group-specific terms: int
(beta_hat_int = coef(fit_abc)[
  match(paste("k_1", levels(k_1), sep=""),
        names(coef(fit_abc)))
])
# Average:
beta_hat_int%*%pi_hat_1 # numerical zero

# Group-specific terms: slope
(beta_hat_slope = coef(fit_abc)[
  match(paste("x1:k_1", levels(k_1), sep=""),
        names(coef(fit_abc)))
])
# Average:
beta_hat_slope%*%pi_hat_1 # numerical zero
# --------------------------------------------
# Showcase some other functions:
# --------------------------------------------
# Full design matrix (overparametrized):
XX = getFullDesign(f0, data = dat)
head(XX) # inspect...
ncol(XX);
mrank(XX) # rank < num columns -> rank deficient!

# Constraint matrix for ABCs:
(CC = getConstraints(f0,data = dat))
mrank(CC) # this makes up the difference above

# Check the constraint:
CC%*%coef(fit_abc) # numerical zeros
