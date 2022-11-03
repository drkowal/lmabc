n <- 500

groups_1 <- c("Asian", "Hisp", "NHB", "NHW") # groups
pi_1 <- c(0.10, 0.15, 0.20, 0.55) # population proprtions
K_1 <- length(groups_1) # number of levels
k_1 <- factor(sample(groups_1,
										size  = n, replace = TRUE,
										prob  = pi_1),
						 levels = c("NHW", "Asian", "Hisp", "NHB")) # force NHW as baseline

# Summarize:
table(k_1)/n

# Simulate  continuous covariates:
x1 <- rnorm(n = n, mean = 5,  sd = 2) #  not standardized
x2 <- x1 + rnorm(n = n, mean = -5,  sd = 1) #  not standardized
cor(x1, x2) # x1, x2 correlated

# Expected value of the response:
Ey_cont <- 1 + 2*x1 + -2*x2 + 3*I(k_1==groups_1[1]) -.5*I(k_1==groups_1[2]) + .5*I(k_1==groups_1[2])*x1 + rnorm(sd = 10, n=n)
Ey <- Ey_cont >= 12

# Observed y-value:
y <- round(abs(as.numeric(Ey) - runif(n, 0, 1))); table(y)

f0 <- formula(y ~ x1 + x2 + x1*k_1)

# Data frame w/ variables needed:
dat <- data.frame(y, x1, x2, k_1)

fit_base_glm <- glm(f0, family = "binomial")

fit_abc_glm <- glm_abc(f0, data = dat, family = "binomial")
