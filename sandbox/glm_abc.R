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
Ey <- 1 + 2*x1 + -2*x2 + 3*I(k_1==groups_1[1]) -.5*I(k_1==groups_1[2]) + .5*I(k_1==groups_1[2])*x1 + rnorm(sd = 10, n=n) >= 12

# Observed y-value:
y <- as.numeric(Ey); table(y)
