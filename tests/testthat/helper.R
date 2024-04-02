df_nrow <- 1000

df <- mtcars |>
	dplyr::slice_sample(n = df_nrow, replace = TRUE) |>
	dplyr::mutate(dplyr::across(c(cyl, gear, carb), as.factor),
								dplyr::across(c(cyl, gear, carb), ~ sample(., df_nrow, replace = TRUE)),
								y_bin = rbinom(n = df_nrow, size = 1, prob = 0.75))

f_contY_contX <- formula(mpg ~ hp + disp)
f_contY_mixedX <- formula(mpg ~ disp + cyl)
f_contY_catX <- formula(mpg ~ cyl + gear)
f_contY_contX.contX <- formula(mpg ~ hp + disp + hp:disp)
f_contY_mixedX_contX.contX <- formula(mpg ~ hp + disp + cyl + hp:disp)
f_contY_contX.catX <- formula(mpg ~ disp + cyl + disp:cyl)
f_contY_catX.catX <- formula(mpg ~ cyl + gear + cyl:gear)
f_contY_all <- formula(mpg ~ hp + disp + cyl + gear + carb + hp:disp + disp:cyl + cyl:gear)

g_contY_contX <- formula(y_bin ~ hp + disp)
g_contY_catX <- formula(y_bin ~ cyl + gear)

model_frame_wrapper <- function(formula, data, props = NULL) {
	model_frame(formula, data, props)
}

n <- 500

sex <- factor(sample(c("uu", "vv"),
										 size  = n, replace = TRUE,
										 prob  = c(0.5, 0.5)))

race <- rep('A', n)
race[sex == 'uu'] <- sample(c("A", "B", "C", "D"),
														size  = sum(sex == 'uu'),
														replace=TRUE,
														prob  = c(0.55, 0.20, 0.10, 0.15))
race[sex == 'vv'] <- sample(c("A", "B", "C", "D"),
														size  = sum(sex == 'vv'),
														replace = TRUE,
														prob  = c(0.15, 0.10, 0.20, 0.55))

x <- rep(NA, n)
x[race == 'A'] <- 5 + rnorm(n = sum(race == 'A'))
x[race == 'B'] <- 0 + sqrt(12)*runif(n = sum(race == 'B'))
x[race == 'C'] <- -5 + sqrt((4-2)/4)*rt(n = sum(race == 'C'), df = 4)
x[race == 'D'] <- 0 + rgamma(n = sum(race == 'D'), shape = 1, rate = 1)

eta <- 0.5
Ey <- 1 + x + I(race=='A') - I(race == 'C') +
	eta*x*I(race=='A') -
	eta*x*I(race =="B")
y <- Ey + sqrt((4-2)/4)*rt(n = n, df = 4)
