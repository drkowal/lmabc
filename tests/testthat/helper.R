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
f_contY_contX.catX <- formula(mpg ~ disp + cyl + disp:cyl)
f_contY_catX.catX <- formula(mpg ~ cyl + gear + cyl:gear)
f_contY_all <- formula(mpg ~ hp + disp + cyl + gear + carb + hp:disp + disp:cyl + cyl:gear)

g_contY_contX <- formula(y_bin ~ hp + disp)
g_contY_catX <- formula(y_bin ~ cyl + gear)
