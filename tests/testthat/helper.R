df <- mtcars |>
	dplyr::slice_sample(n = 1000, replace = TRUE) |>
	dplyr::mutate(dplyr::across(c(cyl, gear, carb), as.factor),
								dplyr::across(c(cyl, gear, carb), ~ sample(., 1000, replace = TRUE)))

f_contY_contX <- formula(mpg ~ hp + disp)
f_contY_mixedX <- formula(mpg ~ disp + cyl)
f_contY_catX <- formula(mpg ~ cyl + gear)
f_contY_contX.contX <- formula(mpg ~ hp + disp + hp:disp)
f_contY_contX.catX <- formula(mpg ~ disp + cyl + disp:cyl)
f_contY_contX.catX2 <- formula(mpg ~ disp + cyl + disp:cyl + gear + disp:gear + carb)
f_contY_catX.catX <- formula(mpg ~ cyl + gear + cyl:gear)
f_contY_all <- formula(mpg ~ hp + disp + cyl + gear + carb + hp:disp + disp:cyl + cyl:gear)
