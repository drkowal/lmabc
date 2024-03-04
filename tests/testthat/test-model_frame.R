test_that("all columns in data works", {
	f <- f_contY_all
  expect_equal(model_frame_wrapper(f, df),
  						 df |>
  						 	dplyr::select(mpg, hp, disp, cyl, gear, carb)
  						 )
})

test_that("some columns in data, some environment works", {
	x <- rnorm(nrow(df), 5, 1)
	f <- formula(mpg ~ x + disp + cyl)

	expect_equal(model_frame_wrapper(f, df),
							 df |>
							 	dplyr::select(mpg, disp, cyl) |>
							 	tibble::add_column(x) |>
							 	dplyr::relocate(x, .after = mpg)
							 )
})

test_that("some columns in data, some environment duplicated prioritizes data columns", {
	mpg <- rep(0, nrow(df))
	x <- rnorm(nrow(df), 5, 1)
	f <- formula(mpg ~ x + disp + cyl)

	expect_equal(model_frame_wrapper(f, df),
							 df |>
							 	dplyr::select(mpg, disp, cyl) |>
							 	tibble::add_column(x) |>
							 	dplyr::relocate(x, .after = mpg)
	)
})

test_that("all columns in environment works", {
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

	f <- formula(y ~ x + race + x:race + sex + race:sex)

	expect_equal(model_frame_wrapper(f), data.frame(y, x, race, sex))
})

test_that("missing column throws error", {
	f <- formula(mpg ~ x + disp + cyl)

	expect_error(model_frame_wrapper(f, df))
})
