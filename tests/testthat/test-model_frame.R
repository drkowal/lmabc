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
	f <- formula(y ~ x + race + x:race + sex + race:sex)

	expect_equal(model_frame_wrapper(f), data.frame(y, x, race, sex))
})

test_that("missing column throws error", {
	f <- formula(mpg ~ x + disp + cyl)

	expect_error(model_frame_wrapper(f, df))
})
