test_that("y ~ cont1 + cont2 works", {
  expect_null(getConstraints(f_contY_contX, df))
})

test_that("y ~ cat1 + cat2 works", {
	expected <- matrix(nrow = 2, ncol = 7,
										 dimnames = list(c("cyl", "gear"),
										 								c("(Intercept)", "cyl4", "cyl6", "cyl8", "gear3", "gear4", "gear5")))

	expect_equal(construct_empty_con(f_contY_catX, df), expected = expected)
})
