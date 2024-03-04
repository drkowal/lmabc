test_that("lmabc has correct class in only continuous", {
	f <- f_contY_contX
	expect_s3_class(lmabc(f, df), "lmabc")
})

test_that("lmabc does not have class 'lm' in only continuous", {
	f <- f_contY_contX
	expect_failure(expect_s3_class(lmabc(f, df), "lm"))
})

test_that("lmabc has correct class with some categoricals", {
	f <- f_contY_catX
	expect_s3_class(lmabc(f, df), "lmabc")
})

test_that("lmabc does not have class 'lm' with some categoricals", {
	f <- f_contY_catX
	expect_failure(expect_s3_class(lmabc(f, df), "lm"))
})

test_that("lmabc works with f_contY_contX", {
	f <- f_contY_contX
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc works with f_contY_mixedX", {
	f <- f_contY_mixedX
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc works with f_contY_catX", {
	f <- f_contY_catX
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc works with f_contY_contX.contX", {
	f <- f_contY_contX.contX
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc works with f_contY_contX.catX", {
	f <- f_contY_contX.catX
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc works with f_contY_catX.catX", {
	f <- f_contY_catX.catX
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc works with f_contY_all", {
	f <- f_contY_all
	expect_equal(helper_fitted(f, df), lm(f, df)$fitted.values)
})

test_that("lmabc returns the correct pi_hat vector without specifying", {
	f <- f_contY_contX.catX
	expect_equal(lmabc(f, df)$pi_hat,
							 list("cyl" = sapply(levels(df$cyl),
							 										 function(g) mean(df$cyl == g))))
})

test_that("lmabc returns the correct pi_hat vector with custom props", {
	f <- f_contY_contX.catX
	props <- list("cyl" = c("4" = 0.9, "6" = 0.05, "8" = 0.05))
	expect_equal(lmabc(f, df, props = props)$pi_hat,
							 props)
})

test_that("lmabc returns NULL pi_hat vector with no cat, default props", {
	f <- f_contY_contX
	expect_null(lmabc(f, df)$pi_hat)
})

test_that("lmabc returns given pi_hat vector with no cat, custom props", {
	f <- f_contY_contX
	props <- list("blah" = 1)
	expect_equal(lmabc(f, df, props = props)$pi_hat,
							 props)
})
