test_that("lmabc has correct class in only continuous", {
	skip(message = "Current design decision means this will fail")
	f <- f_contY_contX
	expect_s3_class(lmabc(f, df), "lmabc")
})

test_that("lmabc has correct class with some categoricals", {
	f <- f_contY_catX
	expect_s3_class(lmabc(f, df), "lmabc")
})

test_that("lmabc works with f_contY_contX", {
	skip(message = "Current design decision means this will fail")
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
	skip(message = "Current design decision means this will fail")
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
