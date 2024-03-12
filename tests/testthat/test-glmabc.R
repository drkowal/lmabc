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
