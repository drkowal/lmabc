test_that("glmabc has correct class in only continuous", {
	f <- f_contY_contX
	expect_s3_class(glmabc(f, df), c("glmabc", "lmabc"))
})

test_that("glmabc does not have class 'lm' in only continuous", {
	f <- f_contY_contX
	expect_failure(expect_s3_class(glmabc(f, df), "glm"))
})

test_that("glmabc has correct class with some categoricals", {
	f <- f_contY_catX
	expect_s3_class(glmabc(f, df), c("glmabc", "lmabc"))
})

test_that("glmabc does not have class 'lm' with some categoricals", {
	f <- f_contY_catX
	expect_failure(expect_s3_class(glmabc(f, df), "glm"))
})
