test_that("getConstraints works with no cat and no interactions", {
  expect_null(getConstraints(f_contY_contX, df))
})

test_that("getConstraints works with cat and no interactions", {
	expect_equal(dim(getConstraints(f_contY_mixedX, df)), c(1, 5))
})

test_that("getConstraints works with cat and cont:cont interactions", {
	expect_equal(dim(getConstraints(f_contY_mixedX_contX.contX, df)), c(1, 7))
})
