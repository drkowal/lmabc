test_that("getConstraints returns NULL with no cat and no interactions", {
  expect_null(getConstraints(f_contY_contX, df))
})

test_that("getConstraints works with cat and no interactions", {
	expect_equal(dim(getConstraints(f_contY_mixedX, df)), c(1, 5))
})

test_that("getConstraints works with cat and cont:cont interactions", {
	expect_equal(dim(getConstraints(f_contY_mixedX_contX.contX, df)), c(1, 7))
})

test_that("getConstraints works with cont:cat interactions and all main effects", {
	expect_equal(dim(getConstraints(f_contY_contX.catX, df)), c(2, 8))
})

test_that("getConstraints works with cat:cat interactions and all main effects", {
	expect_equal(dim(getConstraints(f_contY_catX.catX, df)), c(7, 16))
})
