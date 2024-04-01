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

test_that("getConstraints works with cont:cont missing main effect", {
	expect_null(getConstraints(formula(mpg ~ hp:disp), df))
})

test_that("getConstraints works with cat and cont:cont missing main effect", {
	expect_equal(dim(getConstraints(formula(mpg ~ gear + hp:disp), df)), c(1, 5))
})

test_that("getConstraints fails with cont:cat missing main cont effect", {
	expect_error(getConstraints(formula(mpg ~ gear + hp:gear), df))
})

test_that("getConstraints fails with cont:cat missing main cat effect", {
	expect_error(getConstraints(formula(mpg ~ hp + hp:gear), df))
})

test_that("getConstraints fails with cont:cat missing main cont and cat effects", {
	expect_error(getConstraints(formula(mpg ~ hp:gear), df))
})

test_that("getConstraints fails with cat:cat missing main cat effect", {
	expect_error(getConstraints(formula(mpg ~ cyl + cyl:gear), df))
})

test_that("getConstraints fails with cat:cat missing both main cat effects", {
	expect_error(getConstraints(formula(mpg ~ cyl:gear), df))
})
