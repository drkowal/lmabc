test_that("predict.lmabc works with f_contY_contX", {
	coefs <- as.matrix(coef(lmabc(f_contY_contX, df)))
	X <- as.matrix(cbind(1, df[,rownames(coefs)[-1]]))
	expect_equal((X %*% coefs)[,1],
							 predict(lm(f_contY_contX, df), df))
})
