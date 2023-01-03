helper_fitted <- function(f, df) {
	df_used <- df[,attr(terms(f), "term.labels")]
	df_factors <- df_used[,c(which(sapply(df_used, is.factor))), drop = FALSE]
	mm <- stats::model.matrix(object = f[-2],
													  data = df_used,
													  contrasts.arg = lapply(df_factors,
													 											   stats::contrasts,
													 											   contrasts=FALSE))

	coefs <- as.matrix(coef(lmabc(f, df)))
	X <- as.matrix(mm)
	(X %*% coefs)[,1]
}
