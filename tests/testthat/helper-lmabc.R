helper_fitted <- function(f, df) {
	all_terms <- attr(terms(f), "term.labels")
	factor_terms <- which(sapply(df_used, is.factor))
	cont_terms <- setdiff(all_terms, names(factor_terms))

	df_used <- df[,all_terms]
	for (ct in cont_terms) {
		df_used[,ct] <- scale(df_used[,ct], center = TRUE, scale = FALSE)
	}

	df_factors <- df_used[,factor_terms, drop = FALSE]
	mm <- stats::model.matrix(object = f[-2],
													  data = df_used,
													  contrasts.arg = lapply(df_factors,
													 											   stats::contrasts,
													 											   contrasts=FALSE))

	coefs <- as.matrix(coef(lmabc(f, df)))
	X <- as.matrix(mm)
	(X %*% coefs)[,1]
}
