#' Summarizing Linear ABC Model Fits
#'
#' \code{summary} method for class "lmabc".
#'
#' @param object An object of class "lmabc", usually, a direct result of a call to [lmabc::lm_abc]
#' @inheritParams stats::summary.lm
#'
#' @returns A list of summary statistics of the fitted linear ABC model given in object.
#' @seealso [stats::summary.lm]
#' @export
summary.lmabc = function(object, correlation = FALSE, symbolic.cor = FALSE, ...){
	# New coefficient matrix
	summary_base <- summary(object$lm) # a lot of the information is the same between the base summary and the abc summary

	ses = sqrt(diag(vcov(object))) # Calculating the new standard error
	term_coeff = object$coefficients # extracting all the coefficents from the lmabc model
	t_val = term_coeff/ses # New t-values
	p_val = 2*pt(abs(t_val), object$df.residual, lower.tail = FALSE) # new probabilities calculated here

	coefficients_abc <- cbind(term_coeff, ses, t_val, p_val) # entering the new values of the coeff matrix
	# renaming the dimensions of the new coeff matrix
	dimnames(coefficients_abc)<-
		list(names(term_coeff),
				 c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

	summary_abc <- summary_base; attr(summary_abc, 'class') <- "summary.lmabc"
	summary_abc$summary.lm <- summary_base
	summary_abc$coefficients <- coefficients_abc

	# New correlation matrix
	if (correlation) {
		rdf <- object$df.residual
		p <- object$rank
		p1 <- 1L:p
		R <- chol2inv(object$qr$qr[p1, p1, drop = FALSE])
		rss <- sum(object$residuals^2)
		resvar <- rss/rdf

		correlation <- (R * resvar)/outer(ses, ses)
		dimnames(correlation) <- dimnames(summary_base$cov.unscaled)

		summary_abc$correlation <- correlation
		summary_abc$symbolic.cor <- symbolic.cor
	}

	summary_abc
}

#' @export
vcov.lmabc = function(object, ...){
	object$sigma^2*object$cov.unscaled
}
