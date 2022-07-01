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
	term_coeff = object$coefficients # extracting all the coefficients from the lmabc model
	t_val = term_coeff/ses # New t-values
	p_val = 2*pt(abs(t_val), object$df.residual, lower.tail = FALSE) # new probabilities calculated here

	summary_base$coefficients <- cbind(term_coeff, ses, t_val, p_val) # entering the new values of the coeff matrix
	# renaming the dimensions of the new coeff matrix
	dimnames(summary_base$coefficients)<-
		list(names(term_coeff),
				 c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

	# New correlation matrix
	if (correlation) {
		summary_base$correlation <- stats::cov2cor(object$cov.unscaled)
		# rdf <- object$df.residual
		# p <- object$rank
		# p1 <- 1L:p
		# R <- chol2inv(object$qr$qr[p1, p1, drop = FALSE])
		# rss <- sum(object$residuals^2)
		# resvar <- rss/rdf

		# summary_base$correlation <- (R * resvar)/outer(ses, ses)
		# dimnames(summary_base$correlation) <- dimnames(object$cov.unscaled)
		summary_base$symbolic.cor <- symbolic.cor
	}
	# class of the returned object is still summary.lm, is this something we want
	# to change?
	# class(summary_base) <- "summary.lmabc"
	summary_base
}

#' @export
vcov.lmabc = function(object, ...){
	object$sigma^2*object$cov.unscaled
}
