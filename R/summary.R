#' Summarizing Linear ABC Model Fits
#'
#' \code{summary} method for class "lmabc".
#'
#' @param object An object of class "lmabc", usually, a direct result of a call to [lmabc::lm_abc]
#' @inheritParams stats::summary.lm
#'
#' @returns A list of summary statistics of the fitted linear ABC model given in object.
#' @seealso [base::summary()], [stats::summary.lm()]
#' @export
summary.lmabc <- function(object, correlation = FALSE, symbolic.cor = FALSE, ...) {
	summary_base <- summary(object$lm, correlation = correlation, symbolic.cor = symbolic.cor, ...) # a lot of the information is the same between the base summary and the abc summary

	term_coeff <- object$coefficients # extracting all the coefficents from the lmabc model
	ses <- sqrt(diag(vcov(object))) # Calculating the new standard error
	t_val <- term_coeff/ses # New t-values
	p_val <- 2*pt(abs(t_val), object$df.residual, lower.tail = FALSE) # new probabilities calculated here

	coefficients_abc <- cbind(term_coeff, ses, t_val, p_val) # entering the new values of the coeff matrix
	# renaming the dimensions of the new coeff matrix
	dimnames(coefficients_abc) <- list(names(term_coeff), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

	summary_abc <- summary_base; attr(summary_abc, 'class') <- "summary.lmabc"
	summary_abc$summary.lm <- summary_base
	summary_abc$coefficients <- coefficients_abc
	summary_abc$call <- object$call
	summary_abc$cov.unscaled <- object$cov.unscaled

	if (correlation) {
		summary_abc$correlation <- stats::cov2cor(object$cov.unscaled)
		summary_abc$symbolic.cor <- symbolic.cor
	} else {
		summary_abc$correlation <- NULL
		summary_abc$symbolic.cor <- NULL
	}

	summary_abc
}

#' @export
vcov.lmabc = function(object, ...){
	object$sigma^2*object$cov.unscaled
}

#' @export
nobs.lmabc <- function(object, ...) {
	stats::nobs(object$lm, ...)
}

#' @export
print.summary.lmabc <- function(x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, signif.stars = getOption("show.signif.stars"), ...) {
	print.summary.lm <- utils::getFromNamespace("print.summary.lm", "stats")
	print.summary.lm(x = x, digits = digits, symbolic.cor = symbolic.cor, signif.stars = signif.stars, ...)
}

#' @export
summary.glmabc <- function(object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, ...) {
	summary_base <- summary(object$glm, ...) # a lot of the information is the same between the base summary and the abc summary

	term_coeff <- object$coefficients # extracting all the coefficents from the lmabc model
	ses <- sqrt(diag(object$cov.unscaled)) # Calculating the new standard error
	t_val <- term_coeff/ses # New t-values
	p_val <- 2*pt(abs(t_val), object$df.residual, lower.tail = FALSE) # new probabilities calculated here

	coefficients_abc <- cbind(term_coeff, ses, t_val, p_val) # entering the new values of the coeff matrix
	# renaming the dimensions of the new coeff matrix
	dimnames(coefficients_abc) <- list(names(term_coeff), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

	summary_abc <- summary_base ; attr(summary_abc, 'class') <- "summary.glmabc"
	summary_abc$summary.glm <- summary_base
	summary_abc$coefficients <- coefficients_abc
	summary_abc$call <- object$call
	summary_abc$cov.unscaled <- object$cov.unscaled
	summary_abc$cov.scaled <- summary_base$dispersion * summary_abc$cov.unscaled

	summary_abc
}

#' @export
print.summary.glmabc <- function(x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, signif.stars = getOption("show.signif.stars"), ...) {
	print.summary.glm <- utils::getFromNamespace("print.summary.glm", "stats")
	print.summary.glm(x = x, digits = digits, symbolic.cor = symbolic.cor, signif.stars = signif.stars, ...)
}
