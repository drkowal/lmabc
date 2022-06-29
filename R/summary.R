#' Summarizing Linear ABC Model Fits
#'
#' \code{summary.lmbc} summary method for class "lmabc".
#'
#' This function uses the summary.lm functionality. To learn more about the original
#' functionality go to \code{\link[stats]{summary.lm}}.

#' @param object An object of class "lmabc", usually, a direct result of a call to lmabc
#' @inheritParams stats::summary.lm
#'
#' @return A list of summary statistics of the fitted linear ABC
#' model given in object.

#' @export
summary.lmabc = function(object, ...){
	ses = sqrt(diag(vcov(object)))  # this is the second column of the coefficient table in the output. i figured this out by running sqrt(diag(vcov(fit_base))), then seeing if there was any overlap with the output of summary(fit_base), which there was!
	summary_base <- summary(object$lm)  # a lot of the information is the same between the base summary and the abc summary

	# if you examine the str() of a summary object—with the code that Kowal gave us, you could try str(summary(fit_base))—you'll see that it is a named list. The names are call, terms, residuals, etc. Look through this output and figure out what we can use and what we need to change. For instance, we can use r.squared, so there's no need to change that. BUT we can't use coefficients, so you'll need to assign a new coefficients matrix to that spot in the list. Here's an example to get you started: summary_base$coefficients <- (generate new coefficient matrix)
	# eventually, we'll have to figure out if we can leave the class of summary_base as "summary.lm". We may have to create a new class, "summary.lmabc", if this doesn't work, along with print.summary.lmabc. But that's an issue for later! First let's get summary.lmabc working
}

#' @export
vcov.lmabc = function(object, ...){
	object$sigma^2*object$cov.unscaled
}
