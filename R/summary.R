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
	# fixme!
	ses = sqrt(diag(vcov(object)))
	class(object) = "lm"
	summary(object, ...) # new line here
}

#' @export
vcov.lmabc = function(object){
	object$sigma^2*object$cov.unscaled
}
