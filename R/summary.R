#' @export
summary.lm.abc = function(object, ...){
	# fixme!
	ses = sqrt(diag(stats::vcov(object)))
}

#' @export
vcov.lm.abc = function(object, ...){
	object$sigma^2*object$cov.unscaled
}
