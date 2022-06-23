#' @export
summary.lmabc = function(object, ...){
	# fixme!
	ses = sqrt(diag(stats::vcov(object)))
}

#' @export
vcov.lmabc = function(object, ...){
	object$sigma^2*object$cov.unscaled
}
