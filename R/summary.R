#' @export
summary.lmabc = function(object, ...){
	# fixme!
	print("heloo")
	ses = sqrt(diag(vcov(object)))
	summary(object$lm) # new line here
}

#' @export
vcov.lmabc = function(object){
	object$sigma^2*object$cov.unscaled
}
