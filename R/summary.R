#' @export
summary.lm.abc = function(fit){
	# fixme!
	ses = sqrt(diag(vcov(fit)))
	summary(fit) # new line here
}

#' @export
vcov.lm.abc = function(fit){
	fit$sigma^2*fit$cov.unscaled
}
