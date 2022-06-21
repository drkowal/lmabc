#' @export
summary.lm.abc = function(fit){
	# fixme!
	ses = sqrt(diag(stats::vcov(fit)))
}

#' @export
vcov.lm.abc = function(fit){
	fit$sigma^2*fit$cov.unscaled
}
