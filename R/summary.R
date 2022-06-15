summary.lm.abc = function(fit){
	# fixme!
	ses = sqrt(diag(vcov(fit)))
}

vcov.lm.abc = function(fit){
	fit$sigma^2*fit$cov.unscaled
}
