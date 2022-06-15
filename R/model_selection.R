#' @export
AIC.lm.abc = function(fit,...){
	AIC(fit$lm,...)
}

#' @export
BIC.lm.abc = function(fit,...){
	BIC(fit$lm,...)
}
