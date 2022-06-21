#' @export
AIC.lm.abc = function(fit,...){
	stats::AIC(fit$lm,...)
}

#' @export
BIC.lm.abc = function(fit,...){
	stats::BIC(fit$lm,...)
}
