#' @export
AIC.lm.abc = function(object,...,k=2){
	stats::AIC(object$lm,...,k=k)
}

#' @method BIC lm.abc
#' @export
BIC.lm.abc = function(object,...){
	stats::BIC(object$lm,...)
}
