#' @export
AIC.lmabc = function(object,...,k=2){
	stats::AIC(object$lm,...,k=k)
}

#' @export
BIC.lmabc = function(object,...){
	stats::BIC(object$lm,...)
}
