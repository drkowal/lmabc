#' @export
AIC.lmabc = function(object, ..., k = 2){
	stats::AIC(object$lm, ..., k = k)
}

#' @importFrom stats BIC
#' @export
BIC.lmabc = function(object, ...){
	stats::BIC(object$lm, ...)
}
