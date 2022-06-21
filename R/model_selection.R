#' @export
AIC.lm.abc = function(fit,...,k=2){
	stats::AIC(fit$lm,...,k=k)
}

#' #' @export
#' BIC.lm.abc = function(fit,...){
#' 	stats::BIC(fit$lm,...)
#' }
