#' @export
predict.lm.abc = function(fit,...){
	stats::predict.lm(fit$lm,...)
}
