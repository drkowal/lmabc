#' @export
predict.lm.abc = function(object,...){
	stats::predict.lm(object$lm,...)
}
