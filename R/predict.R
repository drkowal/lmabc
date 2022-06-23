#' @export
predict.lmabc = function(object,...){
	stats::predict.lm(object$lm,...)
}
