#' @export
predict.lmabc = function(object,...){
	stats::predict.lm(object$lm,...)
}

#' @export
predict.glmabc = function(object, newdata = NULL, type = c("link", "response", "terms"),
													se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass, ...){
	stats::predict.glm(object$glm,...)
}
