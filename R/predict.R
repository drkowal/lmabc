#' @export
predict.lmabc = function(object,...){
	stats::predict.lm(object$lm,...)
}

#' @export
predict.glmabc = function(object, newdata = NULL, type = c("link", "response", "terms"),
													se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass, ...){
	mc <- as.list(match.call(expand.dots = T)[-1])
	mc$object <- object$glm
	do.call(stats::predict.glm, mc)
}
