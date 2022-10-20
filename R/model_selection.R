#' @export
logLik.lmabc <- function(object, REML = FALSE, ...) {
	stats::logLik(object$lm, REML = REML, ...)
}

#' @export
logLik.glmabc <- function(object, REML = FALSE, ...) {
	stats::logLik(object$glm, REML = REML, ...)
}
