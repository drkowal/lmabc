#' @export
logLik.lmabc <- function(object, REML = FALSE, ...) {
	stats::logLik(object$lm, REML = REML, ...)
}
