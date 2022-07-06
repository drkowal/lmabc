#' @export
logLik.lmabc <- function(object, REML = FALSE, ...) {
	logLik(object$lm, REML = REML, ...)
}
