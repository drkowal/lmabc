#' @export
print.lmabc <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
	print.lm <- utils::getFromNamespace("print.lm", "stats")
	print.lm(x = x, digits = digits, ...)
}
