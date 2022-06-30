#' @export
print.lmabc <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
	stats:::print.lm(x = x, digits = digits, ...)
}
