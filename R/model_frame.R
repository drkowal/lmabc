#' Wrapper for model.frame
#'
#' `model.frame` returns a data.frame object for use in [lmabc()] and similar functions.
#' It reimplements the first few lines of [lm()] to properly format the `formula` and `data` parameters.
#' If only `formula` is passed, all variables must be vectors in the environment.
#' If `formula` and `data` are passed, variables are first pulled from `data`, then filled from the environment.
#'
#' @param formula formula object
#' @param data data.frame object
#'
#' @return a data.frame with the variables in `formula`
model_frame <- function(formula, data) {
	# set up call
	mf <- match.call(call = sys.call(which = -1))
	m <- match(c("formula", "data"), names(mf), 0L)
	mf <- mf[c(1L, m)]
	mf$drop.unused.levels <- TRUE

	# change function to stats::model.frame
	mf[[1L]] <- quote(stats::model.frame)

	# call stats::model.frame
	mf <- eval(mf, parent.frame())

	# remove "terms" attribute to avoid confusion
	attr(mf, "terms") <- NULL

	mf
}
