model_frame <- function(formula, data) {
	mf <- match.call(call = sys.call(which = -1))
	m <- match(c("formula", "data"), names(mf), 0L)
	mf <- mf[c(1L, m)]
	mf$drop.unused.levels <- TRUE
	mf[[1L]] <- quote(stats::model.frame)
	mf <- eval(mf, parent.frame())
	attr(mf, "terms") <- NULL
	mf
}
