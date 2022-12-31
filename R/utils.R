mrank <- function(M, tol = 10^-8) {
	sum(svd(M)$d^2 > tol)
}

abort <- function(..., call. = TRUE, domain = NULL) {
	if (requireNamespace("rlang", quietly = TRUE)) {
		rlang::abort(...)
	} else {
		stop(..., call. = call., domain = domain)
	}
}
