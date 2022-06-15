#' @export
mrank = function(M, tol = 10^-8) sum(svd(M)$d^2 > tol)
