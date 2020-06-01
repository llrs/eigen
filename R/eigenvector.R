#' Eigenvector from the eigenvalues
#'
#' @param x Numeric vector of the eigenvalues
#'
#' @return The eigenvector for each eigenvalue
#' @export
#'
#' @examples
#' C <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3)
#' D <- matrix(c(1, 1, -1, 1, 3, 1, -1, 1, 3), 3, 3)
#' eigenvector(c(14.9330343736593, 1, 0.0669656263407531), C)
eigenvector <- function(x, A) {
    if (!(is.numeric(x) & is.vector(x))) {
        stop("It should be a numeric vector.")
    }
    n <- length(x)
    if (nrow(A) != ncol(A) & !is.matrix(A)) {
        stop("C should be a square matrix.")
    }
    if (nrow(A) != n) {
        stop("The eigenvalues must be of the same length as the diagonal of the matrix")
    }

    # On the right more or less is this
    l <- vector("list", length = n)
    for (i in seq_len(n-1)) {
        M <- A[-i, -i]
        e <- eigen(M, symmetric = TRUE, only.values = TRUE)
        l[[i]] <- x[i] - e$values
    }


    # On the left side more or less
    l <- vector("list", length = n)
    for (i in seq_len(n)) {
        for (j in seq_len(n)) {
            if (i == j){
                next
            }
            l[[i]] <- prod(x[i]-x[j])
        }
    }


}
