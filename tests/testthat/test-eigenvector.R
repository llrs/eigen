test_that("eigenvector works", {
    C <- matrix(c(1,2,3,2,5,6,3,6,10), 3, 3)
    e_values <- c(14.9330343736593, 1, 0.0669656263407531)
    e_vector <- eigenvector(e_values, C)
    expect_equal(e_vector %*% diag(e_values) %*% t(e_vector), C)
})
