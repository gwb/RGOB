context("Tests the utilities functions for extracting predictor dataframes")

test_that("predictor dataframes can be extracted from environment", {
    A <- c(1,2,3,4); B <- c(5,6,7,8); C <- c(1,0,1,0);
    res <- data.frame(B = c(5, 6, 7, 8), C = c(1, 0, 1, 0))
    expect_equal(build_predictor_dataframe(A ~ B + C), res)
})

test_that("predictor dataframes can be extracted from larger dataframes", {
    dat <- data.frame(A=c(1,2,3,4), B=c(5,6,7,8), C=c(1,0,1,0))
    res <- data.frame(B = c(5, 6, 7, 8), C = c(1, 0, 1, 0))
    expect_equal(build_predictor_dataframe_from_data(A ~ B + C, dat), res)
})
