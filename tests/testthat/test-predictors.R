context("Test the predictor-generating functions")

test_that("The lm.pred function works as intended for scalar predictors", {
    N <- 100
    X <- rnorm(N)
    Z <- sample(c(0,1), size=N, replace=TRUE)
    Y <- 1 + 0.2 * X + 0.5*Z
    mu_hat_ls <- lm.pred(Y~X)
    mu_hat_0 <- mu_hat_ls[[1]](Y[Z==0], X[Z==0])
    mu_hat_1 <- mu_hat_ls[[2]](Y[Z==1], X[Z==1])

    expect_equal(mu_hat_0(0.2), 1.04)
    expect_equal(mu_hat_1(0.1), 1.52)
})
    
