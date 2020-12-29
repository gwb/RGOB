context("Test the high level `sugar` in GoB function with lm prediction")

test_that("gob works with lm as call-form", {
    N <- 100
    X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
    Z <- sample(c(0,1), N, replace=T)
    Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1)
    dat <- data.frame(Yp = Y, D=X, E=B, F=C)
    
    expect_equal(gob(lm(Y~X + B), Z), gob(lm(Yp ~ D + E), Z, data=dat))
})

test_that("gob returns same result with lm and lm.pred call forms", {
    N <- 100
    X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
    Z <- sample(c(0,1), N, replace=T)
    Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1)

    expect_equal(gob(lm(Y ~ X + B),Z), gob(Y ~ X + B, Z, lm.pred(Y ~ X + B)))
})
