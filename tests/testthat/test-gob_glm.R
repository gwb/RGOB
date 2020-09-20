context("Test the high level `sugar` in GoB function with glm prediction")


test_that("gob works with lm as call-form", {
    N <- 100
    X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
    Z <- sample(c(0,1), N, replace=T)
    Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1)
    dat <- data.frame(Yp = Y, D=X, E=B, F=C)
    
    expect_equal(gob(glm(Y~X + B, family=gaussian), Z),
                 gob(glm(Yp ~ D + E, family=gaussian), Z, data=dat))
})

test_that("gob with glm+gaussian matches results with lm", {
    N <- 100
    X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
    Z <- sample(c(0,1), N, replace=T)
    Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1)
    dat <- data.frame(Yp = Y, D=X, E=B, F=C)
    
    expect_equal(gob(lm(Y ~ X + B), Z), gob(glm(Y ~ X + B, family=gaussian), Z))
})


