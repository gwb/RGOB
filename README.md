
# RGOB

<!-- badges: start -->
<!-- badges: end -->

This R package implements the Generalized Oaxaca Blinder framework described in 
Guo and Basse (2020). It provides utilities for computing point estimates and 
confidence intervals using common regression functions, and for constructing 
custom regression adjustment.

## Installation

Install the package from Github with:

``` r
# install.packages("devtools")
devtools::install_github("gwb/RGOB")
```

## Example

The main entry point is the `gob` function, which computes the point estimate 
and associated confidence interval, using the specified regression function. 
The code below gives a simple example of how to ajdust using a linear model:

``` r
library(RGOB)

set.seed(123) # for replicability

## Generates synthetic data
N <- 100 # the number of units
X <- rnorm(N); B <- rnorm(N); C <- rnorm(N)
Z <- sample(c(0,1), N, replace=T) # the binary assignment vector
Y <- 1 + 0.2 * X + 0.5 * Z + rnorm(N, sd=0.1) # the observed outcomes
Yb <- ifelse(Y > 1.2, 1, 0) # a binary observed outcome
Yp <- rpois(N, lambda=Y) # integer observed outcomes

## Computes the point estimate and associated confidence interval
## using the separate slope adjustment (same as in Lin 2013)
gob(lm(Y ~ X + B), Z)

## Logistic regression adjustment
gob(glm(Yb ~ X + B, family=binomial(link="logit")), Z)

## Poisson regression adjustment
gob(glm(Yp ~ X + B, family=poisson), Z)
```

