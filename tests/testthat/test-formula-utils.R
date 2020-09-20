context("Tests the utilities functions for dealing with formulas")

test_that("formulas with one predictor can be identified", {
    expect_equal(is_one_predictor_formula(Y ~ X + B), FALSE)
    expect_equal(is_one_predictor_formula(Y ~ X), TRUE)
    expect_equal(is_one_predictor_formula(Y ~ .), FALSE)

    form.1 <- Y ~ X + B; form.2 <- Y~X; form.3 <- Y~.
    
    expect_equal(is_one_predictor_formula(form.1), FALSE)
    expect_equal(is_one_predictor_formula(form.2), TRUE)
    expect_equal(is_one_predictor_formula(form.3), FALSE)
})


test_that("formulas with a dot predictor can be identified", {
    expect_equal(is_dot_predictor_formula(Y ~ B), FALSE)
    expect_equal(is_dot_predictor_formula(Y ~ B + X), FALSE)
    expect_equal(is_dot_predictor_formula(. ~ B), FALSE)
    expect_equal(is_dot_predictor_formula(Y ~ .), TRUE)

    form.1 <- Y ~ B; form.2 <- Y ~ B + X; form.3 <- . ~ B; form.4 <- Y~.

    expect_equal(is_dot_predictor_formula(form.1), FALSE)
    expect_equal(is_dot_predictor_formula(form.2), FALSE)
    expect_equal(is_dot_predictor_formula(form.3), FALSE)
    expect_equal(is_dot_predictor_formula(form.4), TRUE)
    
})

test_that("formulas can be converted to strings", {
    expect_equal(formula_to_string(Y~.),  "Y ~ .")
    expect_equal(formula_to_string(Y~B),  "Y ~ B")    
    expect_equal(formula_to_string(Y~B + X),  "Y ~ B + X")

    form.1 <- Y~.; form.2 <- Y~B; form.3 <- Y~B+X
    expect_equal(formula_to_string(form.1), "Y ~ .")
    expect_equal(formula_to_string(form.2),  "Y ~ B")    
    expect_equal(formula_to_string(form.3),  "Y ~ B + X")
})

test_that("predictor names are extracted correctly", {
    expect_equal(get_predictor_names(Y ~ X), "X")
    expect_equal(get_predictor_names(Y ~ X + B), c("X", "B"))
    expect_equal(get_predictor_names(Y ~ .), ".")

    form.1 <- Y ~ X; form.2 <- Y ~ X + B; form.3 <- Y ~ .

    expect_equal(get_predictor_names(form.1), "X")
    expect_equal(get_predictor_names(form.2), c("X", "B"))
    expect_equal(get_predictor_names(form.3), ".")
})
