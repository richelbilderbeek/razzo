context("test-predict_n_extant_taxa")

test_that("use", {

  wkip("WIP")

  # Use MBD default interface
  lambda <- 0.5
  mu <- 0.2
  nu <- 0.5
  q <- 0.4
  crown_age <- 15.0

  n_baseline <- predict_n_extant_taxa(
    pars = c(lambda, mu, nu, q), crown_age = crown_age)

  # Expect more taxa for higher speciation rate
  testit::assert(baseline < predict_n_extant_taxa(
    pars = c(lambda * 2.0, mu, nu, q), crown_age = crown_age))

  # Expect more taxa for lower extinction rate
  testit::assert(baseline < predict_n_extant_taxa(
    pars = c(lambda, mu / 2.0, nu, q), crown_age = crown_age))

  # Expect more taxa for higher number of MBD events
  testit::assert(baseline < predict_n_extant_taxa(
    pars = c(lambda, mu, nu * 2.0, q), crown_age = crown_age))

  # Expect more taxa for more intense MBD events
  testit::assert(baseline < predict_n_extant_taxa(
    pars = c(lambda, mu, nu, q * 2.0), crown_age = crown_age))
})
