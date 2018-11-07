context("test-raz_calc_mut_rates")

test_that("use, simple case", {

  # For two identical trees, both mutation rates will be 1 / crown_age

  #   +---------------- A                                                       # nolint this is no code
  # +-+
  # | +---------------- B
  # +
  # | +---------------- C
  # +-+
  #   +---------------- D                                                       # nolint this is no code
  tree <- ape::read.tree(text = "((A:9, B:9):1, (C:9, D:9):1);")

  skip("TODO: Issue 61, #61")
  mut_rates <- raz_calc_mut_rates(mbd_tree = tree, bd_tree = tree)

  # Identical trees must have same mutation rate
  expect_equal(mut_rates[1], mut_rates[2])

  # Mutation rate so to maximize information
  crown_age <- 10.0
  max_mut_rate <- 1.0 / crown_age
  expected_mut_rate <- max_mut_rate

  # Both trees have that
  expect_equal(mut_rates[1], expected_mut_rate)

})

test_that("use", {

  # Tree that has all branching events close to root
  # Sum of branch lengths: 38
  #
  #   +---------------- A                                                       # nolint this is no code
  # +-+
  # | +---------------- B
  # +
  # | +---------------- C
  # +-+
  #   +---------------- D                                                       # nolint this is no code
  early_tree <- ape::read.tree(text = "((A:9, B:9):1, (C:9, D:9):1);")

  # Tree that has all branching events far from root
  # Sum of branch lengths: 22
  #
  #                  +- A                                                       # nolint this is no code
  # +----------------+
  # |                +- B
  # +
  # |                +- C
  # +----------------+
  #                  +- D                                                       # nolint this is no code
  late_tree <- ape::read.tree(text = "((A:1, B:1):9, (C:1, D:1):9);")

  skip("TODO: Issue 61, #61")
  mut_rates <- raz_calc_mut_rates(mbd_tree = early_tree, bd_tree = late_tree)

  # Mutation rate so to maximize information
  crown_age <- 10.0
  max_mut_rate <- 1.0 / crown_age

  # The 'early' tree will have a lower mutation rate,
  # so that the number of mutations -the information content-
  # of both trees is equal
  #
  # mut_rates[1] must be 22/38 of mut_rates[2]
  expect_true(mut_rates[1] < mut_rates[2])

  # The 'late' tree will have the maximum mutation rate,
  # because it has lower sum of branch lengths
  expect_equal(mut_rates[2], max_mut_rate)
})
