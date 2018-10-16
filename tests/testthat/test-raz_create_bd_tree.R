context("raz_create_bd_tree")

test_that("use", {

  bd_tree_filename <- tempfile()

  # No tree present yet
  testit::assert(!file.exists(bd_tree_filename))

  # Create tree
  raz_create_bd_tree(
    init_speciation_rate = 0.1,
    init_extinction_rate = 0.1,
    mbd_tree = ape::rcoal(4),
    bd_tree_filename
  )

  # TODO: Issue #9: actually create a BD tree and save it
  if (1 == 2) {
    expect_true(file.exists(bd_tree_filename))
  }
})
