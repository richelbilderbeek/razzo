test_that("use", {
  expect_equal(
    get_tree_filename(folder_name = "", tree_type = "true"),
    "/mbd.tree"
  )
  expect_equal(
    get_tree_filename(folder_name = "", tree_type = "twin"),
    "/mbd_twin.tree"
  )
})
