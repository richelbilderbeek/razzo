context("test-raz_create_fig_1")

test_that("use", {
  skip("TODO. Issue 79, #79")

  plot <- raz_creat_fig_1(raz_get_path("razzo_project"))


    expect_true(is.factor(df$site_model))
})

test_that("abuse", {
  skip("TODO. Issue 79, #79")
  expect_error(
    raz_collect_nltt_stats("nonsense"),
    "'folder_name' must end with 'razzo_project'"
  )
})
