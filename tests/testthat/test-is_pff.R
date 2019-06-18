# is_pff: Is Peregrine Friendly Filename

test_that("use", {
  expect_true(is_pff(filename = "/home/p230198/Parameters.RDa"))
  expect_false(is_pff(filename = "/local/tmp/mbd.log"))
  expect_false(is_pff(filename = "/tmp/mbd.log"))
})
