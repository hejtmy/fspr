test_that("loading works", {
  expect_silent(obj <- load_screenshot_data("../testdata/1"))
  expect_length(obj, 4)
})
