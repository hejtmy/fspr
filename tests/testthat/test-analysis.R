obj <- load_screenshot_data("../testdata/original")

test_that("screenshot analysis works", {
  expect_silent(res <- create_screenshot_summary(obj, 0))
  expect_equal(nrow(res), 1)
})

test_that("multiple screenshot analysis works", {
  expect_silent(res <- create_screenshot_summaries(obj))
})