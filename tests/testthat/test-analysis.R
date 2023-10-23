obj <- load_screenshot_data("../testdata/original")

test_that("screenshot analysis works", {
  create_screenshot_summary(obj, 0)
})
