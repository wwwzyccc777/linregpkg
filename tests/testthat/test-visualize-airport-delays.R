test_that("airport delay visualization returns a ggplot", {
  plot <- visualize_airport_delays()

  expect_s3_class(plot, "ggplot")
  expect_match(plot$labels$title, "Mean arrival delay")
  expect_match(plot$labels$x, "Longitude")
  expect_match(plot$labels$y, "Latitude")
})
