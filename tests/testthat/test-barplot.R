# Unit tests for barplot of remode result

# library(vdiffr)
#
# test_that("barplot.remode_result produces correct plot", {
#  result <- remode(c(70, 80, 110, 30, 70, 100, 90, 120))
#  # Capture the plot output
#  plot_capture <- recordPlot({
#    barplot(result)
#  })
#  # Test if the title is correct
#  expect_true(grepl("Modes = 2", plot_capture$main))
#  # Using vdiffr to compare the plot
#  vdiffr::expect_doppelganger("base R barplot of remode_result", function() barplot(result))
# })
