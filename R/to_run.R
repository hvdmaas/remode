# install.packages("devtools)
# library(devtools)
#
# use_mit_license()

# install.packages("roxygen2)
# library(roxygen2)
#
# document()

# export(remode)
# export(barplot.remode_result)
# export(remode_robustness)



# use_testthat()
# use_test("remode")
## paste following into script:

#
# #Test if remode() returns the correct result --------
# #Define the expected result for comparison
# expected_result <- list(
#   nr_of_modes = 2,
#   modes = c(8, 3),
#   xt = c(70, 80, 110, 30, 70, 100, 90, 120),
#   alpha = 0.05,
#   alpha_correction = c("none", "max_modes")
# )
# class(expected_result) <- "remode_result"
# test_that("remode() returns correct result", {
#   result <- remode(c(70, 80, 110, 30, 70, 100, 90, 120))
#
#   # Check if result is an S3 class
#   expect_s3_class(result, "remode_result")
#
#   # Check if the number of modes is correct
#   expect_equal(result$nr_of_modes, expected_result$nr_of_modes)
#
#   # Check if the modes are correct
#   expect_equal(result$modes, expected_result$modes)
#
#   # Check if the input data is correct
#   expect_equal(result$xt, expected_result$xt)
#
#   # Check if the alpha value is correct
#   expect_equal(result$alpha, expected_result$alpha)
#
#   # Check if the alpha correction method is correct
#   expect_equal(result$alpha_correction, expected_result$alpha_correction)
# })

# # remode returns warning about data format
# test_that("remode() returns warning if c > 50 and no warning if c < 50", {
# expect_warning(remode(1:51), "The length of your data suggests it might be in raw format and not a frequency table.
# If so, please specify by setting format_raw = TRUE.")
#
# expect_no_warning(remode(1:49), message = "raw")
# })
#

# # remode prints output to console if check = TRUE
# ?



# use_test("barplot.remode_result")
## paste following into script:

# library(vdiffr)
# test_that("barplot.remode_result produces correct plot", {
#   result <- remode(c(70, 80, 110, 30, 70, 100, 90, 120))
#
#   # Capture the plot output
#   plot_capture <- recordPlot({
#     barplot(result)
#   })
#
#   # Test if the title is correct
#   expect_true(grepl("Modes = 2", plot_capture$main))
#
#   # Using vdiffr to compare the plot
#   vdiffr::expect_doppelganger("base R barplot of remode_result", function() barplot(result))
# })




# use_test("remode_robustness")
## paste following into script:

# remode robustness returns correct result when run with seed
#test_that("remode_robustness() returns correct result", {
#  set.seed(123)
#  robustness_result <- remode_robustness(remode(c(70, 80, 110, 90)))
#
#  # check if jacknife_df is of correct dimensions
#  expect_output(str(robustness_result$jacknife_df), "21 obs")
#  expect_output(str(robustness_result$jacknife_df), "4 variables")
#
#  # check if results correct
#  expect_equal(robustness_result$robust_until, 95)
#  expect_equal(robustness_result$jacknife_df[,1], c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
#  expect_equal(robustness_result$jacknife_df[,2], rep(1,21))
#  expect_equal(robustness_result$jacknife_df[,3], c(rep(1,20),0))
#  expect_equal(robustness_result$jacknife_df[,4], c(rep(1,20),0))
#})
#
# remode_robustness returns plot if plot = TRUE
# # use vdiffr?


