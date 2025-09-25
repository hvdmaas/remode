# Unit tests for remode() function

#Test if remode() returns the correct result --------

# Define the expected result for comparison
expected_result <- list(
 nr_of_modes = 2,
 mode_indeces = c(8, 3),
 frequency_input_data = c(70, 80, 110, 30, 70, 100, 90, 120),
 alpha = 0.05
)
class(expected_result) <- "remode_result"

test_that("remode() returns correct result", {
 result <- remode(c(70, 80, 110, 30, 70, 100, 90, 120))
 # Check if result is an S3 class
 expect_s3_class(result, "remode_result")
 # Check if the number of modes is correct
 expect_equal(result$nr_of_modes, expected_result$nr_of_modes)
 # Check if the modes are correct
 expect_equal(result$mode_indeces, expected_result$mode_indeces)
 # Check if the input data is correct
 expect_equal(result$frequency_input_data, expected_result$frequency_input_data)
 # Check if the alpha value is correct
 expect_equal(result$alpha, expected_result$alpha)
})

# remode returns warning about data format
test_that("remode() returns warning if c > 50 and no warning if c < 50", {
expect_warning(remode(1:51), "raw")
expect_no_warning(remode(1:49), message = "raw")
})
