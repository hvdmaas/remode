# Unit tests for remode() function

#Test if remode() returns the correct result --------

# Define the expected result for comparison
expected_result <- list(
 nr_of_modes = 2,
 modes = c(8, 3),
 xt = c(70, 80, 110, 30, 70, 100, 90, 120),
 alpha = 0.05,
 alpha_correction = c("none", "max_modes")
)
class(expected_result) <- "remode_result"

test_that("remode() returns correct result", {
 result <- remode(c(70, 80, 110, 30, 70, 100, 90, 120))
 # Check if result is an S3 class
 expect_s3_class(result, "remode_result")
 # Check if the number of modes is correct
 expect_equal(result$nr_of_modes, expected_result$nr_of_modes)
 # Check if the modes are correct
 expect_equal(result$modes, expected_result$modes)
 # Check if the input data is correct
 expect_equal(result$xt, expected_result$xt)
 # Check if the alpha value is correct
 expect_equal(result$alpha, expected_result$alpha)
 # Check if the alpha correction method is correct
 expect_equal(result$alpha_correction, expected_result$alpha_correction)
})

# remode returns warning about data format
test_that("remode() returns warning if c > 50 and no warning if c < 50", {
expect_warning(remode(1:51), "raw")
expect_no_warning(remode(1:49), message = "raw")
})

# remode prints output to console if check = TRUE
# ?
