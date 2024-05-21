# Unit tests for remode_robustness function

# remode robustness returns correct result when run with seed
test_that("remode_robustness() returns correct result", {
  set.seed(123)
  robustness_result <- remode_robustness(remode(c(70, 80, 110, 90)))

  # check if jacknife_df is of correct dimensions
  expect_output(str(robustness_result$jacknife_df), "21 obs")
  expect_output(str(robustness_result$jacknife_df), "4 variables")

  # check if results correct
  expect_equal(robustness_result$robust_until, 95)
  expect_equal(robustness_result$jacknife_df[,1], c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
  expect_equal(round(robustness_result$jacknife_df[,2],1), c(rep(1,20),0))
  expect_equal(robustness_result$jacknife_df[,3], c(rep(1,20),0))
  expect_equal(robustness_result$jacknife_df[,4], c(rep(1,20),0))
})

# remode_robustness returns plot if plot = TRUE
# # use vdiffr?
