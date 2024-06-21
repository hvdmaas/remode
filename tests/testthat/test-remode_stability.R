# Unit tests for remode_stability function

# remode stability returns correct result when run with seed
test_that("remode_stability() returns correct result", {
  set.seed(123)
  stability_result <- remode_stability(remode(c(70, 80, 110, 90, 100, 20)), percentage_steps=20)

  # check if num_mode_stability is of correct dimensions
  expect_output(str(stability_result$num_mode_stability), "21 obs")
  expect_output(str(stability_result$num_mode_stability), "4 variables")

  # check if results correct
  expect_equal(stability_result$stable_until, 90)
  expect_equal(stability_result$num_mode_stability[,1],  seq(0,100,by=5))
  expect_equal(round(stability_result$num_mode_stability[,2],0), c(rep(1,20),0))
  expect_equal(stability_result$num_mode_stability[,3], c(rep(1,20),0))
  expect_equal(stability_result$num_mode_stability[,4], c(rep(1,20),0))
  })

