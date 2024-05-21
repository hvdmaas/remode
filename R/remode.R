# Code for top-level function remode() and lower level supporting functions

# Helper functions ------------------------

# Helper function 1
# define fisher test for statistical test for remode_find_maxima
perform_fisher_test <- function(candidate, left_minimum, right_minimum, x, alpha){
  p_left <- fisher.test(matrix(c(x[candidate], sum(x) - x[candidate], x[left_minimum], sum(x) - x[left_minimum]), ncol = 2),
                        alternative = "greater")$p.value
  p_right <- fisher.test(matrix(c(x[candidate], sum(x) - x[candidate], x[right_minimum], sum(x) - x[right_minimum]), ncol = 2),
                         alternative = "greater")$p.value

  if (p_left < alpha && p_right < alpha) return(candidate)
}

#  Helper function 2
# define binomial test for statistical test for remode_find_maxima
perform_binomial_test <- function(candidate, left_minimum, right_minimum, x, alpha){
  p_left <- binom.test(c(x[candidate], x[left_minimum]),alternative='greater')$p.value
  p_right <- binom.test(c(x[candidate], x[right_minimum]),alternative='greater')$p.value

  if (p_left < alpha && p_right < alpha) return(candidate)
}

# Helper function 3
# Recursive function used inside remode()
remode_find_maxima <- function(x, alpha = 0.05, check = FALSE, test_func) {

  # Early return for short vectors
  if (length(x) < 3) {
    if (check) print(paste('x =', paste(x,collapse=","),'stop'))
    return(integer(0))
  }
  if (check)
    print(paste('x =', paste(x,collapse=",")))


  candidate <- which.max(x) # position candidate maximum
  left_minimum <- which.min(x[1:candidate]) # position left minimum
  right_minimum <- which.min(x[candidate:length(x)]) + candidate - 1 # position right minimum

  if (check) {
    print(paste('locations extrema =', paste(c(left_minimum, candidate, right_minimum),collapse=",")))
  }

  # perform chosen statistical test on both sides
  result <- if ((x[candidate] > x[left_minimum]) & (x[candidate] > x[right_minimum])) {
    test_func(candidate, left_minimum, right_minimum, x, alpha)
  }

  if (check) {
    print(paste("mode detected at:", result))
  }

  # recursive calls on left and right sides of candidate
  c(result,
    remode_find_maxima(x[1:(candidate - 1)], alpha = alpha, check = check, test_func = test_func),
    remode_find_maxima(x[(candidate + 1):length(x)], alpha = alpha, check = check, test_func = test_func) + candidate)
}


# Main function ----------
#' Performs a recursive mode detection function for ordinal data
#'
#' @param xt A (non-empty) vector of input data. Either in frequencies (default),
#'           or as raw data. For raw data, set format_raw argument to TRUE.
#' @param alpha A value specifying the significance level for the statistical tests. Default is 0.05.
#' @param alpha_correction A character string or function specifying the method for correcting the alpha level.
#'                         Options are "none" (no correction;default),
#'                         "max_modes" (Bonferroni-like correction), or a user-defined function.
#' @param check A logical variable indicating whether to return input, test, and
#'          outcome of each recursive step of the algorithm. Default is FALSE.
#' @param f_sign_test A character string or function specifying the statistical test to use for
#'          significance testing. Options are "fisher" (default), "binomial", or a user-defined function.
#' @param format_raw  A logical value indicating whether the input data (`xt`) is raw data.
#'         If TRUE, data will be converted to a frequency table inside the function. Default is FALSE.
#' @param levels A numeric vector specifying the categories of the (ordinal) distribution.
#'        Used for the factor conversion if `format_raw` is TRUE. Default is `seq(min(xt), max(xt))`.
#'
#' @details The function recursively detects a mode candidate (highest frequency), tests whether
#' its frequency significantly deviates from the lowest frequencies on both its left and right side.
#' If significant, the candidate is classified as a mode. The function recursively processes the segments
#' of the vector to the left and right of the mode candidate, applying the same procedure to
#' identify additional modes.
#'
#' @return A list of class `remode_result` containing:
  #' \describe{
  #'   \item{nr_of_modes}{The number of modes identified in the data.}
  #'   \item{modes}{The indices of the identified modes.}
  #'   \item{xt}{Input data (as frequency table).}
  #'   \item{alpha}{The original significance level.}
  #'   \item{alpha_correction}{The method used for alpha correction.}
  #' }
#'
#' @examples
#' # Input data as frequencies, Bonferroni-like alpha correction
#' data <- c(80, 90, 110, 70, 90)
#' remode(data, alpha_correction = "max_modes")
#'
#' # Raw data input
#' x <- c(rep(1, 80), rep(2, 90), rep(3, 110), rep(4, 70), rep(5, 90))
#' remode(x, alpha_correction = "max_modes", format_raw = TRUE)
#'
#' @export
remode <- function(xt, alpha = 0.05, alpha_correction = c("none", "max_modes"),
                   check = FALSE, f_sign_test = c("fisher", "binomial"),
                   format_raw = FALSE, levels = seq(min(xt), max(xt)), ...) {

  # define which stat. function to use based on user input
  if (is.function(f_sign_test)) { # function defined by user
    test_func <- f_sign_test
  } else { # user chooses predefined function
    test_type <- match.arg(f_sign_test) # fisher is default test
    test_func <- switch(test_type,
                        fisher = perform_fisher_test,
                        binomial = perform_binomial_test)
  }

  # convert raw data to table if required
  if (format_raw) {
    x_factor <- factor(xt, levels = levels)
    xt <- table(x_factor)
  }
  if(length(xt) > 50){
    warning("The length of your data suggests it might be in raw format and not a frequency table.
            If so, please specify by setting format_raw = TRUE.")
  }

  # alpha correction based on user input
  le <- length(xt)
  if (is.function(alpha_correction)) { # function defined by user
    alpha_function <- alpha_correction
    alpha_cor <- alpha_function(alpha = alpha, le = le)
  } else { # user chooses predefined function
    correction_method <- match.arg(alpha_correction) # default: no correction
    alpha_cor <- switch(correction_method ,
                        none = alpha,
                        max_modes = alpha / (floor((le + 1) / 2)))
  }

  # add zeros to sides and correct results
  modes <- remode_find_maxima(c(0, xt, 0), alpha = alpha_cor, check = check, test_func = test_func)
  modes <- modes - 1

  nr_of_modes <- length(modes)

  result <- list(nr_of_modes = nr_of_modes, modes = modes, xt = xt, alpha = alpha,
                 alpha_correction = alpha_correction)

  class(result) <- 'remode_result'

  return(result)
}


#' Barplot for remode_result
#'
#' The `barplot.remode_result` function provides a way to visualize the output of the `remode`
#' function using a bar plot, highlighting the identified modes by adjusting the bar density.
#'
#' @param remode_result A list of class `remode_result` containing the output of the `remode` function.
#' @param main A character string specifying the main title of the plot. Default is "Modes = (number of modes)".
#' @param density A numeric vector specifying the density of shading lines, in lines per inch, for the bars.
#'                Default is 20 for non-mode bars and 50 for mode bars.
#' @param ... Additional arguments passed to the `barplot` function.
#'
#' @return None. This function is called for its side effects.
#'
#' @examples
#' data <- c(80, 90, 110, 70, 90)
#' result <- remode(data, alpha_correction = "max_modes")
#' barplot(result)
#'
#' @export
barplot.remode_result <- function(remode_result, main = paste("Modes =", remode_result$nr_of_modes),
                                  density = replace(rep(20, length(remode_result$xt)), remode_result$modes, 50), ...){
  barplot(remode_result$xt, density = density, main = main, ...)
}

