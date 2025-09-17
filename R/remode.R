# remode.R
source("R/helper_functions.R")

#' @importFrom stats chisq.test fisher.test binom.test
#' @importFrom graphics legend lines barplot text
#' @importFrom utils head
NULL

# Main function ----------
#' @title Recursive Mode Detection for Distributions of Ordinal Data
#'
#' @name remode
#'
#' @description Implementation of the recursive modality detection method (ReMoDe) which detects modes in univariate distributions through recursive significance testing. ReMoDe recursively tests whether the frequency of a local maximum significantly exceeds the frequencies of neighboring local minima on its left and right side.
#'
#' @param xt A numeric vector of ordinal data.
#' @param alpha The significance level for the chi-squared test. Default is 0.05.
#' @param f_sign_test A character string or function specifying the statistical test to use for significance testing. Options are "bootstrap" (default), "binomial" (more efficient when N is large), "fisher" (exact fisher test) or a user-defined function. User-defined functions must include the following arguments: candidate, left_minimum, right_minimum, xt, alpha
#' @param alpha_correction A character string or function specifying the method for correcting the alpha level. Options are "max_modes" (Bonferroni-like correction), "none" (no correction), or a user-defined function. User-defined function must entail alpha and k (number of categories) as arguments.
#' @param definition Underlying modality definition. If "shape_based", the unifom distribution is classified as unimodal. If "peak_based", a uniform distribution is classified as having zero modes.
#' @param check A logical variable indicating whether to return input, test, and outcome of each recursive step of the algorithm. Default is FALSE.
#' @param format_raw A logical value indicating whether the input data (`xt`) is raw data. If TRUE, data will be converted to a frequency table inside the function. Default is FALSE.
#' @param levels A numeric vector specifying the categories of the (ordinal) distribution. Used for the factor conversion if `format_raw` is TRUE. Default is `seq(min(xt), max(xt))`.
#' @param n_boot Number of bootstrap samples. Only used if f_dign_test = "bootstrap".
#' @param ... Additional arguments
#'
#' @details The function recursively detects a mode candidate (highest frequency), tests whether its frequency significantly deviates from the lowest frequencies on both its left and right side. If significant, the candidate is classified as a mode. The function recursively processes the segments of the vector to the left and right of the mode candidate, applying the same procedure to identify additional modes.
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
#' # Input data as frequencies
#' data <- c(80, 90, 110, 70, 90)
#' result <- remode(data)
#' summary(result)
#'
#' # Raw data input
#' x <- c(rep(1, 80), rep(2, 90), rep(3, 110), rep(4, 70), rep(5, 90))
#' result <- remode(x, format_raw = TRUE)
#' summary(result)
#'
#' @export
remode <- function(xt,
                   alpha = 0.05,
                   f_sign_test = c("bootstrap", "binomial", "fisher"),
                   alpha_correction = c("max_modes", "none"),
                   definition = c("shape_based", "peak_based"),
                   check = FALSE,
                   format_raw = FALSE,
                   levels = seq(min(xt), max(xt)),
                   n_boot=10000,
                   ...){

  # DEFINITIONS -----------------

  # frequency count
  if (format_raw) { # convert raw data if required
    x_factor <- factor(xt, levels = levels)
    xt <- table(x_factor)
  }
  if(length(xt) > 50){
    warning("The length of your data suggests it might be in raw format and not a frequency table.
            If so, please specify by setting format_raw = TRUE.")
  }

  k <- length(xt) # number of categories
  N <- sum(xt) # sample size

  # ARGUMENT VALIDATION & SET UP --------------

  # set statistical function based on user input
  test_args <- list() # function specific arguments

  if (is.function(f_sign_test)) { # custom user-defined function
    test_func <- f_sign_test

  } else { # user chooses predefined function
    chosen_test <- match.arg(f_sign_test) # bootstrap is default test

    if (chosen_test == "bootstrap") { # if bootstrap, specify n_boot argument
      test_func <- perform_bootstrap_test
      test_args$n_boot <- n_boot
    } else {
      # Error handling: if n_boot is provided for a non-bootstrap test
      if (!missing(n_boot) && n_boot != 10000) { # Check if n_boot explicitly changed by  user
        stop("The 'n_boot' argument is only applicable when f_sign_test is set to 'bootstrap'.")
      }
      test_func <- switch(chosen_test,
                          binomial = perform_binomial_test,
                          fisher = perform_fisher_test)
    }




    test_func <- switch(chosen_test,
                        bootstrap = perform_bootstrap_test,
                        binomial = perform_binomial_test,
                        fisher = perform_fisher_test)
  }

  # correct alpha level based on user input
  if (is.function(alpha_correction)) { # custom user-defined function
    alpha_function <- alpha_correction
    corrected_alpha <- alpha_function(alpha = alpha, k = k)

  } else { # user chooses predefined function
    chosen_alpha_correction <- match.arg(alpha_correction) # default: max_modes
    corrected_alpha <- switch(chosen_alpha_correction,
                              max_modes = alpha / (floor((k + 1) / 2)),
                              none = alpha
                              )
  }

  # set modality definition based on user input
  chosen_definition <- match.arg(definition) # default: shape_based

  # PERFORM REMODE ALGORITHM ----------

  # execute recursive function
  modes <- remode_find_maxima(
    c(0, xt, 0), # apply zero-padding
    alpha = corrected_alpha,
    check = check,
    test_func = test_func,
    test_args = test_args # contains n_boot for if test_func is bootstrap
  )
  modes <- unlist(modes) # unpack list of detected modes and p-values

  # get p-values and Bayes Factor approximations for each detected mode
  if(length(modes)>0){ # if modes are found
    # get p-values and Bayes factors
    p_values <- modes[seq(2, length(modes), by = 2)]
    b_factors <- sapply(p_values, bayes_factor)

    modes <- modes[seq(1, length(modes), by = 2)] # extract indices of modes
    modes <- modes - 1 # correct indices after zero-padding of left side

  } else { # if no modes found, return NULL
    p_values <- NULL
    b_factors <- NULL
  }

  # prepare function output
  results <- list(
    nr_of_modes = length(modes),
    mode_indeces = modes,
    p_values = p_values,
    approx_bayes_factors= b_factors,
    frequency_input_data = xt,
    alpha = alpha,
    alpha_corrected = corrected_alpha,
    alpha_correction = if (is.function(alpha_correction)) "custom" else chosen_alpha_correction,
    definition = chosen_definition
  )

  # FOR PEAK-BASED MODALITY DEFINITION: TEST FOR UNIFORMITY
  if(chosen_definition=="peak_based"){
    chisq_result = chisq.test(xt) # pearson's Chi square test (goodness of fit)

    if(chisq_result$p.value > 0.05){ # if uniform: modify function output
      results <- list(
        nr_of_modes = 0,
        mode_indeces = "Input distribution is uniform (Pearson's Chi^2 test; p > 0.05). Following a peak-based definition of modality, no modes are detected.",
        p_values = c("p-value of Chi^2 test: ",chisq_result$p.value),
        approx_bayes_factors= NULL,
        frequency_input_data = xt,
        alpha = NULL,
        alpha_corrected = NULL,
        alpha_correction = NULL,
        definition = chosen_definition
      )

    }
  }

  # return result output
  class(results) <- 'remode_result'
  return(results)
}



#' @title Plot for remode_result
#'
#' @description The `plot.remode_result` function provides a way to visualize the output of the `remode` function using a bar plot, highlighting the identified modes by adjusting the bar density.
#'
#' @param x A list of class `remode_result` containing the output of the `remode` function.
#' @param main A character string specifying the main title of the plot. Default is "Modes = (number of modes)".
#' @param density A numeric vector specifying the density of shading lines, in lines per inch, for the bars. Default is 20 for non-mode bars and 50 for mode bars.
#' @param ... Additional arguments passed to the `barplot` function.
#'
#' @return None. This function is called for its side effects.
#'
#' @examples
#' data <- c(80, 90, 110, 70, 90)
#' result <- remode(data)
#' plot(result, xlab="This is my x-axis label", col="red")
#'
#' @export
plot.remode_result <- function(
    x,
    main = paste("Number of modes =", x$nr_of_modes),
    density = replace(
      rep(20, length(x$frequency_input_data)),
      x$mode_indeces,
      50), ...
){

  # Add headroom for p-values *
  y_max <- max(x$frequency_input_data) * 1.1 # 10% padding

  # Create barplot and capture midpoints
  mids <- barplot(x$frequency_input_data,
                  density = density,
                  main = main,
                  ylim = c(0, y_max),
                  ...)

  # Add stars for p-values on top of mode bars
  if (!is.null(x$p_values)) {
    for (i in seq_along(x$mode_indeces)) { # for each mode

      mode_index <- x$mode_indeces[i]
      pval <- x$p_values[i]

      # assign star notation to p-value
      if(pval<.001) star='***'
      else if(pval<.01) star='**'
      else if(pval<.05) star='*'
      else star=''

      # add significance star labels as text
      text(x = mids[mode_index],
           y = x$frequency_input_data[mode_index] + y_max * 0.02, # small offset
           labels = star,
           cex = 0.8)
    }
  }
}


#' @title Printing remode_result
#'
#' @description Prints a `remode_result` object in easily readable format.
#'
#' @param x A list of class `remode_result` returned by the `remode` function.
#' @param ... Additional arguments.
#'
#' @export
print.remode_result <- function(x, ...) {
  cat("Recursive modality detection (ReMoDe):\n\n")

  cat("For input data", x$input_data)
  cat("Number of modes detected:", x$nr_of_modes, "\n\n")

  if (x$nr_of_modes > 0) {
    for (i in 1:x$nr_of_modes) {
      cat("  Mode", i, ": at index", x$mode_indeces[[i]],
          "with p-value =", x$p_values[[i]],
          "and approx BF =", x$approx_bayes_factors[[i]], "\n")
    }

    cat("\nParameters used: alpha = ", x$alpha, ", ",
        x$alpha_correction, " correction, ", x$definition,
        " definition\n", sep = "")

  } else {
    print("Input distribution is uniform (Pearson's Chi^2 test; p > 0.05). Following a peak-based definition of modality, no modes are detected.")

  }
}


