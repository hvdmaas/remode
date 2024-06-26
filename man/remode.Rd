% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remode.R
\name{remode}
\alias{remode}
\title{Performs a recursive mode detection function for ordinal data}
\usage{
remode(
  xt,
  alpha = 0.05,
  alpha_correction = c("none", "max_modes"),
  check = FALSE,
  f_sign_test = c("fisher", "binomial"),
  format_raw = FALSE,
  levels = seq(min(xt), max(xt)),
  ...
)
}
\arguments{
\item{xt}{A (non-empty) vector of input data. Either in frequencies (default),
or as raw data. For raw data, set format_raw argument to TRUE.}

\item{alpha}{A value specifying the significance level for the statistical tests. Default is 0.05.}

\item{alpha_correction}{A character string or function specifying the method for correcting the alpha level.
Options are "none" (no correction;default),
"max_modes" (Bonferroni-like correction), or a user-defined function.}

\item{check}{A logical variable indicating whether to return input, test, and
outcome of each recursive step of the algorithm. Default is FALSE.}

\item{f_sign_test}{A character string or function specifying the statistical test to use for
significance testing. Options are "fisher" (default), "binomial", or a user-defined function.}

\item{format_raw}{A logical value indicating whether the input data (`xt`) is raw data.
If TRUE, data will be converted to a frequency table inside the function. Default is FALSE.}

\item{levels}{A numeric vector specifying the categories of the (ordinal) distribution.
Used for the factor conversion if `format_raw` is TRUE. Default is `seq(min(xt), max(xt))`.}
}
\value{
A list of class `remode_result` containing:
\describe{
  \item{nr_of_modes}{The number of modes identified in the data.}
  \item{modes}{The indices of the identified modes.}
  \item{xt}{Input data (as frequency table).}
  \item{alpha}{The original significance level.}
  \item{alpha_correction}{The method used for alpha correction.}
}
}
\description{
Performs a recursive mode detection function for ordinal data
}
\details{
The function recursively detects a mode candidate (highest frequency), tests whether
its frequency significantly deviates from the lowest frequencies on both its left and right side.
If significant, the candidate is classified as a mode. The function recursively processes the segments
of the vector to the left and right of the mode candidate, applying the same procedure to
identify additional modes.
}
\examples{
# Input data as frequencies, Bonferroni-like alpha correction
data <- c(80, 90, 110, 70, 90)
remode(data, alpha_correction = "max_modes")

# Raw data input
x <- c(rep(1, 80), rep(2, 90), rep(3, 110), rep(4, 70), rep(5, 90))
remode(x, alpha_correction = "max_modes", format_raw = TRUE)

}
