# Code for top-level remode_robustness function, performing robustness check on
# result of remode(), as well as lower-level helper function

# Helper function: jacknife
jacknife= function(xt,percentage){
  x=rep(1:length(xt),xt)
  delete=sample(1:length(x),percentage*length(x)/100,replace=F)
  if(length(delete)>0) x=x[-delete]
  x <- factor(x,levels = 1:length(xt))
  table(x)
}

# Main function ----
#' Assess Robustness of Mode Estimation Using Jackknife Resampling
#'
#' The `remode_robustness` function evaluates the robustness of the mode estimation (output of remode() function)
#' by performing jackknife resampling. It generates a series of jackknife samples,
#' calculates the mean and most frequent number of modes for each sample, and determines
#' the robustness of the original mode estimate.
#'
#' @param remode_result A list of class `remode_result` containing the output of the `remode` function.
#' @param iterations An integer specifying the number of jackknife samples to generate for each percentage step. Default is 100.
#' @param percentage_steps An integer specifying the number of percentage steps for data removal in the jackknife resampling. Default is 20 (i.e., steps of 5%).
#' @param plot A logical value indicating whether to plot the results. Default is TRUE.
#'
#' @details
#' The function generates jackknife samples by systematically removing increasing percentages of data. For each percentage step,
#' it calculates the mean and most frequent number of modes based on the `remode` function applied to the jackknife samples.
#' It then checks if the majority of the iterations yield the same number of modes as the original result.
#' The robustness of the mode estimation is determined by the maximum percentage of data removal for which the majority
#' of iterations still find the original number of modes.
#'
#' @return A list containing:
#' \describe{
#'   \item{jacknife_df}{A data frame with the results of the jackknife resampling, including mean modality, most frequent modality, and majority result for each percentage of data removed.}
#'   \item{robust_until}{The maximum percentage of data that can be removed while still retaining the original number of modes in the majority of iterations.}
#' }
#'
#' @examples
#' data <- c(80, 90, 110, 70, 90)
#' result <- remode(data, alpha_correction = "max_modes")
#' robustness <- remode_robustness(result, iterations = 100, percentage_steps = 20, plot = TRUE)
#' print(robustness)
#'
#' @export
remode_robustness <- function(remode_result, iterations = 100, percentage_steps = 20, plot = T){

  modes <- data.frame(perc = seq(0, 100, by = (100/percentage_steps)), mean_modality = NA, most_freq_modality=NA, majority_result = NA)
  modes$mean_modality[1] <- remode_result$nr_of_modes # 0% means no jacknife (i.e., result)
  modes$most_freq_modality[1] <- remode_result$nr_of_modes
  modes$majority_result[1] <- 1
  m=integer(iterations)
  # for each jacknife proportion
  for(i in 2:(percentage_steps)){
    for(j in 1:iterations){
      xt_jacknifed <- jacknife(remode_result$xt, percentage = modes$perc[i])
      m[j] <- c(remode(xt_jacknifed, alpha = remode_result$alpha,
                       alpha_correction = remode_result$alpha_correction)$nr_of_modes[1])
    }
    # get mean and mode of estimated modalities, check if majority of runs find initial result
    modes$mean_modality[i] <- mean(m)
    modes$most_freq_modality[i] <- as.numeric(names(which.max(table(m))))
    modes$majority_result[i] <- (sum(m == remode_result$nr_of_modes)/iterations) >= 0.5
  }

  modes$mean_modality[nrow(modes)] <- 0  # if 100% deleted
  modes$most_freq_modality[nrow(modes)] <- 0
  modes$majority_result[nrow(modes)] <- 0

  # how robust is the mode estimation?
  robust_until <- max(modes$perc[modes$majority_result == 1])

  # plot
  if(plot){
    plot(modes$perc, modes$mean_modality, type="S", col = "red", frame = F,
         main=paste0("Modes : ", remode_result$nr_of_modes,", Robustness: ", robust_until ," % of data can be removed"),
         xlab="Percentage of removed data",
         ylab="Modality")
    lines(modes$perc, modes$most_freq_modality, type="S", col="blue", lty = 2)
    legend("bottomleft", legend=c("Mean Modality", "Most freq. modality"),
           col=c("red", "blue"), lty=1:2, cex=0.8,bty='n')
  }

  return(list(
    "jacknife_df" = modes,
    "robust_until" = robust_until)) # based on majority of iterations finding initial modality
}
