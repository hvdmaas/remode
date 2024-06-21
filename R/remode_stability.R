# Code for top-level remode_stability function, performing stability check on
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
#' Assess stability of Mode Estimation Using Jackknife Resampling
#'
#' The `remode_stability` function evaluates the stability of the mode estimation (output of remode() function)
#' by performing jackknife resampling. It generates a series of jackknife samples,
#' calculates the mean and most frequent number of modes for each sample, and determines
#' the stability of the original mode estimate.
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
#' The stability of the mode estimation is determined by the maximum percentage of data removal for which the majority
#' of iterations still find the original number of modes. Using the same logic, this method furthermore checks the stability of location at which a mode is detected:
#' It estimates the percentage of data that can be removed to still find a mode at this location in the majority of iterations.
#'
#' @return A list containing:
#' \describe{
#'   \item{num_mode_stability}{A data frame with the results of the jackknife resampling, including mean modality, most frequent modality, and majority result for each percentage of data removed.}
#'   \item{stable_until}{The maximum percentage of data that can be removed while still retaining the original number of modes in the majority of iterations.}
#'   \item{location_stability}{A data frame storing the location of each detected mode as well as a stability estimate for this mode, calculated as the percentage of data that can be removed to still detect the mode at this location in the majority of iterations.}
#' }
#'
#' @examples
#' data <- c(80, 90, 110, 70, 90)
#' result <- remode(data, alpha_correction = "max_modes")
#' stability <- remode_stability(result, iterations = 100, percentage_steps = 20, plot = TRUE)
#' print(stability)
#'
#' @export
remode_stability <- function(remode_result, iterations = 100, percentage_steps = 10, plot = T){

  # Initialize the percentage steps and create a data frame to store results
  modes <- data.frame(perc = seq(0, 100, by = (100/percentage_steps)),
                      mean_modality = NA,
                      most_freq_modality=NA,
                      majority_result = NA)
  # Set initial values for 0% removed data
  modes$mean_modality[1] <- remode_result$nr_of_modes
  modes$most_freq_modality[1] <- remode_result$nr_of_modes
  modes$majority_result[1] <- 1

  # Initialize matrix to store data counts of mode locations
  modes_locations=matrix(0,percentage_steps+1,length(remode_result$xt))
  modes_locations[1,remode_result$modes]=iterations

  # Loop through each percentage step
  m=integer(iterations)
  for(i in 2:(percentage_steps)){
    # Perform iterations of jackknife and remode
    for(j in 1:iterations){
      xt_jacknifed <- jacknife(remode_result$xt, percentage = modes$perc[i])
      r <- remode(xt_jacknifed, alpha = remode_result$alpha,
                  alpha_correction = remode_result$alpha_correction)
      m[j] <- r$nr_of_modes[1]  # Store number of modes found in each iteration
      modes_locations[i,r$modes]= modes_locations[i,r$modes] + 1 # Update mode location matrix
    }

    # Calculate mean and most frequent modality count for the current percentage step
    modes$mean_modality[i] <- mean(m)
    modes$most_freq_modality[i] <- as.numeric(names(which.max(table(m))))
    # Determine if the majority of iterations find the initial result
    modes$majority_result[i] <- (sum(m == remode_result$nr_of_modes)/iterations) >= 0.5
  }

  # Handle case where 100% of data is removed
  modes$mean_modality[nrow(modes)] <- 0  # if 100% deleted
  modes$most_freq_modality[nrow(modes)] <- 0
  modes$majority_result[nrow(modes)] <- 0
  modes_locations[nrow(modes),] <- 0

  # Determine stability until which percentage of data can be removed while still finding initial result
  #stable_until <- max(modes$perc[modes$majority_result == 1])
  stable_until <- max(modes$perc[(modes$majority_result==1) & (modes$most_freq_modality==remode_result$nr_of_modes)])

    # plot if TRUE
  if(plot){
    plot(modes$perc, modes$mean_modality, type="S", col = "red", frame = F,
         main=paste0("Modes : ", remode_result$nr_of_modes,", stability: ", stable_until ," % of data can be removed"),
         xlab="Percentage of removed data",
         ylab="Modality")
    lines(modes$perc, modes$most_freq_modality, type="S", col="blue", lty = 2)
    legend("bottomleft", legend=c("Mean Modality", "Most freq. modality"),
           col=c("red", "blue"), lty=1:2, cex=0.8,bty='n')
  }

  # Calculate the stability of the location of detected modes
  stability_location=apply(modes_locations,2,function(x) (which.min(x>(iterations/2))-1)/nrow(modes_locations))
  stability_location=stability_location[stability_location>0]
  stability_location=cbind(sort(remode_result$modes),stability_location)
  colnames(stability_location) <- c("Mode location", "Stability estimate")

  return(list(
    "num_mode_stability" = modes, # Df containing stability of number of modes
    "stable_until" = stable_until, #  # Percentage of data until which initial result is stable
    "location_stability" = stability_location)) # Location of stability based on majority iterations
}
