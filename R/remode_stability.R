# Code for top-level remode_stability function, performing stability check on
# result of remode(), as well as lower-level helper function


#' @title Barplot for 'remode' result
#' @importFrom graphics barplot text
#'
#' @description remode_stability evaluates the stability of the mode estimation of the remode() function
#' by performing jacknife resampling. It generates
#' a series of jackknife resamples, calculates the mean and most frequent number of modes for
#' each sample, and determines the stability of the original modality estimate.
#' Stability is determined by te maximum percentage of data removal for which the
#' majority of iterations still find the original number of modes.
#'
#' @param remode_result A list of class `remode_result`, output of remode() function
#' @param iterations Numeric value specifying the number of random sub-samples to generate for each percentage step. Default is 100.
#' @param percentage_steps An integer specifying the number of percentage steps for data removal. Default is 20 (i.e. steps of 5).
#' @param plot Logical value indicating wether to plot the results. Default is TRUE.
#'
#' @return A list containing the stability analysis results.
#'
#' @export
remode_stability <- function(remode_result, iterations = 100, percentage_steps = 10, plot = TRUE){

  if(length(remode_result$mode_indeces) == 0) stop("It appears that either the 'remode' algorithm was not yet applied or that no modes were found. Stability analyses can only be conducted for modes detected by remode().")

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
  modes_locations=matrix(0,percentage_steps+1,length(remode_result$frequency_input_data))
  modes_locations[1,remode_result$modes]=iterations

  # Loop through each percentage step
  m=integer(iterations)
  for(i in 2:(percentage_steps)){

    # Perform iterations of jackknife and 'remode'
    for(j in 1:iterations){
      xt_jacknifed <- jacknife(remode_result$frequency_input_data, percentage = modes$perc[i])
      r <- remode(xt_jacknifed, alpha = remode_result$alpha,
                  f_sign_test = remode_result$sign_test)
      m[j] <- r$nr_of_modes[1] # Store number of modes found in each iteration
      modes_locations[i,r$modes]= modes_locations[i,r$modes] + 1 # Update mode location matrix
    }

    # Calculate mean and most frequent modality count for the current percentage step
    modes$mean_modality[i] <- mean(m)
    modes$most_freq_modality[i] <- as.numeric(names(which.max(table(m))))
    # Determine if the majority of iterations (> 50%) find the initial result
    modes$majority_result[i] <- (sum(m == remode_result$nr_of_modes)/iterations) >= 0.5
  }

  # Handle case where 100% of data is removed
  modes$mean_modality[nrow(modes)] <- 0 # if 100% deleted
  modes$most_freq_modality[nrow(modes)] <- 0
  modes$majority_result[nrow(modes)] <- 0
  modes_locations[nrow(modes),] <- 0

  # Determine max percentage of data that can be removed while still finding initial result in min 50% of iterations
  first_false <- which(!modes$majority_result)[1] # Find the first percentage (starting from 0%) where majority result is FALSE
  if (is.na(first_false)) {
    # if no FALSE found, stable across all perc values
    stable_until <- max(modes$perc)
  } else {
    # otherwise take  value just before the first FALSE
    stable_until <- modes$perc[first_false - 1]
  }

  # optional: plot
  if(plot){
    plot(modes$perc, modes$mean_modality, type="S", col = "red", frame = FALSE,
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
    "num_mode_stability" = modes, # df containing stability of number of modes
    "stable_until" = stable_until, # Percentage of data until which initial result is stable
    "location_stability" = stability_location)) # Location of stability based on majority iterations
}
