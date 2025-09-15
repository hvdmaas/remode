################# HELPER FUNCTIONS FOR REMODE FUNCTION ########################


# STATISTICAL TESTS TO COMPARE CANDIDATE'S FREQUENCY AGAINST LOCAL MINIMA

# bootstrap test
perform_bootstrap_test = function(candidate, left_minimum, right_minimum, x, alpha)
{
  rawdata=rep(1:length(x),x)
  count=0
  n_boot=10000
  for(i in 1:n_boot) {
    x_sample=sample(rawdata,sum(x),replace=T)
    ca.count=sum(x_sample==candidate)
    le.count=sum(x_sample==left_minimum)
    ri.count=sum(x_sample==right_minimum)
    if(ca.count > le.count & ca.count > ri.count) count=count+1
  }
  p=1-count/n_boot
  print('done')
  if(p <alpha) return(c(candidate,p))
}

# binomial test
perform_binomial_test <- function(candidate, left_minimum, right_minimum, x, alpha){
  p_left <- binom.test(c(x[candidate], x[left_minimum]),alternative='greater')$p.value
  p_right <- binom.test(c(x[candidate], x[right_minimum]),alternative='greater')$p.value

  if (p_left < alpha && p_right < alpha) return(c(candidate,max(p_left,p_right)))
}

# fisher test
perform_fisher_test <- function(candidate, left_minimum, right_minimum, x, alpha){
  p_left <- fisher.test(matrix(c(x[candidate], sum(x) - x[candidate], x[left_minimum], sum(x) - x[left_minimum]), ncol = 2),
                        alternative = "greater")$p.value
  p_right <- fisher.test(matrix(c(x[candidate], sum(x) - x[candidate], x[right_minimum], sum(x) - x[right_minimum]), ncol = 2),
                         alternative = "greater")$p.value

  # classify candidate as mode only if both tests were significant;
  # report maximum of the two p-values as a conservative estimate of the joint evidence.
  if(1-(1-p_left)*(1-p_right) < alpha) return(c(candidate,1-(1-p_left)*(1-p_right)))
}


# RECURSIVE FUNCTION USED INSIDE remode() ---------
remode_find_maxima <- function(x, alpha = 0.05, check = FALSE, test_func) {

  # Early return for short vectors
  if (length(x) < 3) {
    if (check) print(paste('x =', paste(x,collapse=","),'stop'))
    return(c(integer(0),integer(0)))
  }
  if (check)
    print(paste('x =', paste(x,collapse=",")))

  candidate <- which.max(x) # position candidate maximum
  left_interval <- x[1:(candidate-1)] # intervals on left & right side of candidate
  right_interval <- x[(candidate+1):length(x)]
  left_minimum <- which.min(left_interval) # position left minimum
  right_minimum <- which.min(right_interval) + candidate # position right minimum

  if (check) {
    print(paste('locations extrema =',
                paste(c(left_minimum, candidate, right_minimum),collapse=",")))
  }

  # perform chosen statistical test on both sides
  result <- if (candidate > 1 && candidate < length(x)){
    test_func(candidate, left_minimum, right_minimum, x, alpha)
  }

  if (check) {
    print(paste("mode detected at:", result[1], "with p-value:", result[2]))
  }

  result_list=list() # store position & p-value of detected mode in list
  if(!is.null(result)) {
    result_list= list(result)
  }

  # recursive calls on left and right sides of candidate
  left <- remode_find_maxima(x[1:(candidate - 1)], alpha, check, test_func)
  right <- remode_find_maxima(x[(candidate + 1):length(x)], alpha, check, test_func)
  right <- lapply(right, function(r) c(r[1] + candidate, r[2])) # corrects indices of right side results

  c(result_list, left, right)
}


# OTHER HELPER FUNCTIONS ------

# Calculate the Bayes factor for a given p-value (Sellke, Bayarri, and Berger (2001))
bayes_factor <- function(p) {
  if(p==0) return(Inf)  else # BF_10: infinite evidence for alternative
    if (p <= 1 / exp(1)) {
      bf01 <- (-exp(1) * p * log(p)) / (1 - p)
      return(1 / bf01)  # BF_10: evidence for alternative
    } else {
      return(NA)  # not valid for large p-values
    }
}

# jacknife function for stability test
jacknife= function(xt,percentage){
  x=rep(1:length(xt),xt)
  delete=sample(1:length(x),percentage*length(x)/100,replace=F)
  if(length(delete)>0) x=x[-delete]
  x <- factor(x,levels = 1:length(xt))
  table(x)
}

