
library(sprintr)

# Function to evaluate the performance of different methods based on deviance
eval.vec <- function(result, x_test, y_test, method){

  # Loop through each method and calculate deviance

  if("sprinter" %in% method) {
    mu_test <- sprintr::predict.cv.sprinter(result$sprintr, x_test)
    pr_test <- exp(mu_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_test)) + pr_test - y_test)
    eval_vec$dev_sprinter <- append(eval_vec$dev_sprinter, dev)
  }

  if("MEL" %in% method) {
    mu_test <- sprintr::predict.other(result$MEL, x_test)
    pr_test <- exp(mu_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_test)) + pr_test - y_test)
    eval_vec$dev_MEL <- append(eval_vec$dev_MEL, dev)}

  if("APL" %in% method) {
    mu_test <- sprintr::predict.other(result$APL, x_test)
    pr_test <- exp(mu_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_test)) + pr_test - y_test)
    eval_vec$dev_APL <- append(eval_vec$dev_APL, dev)
    }

  if("SIS" %in% method) {
    mu_test <- sprintr::predict.other(result$SIS, x_test)
    pr_test <- exp(mu_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_test)) + pr_test - y_test)
    eval_vec$dev_SIS <- append(eval_vec$dev_SIS, dev)
  }

  return(eval_vec)
}
