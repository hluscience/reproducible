
library(sprintr)
library(ROCR)

# Function to evaluate the performance of different methods based on deviance and AUC.
eval.vec <- function(result, x_test, y_test, method, ix_int){
  
  # Loop through each method and calculate predictions, deviance, and AUC.
  if("sprinter" %in% method) {
    mu_test <- sprintr::predict.cv.sprinter(result$sprintr, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_sprinter <- append(eval_vec$dev_sprinter, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc") # auc
    eval_vec$auc_sprinter <- append(eval_vec$auc_sprinter, as.numeric(auc.tmp@y.values))}
  
  if("APL" %in% method) {
    mu_test <- sprintr::predict.other(result$APL, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_APL <- append(eval_vec$dev_APL, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc") # auc
    eval_vec$auc_APL <- append(eval_vec$auc_APL, as.numeric(auc.tmp@y.values))}
  
  if("MEL" %in% method) {
    mu_test <- sprintr::predict.other(result$MEL, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_MEL <- append(eval_vec$dev_MEL, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc") # auc
    eval_vec$auc_MEL <- append(eval_vec$auc_MEL, as.numeric(auc.tmp@y.values))}
  
  if("RAMP" %in% method) {
    mu_test <- sprintr::predict.other(result$RAMP, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_RAMP <- append(eval_vec$dev_RAMP, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc") # auc
    eval_vec$auc_RAMP <- append(eval_vec$auc_RAMP, as.numeric(auc.tmp@y.values))}
  
  if("glinternet" %in% method) {
    mu_test <- sprintr::predict.other(result$glinternet, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_glinternet <- append(eval_vec$dev_glinternet, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc") # auc
    eval_vec$auc_glinternet <- append(eval_vec$auc_glinternet, as.numeric(auc.tmp@y.values))}
  
  if("hierNet" %in% method) {
    mu_test <- sprintr::predict.other(result$hierNet, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_hierNet<- append(eval_vec$dev_hierNet, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc") # auc
    eval_vec$auc_hierNet <- append(eval_vec$auc_hierNet, as.numeric(auc.tmp@y.values))
  }
  
  if("SIS" %in% method) {
    mu_test <- sprintr::predict.other(result$SIS, x_test) # predictions
    dev <- compute_deviance_logistic(y_test, mu_test) # deviance
    eval_vec$dev_SIS<- append(eval_vec$dev_SIS, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc") # auc
    eval_vec$auc_SIS <- append(eval_vec$auc_SIS, as.numeric(auc.tmp@y.values))
  }
  
  return(eval_vec)
}

# Function to compute the deviance for logistic regression models.
compute_deviance_logistic <- function(y, mu){
  
  # Convert log odds to probabilities
  pp <- 1 / (1 + exp(-mu))
  # Adjust probabilities to avoid log(0) issues
  pp[pp == 0] <- .Machine$double.eps * 5
  pp[pp == 1] <- 1 - .Machine$double.eps * 5
  # Calculate deviance as twice the negative log-likelihood
  dev <- -2 * as.numeric(sum(((y == 1) * log(pp) + (y == 0) * log(1 - pp))))
  
  return(dev)
}