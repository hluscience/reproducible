# evaluate deviance and AUC
eval.vec <- function(result, x_test, y_test, method, ix_int){
  
  if("sprinter" %in% method) {
    mu_test <- predict.cv.sprinter(result$sprintr, x_test)
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec$dev_sprinter <- append(eval_vec$dev_sprinter, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc")
    eval_vec$auc_sprinter <- append(eval_vec$auc_sprinter, as.numeric(auc.tmp@y.values))}
  
  if("APL" %in% method) {
    mu_test <- predict.other(result$APL, x_test)
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec$dev_APL <- append(eval_vec$dev_APL, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc")
    eval_vec$auc_APL <- append(eval_vec$auc_APL, as.numeric(auc.tmp@y.values))}
  
  if("MEL" %in% method) {
    mu_test <- predict.other(result$MEL, x_test)
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec$dev_MEL <- append(eval_vec$dev_MEL, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc")
    eval_vec$auc_MEL <- append(eval_vec$auc_MEL, as.numeric(auc.tmp@y.values))}
  
  if("RAMP" %in% method) {
    mu_test <- predict.other(result$RAMP, x_test)
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec$dev_RAMP <- append(eval_vec$dev_RAMP, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc")
    eval_vec$auc_RAMP <- append(eval_vec$auc_RAMP, as.numeric(auc.tmp@y.values))}
  
  if("glinternet" %in% method) {
    mu_test <- predict.other(result$glinternet, x_test)
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec$dev_glinternet <- append(eval_vec$dev_glinternet, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc")
    eval_vec$auc_glinternet <- append(eval_vec$auc_glinternet, as.numeric(auc.tmp@y.values))}
  
  if("hierNet" %in% method) {
    mu_test <- predict.other(result$hierNet, x_test)
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec$dev_hierNet<- append(eval_vec$dev_hierNet, dev)
    pp_test <- 1/(1+exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred,"auc")
    eval_vec$auc_hierNet <- append(eval_vec$auc_hierNet, as.numeric(auc.tmp@y.values))
  }
  
  return(eval_vec)
}

compute_deviance_logistic <- function(y, mu){
  pp <- 1 / (1 + exp(-mu))
  pp[pp == 0] <- .Machine$double.eps * 5
  pp[pp == 1] <- 1 - .Machine$double.eps * 5
  dev <- -2 * as.numeric(sum(((y == 1) * log(pp) + (y == 0) * log(1 - pp))))
  return(dev)
}