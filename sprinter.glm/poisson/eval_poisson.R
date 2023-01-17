eval.vec <- function(result, x_test, y_test, mu_test, pr_test, method, ix_int){
  
  if("sprinter" %in% method) {
    mu_te <- predict.cv.sprinter(result$sprintr, x_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_te)) + pr_te - y_test)
    eval_vec$dev_sprinter <- append(eval_vec$dev_sprinter, dev)
    cat("sprinter", fill = TRUE)}
  
  if("APL" %in% method) {
    mu_te <- predict.other(result$APL, x_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_te)) + pr_te - y_test)
    eval_vec$dev_APL <- append(eval_vec$dev_APL, dev)
    cat("APL", fill = TRUE)}
  
  if("MEL" %in% method) {
    mu_te <- predict.other(result$MEL, x_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_te)) + pr_te - y_test)
    eval_vec$dev_MEL <- append(eval_vec$dev_MEL, dev)
    cat("MEL", fill = TRUE)}
  
  if("SIS" %in% method) {
    mu_te <- predict.other(result$SIS, x_test)
    dev <- 2 * sum(ifelse(y_test == 0, 0, y_test*log(y_test/pr_te)) + pr_te - y_test)
    eval_vec$dev_SIS <- append(eval_vec$dev_SIS, dev)
    cat("SIS", fill = TRUE)}
  
  return(eval_vec)
}