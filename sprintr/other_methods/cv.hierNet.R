# cv.glinternet Function Overview
#
# Build a regression model with hierarchically constrained pairwise interactions from hierNet package.
#
# Inputs:
# - x: An n by p design matrix of main effects. Each row is an observation of p main effects.
# - y: A response vector of size n.
# - family: family response type. Default is ’gaussian’. The other choice is ’binomial’ for logistic regression.
# - nlam: Number of values of lambda to be tried.
# - nfolds: Number of folds in cross-validation. Default value is 10.
# - num_keep: Number of variables to keep in the screening phase
# - strong: Indicator to specify strong hierarchy (TRUE) or weak hierarchy (FALSE). Default FALSE.
# - ... : Other arguments passed to hierNet calls.

# Output:
# Returns an object of S3 class 'cv.hierNet', which includes:
# - n: Sample size.
# - p: Number of main effects.
# - family: The response family used in the model.
# - strong: The hierarchy parameter used.
# - a0: The intercept value.
# - compact: A compact representation of selected variables, including indices of the variables and 
#   their estimated coefficients.

# Install and load necessary packages
if (!require(hierNet)) install.packages("hierNet")
if (!require(Matrix)) install.packages("Matrix")
library(hierNet)
library(Matrix)

cv.hierNet <- function(x, y, family = "gaussian", nlam = 20, nfolds = 10, strong = TRUE, ...){
  
  n <- nrow(x)
  p <- ncol(x)
  
  # Fit a logistic path of hierNet models over different values of the regularization parameter. 
  if(family == "gaussian"){
    fit <- hierNet::hierNet.path(x, y, nlam = nlam, strong = strong, ...)
  }else if(family == "binomial"){
    fit <- hierNet::hierNet.logistic.path(x, y, nlam = nlam, strong = strong, ...)
  }
  
  # Use cross-validation to estimate the regularization parameter for hierNet
  fitcv <- hierNet::hierNet.cv(fit, x, y, nfolds = nfolds, ...)
  
  # The index of best lambda
  lambda_best <- which.min(fitcv$cv.err)
  
  # Grab coefficient estimate
  # Overall main effect estimated coefficients are bp-bn
  # bp: "positive part" main effect
  # pn: "negative part" main effect;
  main <- fit$bp[, lambda_best] - fit$bn[, lambda_best]
  main.ix <- seq(p)[main!=0]
  main.coef <- main[main!=0]
  
  # th: Matrix of estimated interaction coefficients, of dimension p-by-p. 
  # Note: when output from hierNet is printed, th is symmetrized (set to (th+t(th))/2) for simplicity.
  inter.mt <- (t(fit$th[, , lambda_best])+fit$th[, , lambda_best])/2
  inter.vc <- inter.mt[lower.tri(inter.mt, diag=T)] 
  inter.ix <- Matrix::which(Matrix::tril(inter.mt!=0), arr.ind = TRUE)[,c("col","row")]
  inter.coef <- inter.vc[inter.vc!=0]
  
  # We now combine estimated main effects and interactions coefficients
  coef.cb <- c(main.coef, inter.coef)
  ix.cb <- rbind(cbind(rep(0,sum(main!=0)), main.ix), inter.ix)
  compact <- cbind(ix.cb, coef.cb)
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  rownames(compact) <- NULL
  a0 <- as.numeric(fit$b0[lambda_best])
  
  out <- list(n = n,
              p = p,
              family = family, 
              strong = strong,
              a0 = a0,
              compact = compact,
              call = match.call())
  class(out) <- "other"
  return(out)
}