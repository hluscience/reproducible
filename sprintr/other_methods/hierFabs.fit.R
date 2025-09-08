# hierFabs.fit Function Overview
#
# A hierarchical Forward and Backward Stagewise algorithm for identifying hierachical interaction from HierFabs package
# Inputs:
# - x: An n by p design matrix of main effects. Each row is an observation of p main effects.
# - y: A response vector of size n.
# - family: family response type. Default is ’gaussian’. The other choice is ’binomial’ for logistic regression.
# - hier: Whether to enforce strong or weak hierarchy. Default is strong.
# - ... : Other arguments passed to HierFabs calls.

# Output:
# Returns an object of S3 class 'hierFabs.fit', which includes:
# - n: Sample size.
# - p: Number of main effects.
# - family: The response family used in the model.
# - hier: The hierarchy parameter used.
# - a0: The intercept value.
# - compact: A compact representation of selected variables, including indices of the variables and
#   their estimated coefficients.

# Install and load necessary packages
# if (!require(devtools)) install.packages("devtools")
# library(devtools)
# install_github("XiaoZhangryy/HierFabs", force = TRUE)
library(HierFabs)

hierFabs.fit <- function(x, y, family = "gaussian", hier = "strong", ...){

  n <- nrow(x)
  p <- ncol(x)

  # Fit a logistic path of hierNet models over different values of the regularization parameter.
  if(family == "gaussian"){
    fit <- HierFabs::HierFabs(x, y, model = "gaussian", hier = hier, ...)
  }else if(family == "binomial"){
    fit <- HierFabs::HierFabs(x, y, model = "logistic", hier = hier, ...)
  }

  # The index of best lambda
  lambda_best <- fit$opt

  # Grab coefficient estimate
  a0 <- fit$intercept[lambda_best]
  coef <- fit$beta

  # Main effects
  main <- coef[seq(p)]
  main.ix <- seq(p)[main!=0]
  main.coef <- main[main!=0]

  # Interactions
  idx <- t(combn(p, 2))
  inter <- coef[-seq(p)]
  inter.ix <- idx[inter!=0, ]
  inter.coef <- inter[inter!=0]

  # We now combine estimated main effects and interactions coefficients
  coef.cb <- c(main.coef, inter.coef)
  ix.cb <- rbind(cbind(rep(0,length(main.ix)), main.ix), inter.ix)
  compact <- cbind(ix.cb, coef.cb)
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  rownames(compact) <- NULL

  out <- list(n = n,
              p = p,
              family = family,
              hier = hier,
              a0 = a0,
              compact = compact,
              call = match.call())
  class(out) <- "other"
  return(out)
}
