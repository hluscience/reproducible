# ramp.fit Function Overview
#
# Build a quadratic regression model with regularization algorithm under marginality principle from RAMP package.
#
# Inputs:
# - x: An n by p design matrix of main effects. Each row is an observation of p main effects.
# - y: A response vector of size n.
# - family: family response type. Default is ’gaussian’. The other choice is ’binomial’ for logistic regression.
# - hier: whether to specify strong or weak heredity. Default is ’Strong’.
# - ... : Other arguments passed to RAMP calls.

# Output:
# Returns an object of S3 class 'ramp.fit', which includes:
# - n: Sample size.
# - p: Number of main effects.
# - family: The response family used in the model.
# - hier: The hierarchy parameter used.
# - a0: The intercept value.
# - compact: A compact representation of selected variables, including indices of the variables and 
#   their estimated coefficients.

# Install and load necessary packages
if (!require(RAMP)) install.packages("RAMP")
library(RAMP)

ramp.fit <- function(x, y, family = "gaussian", hier = "Strong", ...){
  
  n <- nrow(x)
  p <- ncol(x)
  
  # Call RAMP algorithm for logistic regression model fitting
  fit <- RAMP::RAMP(X=x, y=y, family = family, hier = hier, ...)
  
  # Grab coefficient estimate
  a0 <- fit$a0
  # Main effects
  main <- fit$mainInd
  if(is.null(main)){
    compact.m <- NULL
  }else{
    main.ix <-  matrix(cbind(rep(0,length(main)), main), ncol=2)
    main.coef <- fit$beta.m
    compact.m <- matrix(cbind(main.ix, main.coef), ncol=3)
  }
  
  # Interactions
  inter <- fit$interInd
  if(is.null(inter)){
    compact.i <- NULL
  }else{
    inter.ix <- matrix(unlist(regmatches(inter, gregexpr("[[:digit:]]+", inter))),ncol=2,byrow = T)
    inter.coef <- fit$beta.i
    compact.i <- matrix(cbind(inter.ix, inter.coef),ncol=3)
  }
  
  # We now combine estimated main effects and interactions coefficients
  compact <- matrix(as.numeric(rbind(compact.m, compact.i)), ncol=3)
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