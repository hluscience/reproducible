# cv.glinternet Function Overview
#
# Learn a quadratic regression model via hierarchical group-lasso regularization from glinternet package. 
#
# Inputs:
# - x: An n by p design matrix of main effects. Each row is an observation of p main effects.
# - y: A response vector of size n.
# - family: family response type. Default is ’gaussian’. The other choice is ’binomial’ for logistic regression.
# - ...: Additional arguments to be passed to the glinternet function.
#
# Outputs:
# Returns an object of S3 class 'cv.glinternet', which includes:
# - n: Sample size.
# - p: Number of main effects.
# - family: The response family used in the model.
# - a0: The intercept value.
# - compact: A compact representation of selected variables, including indices of the variables and 
#   their estimated coefficients.
#

# Install and load necessary packages
if (!require(glinternet)) install.packages("glinternet")
library(glinternet)

cv.glinternet <- function(x, y, family = "gaussian", ...){
  
  n <- nrow(x)
  p <- ncol(x)
  
  # Conduct cross validation for glinternet and returns a value of lambda.
  # numLevels: Number of levels for each variable, of length p. Set to 1 for continuous variables.
  fit <- glinternet::glinternet.cv(x, y, numLevels = rep(1, p), family = family, ...)
  
  # Grab estimated coefficients
  a0 <- fit$betahat[[1]][1]
  main.ix <- coef(fit)$mainEffects$cont
  main.n <- length(main.ix)
  inter.ix <- coef(fit)$interactions$contcont
  inter.n <- nrow(inter.ix)
  ix <- matrix(rbind(cbind(rep(0,main.n),main.ix),inter.ix),ncol=2)
  
  # Main effects
  main.coef <- NULL
  if(is.null(main.ix)){
    main.coef <- NULL
  }else{
    for(i in seq(main.n)){main.coef <- append(main.coef, coef(fit)$mainEffectsCoef$cont[[i]])}
  }
  
  # Interactions
  inter.coef <- NULL
  if(is.null(inter.ix)){
    inter.coef <- NULL
  }else{
    for(i in seq(inter.n)){inter.coef <- append(inter.coef, coef(fit)$interactionsCoef$contcont[[i]])}
  }
  
  # We now combine estimated main effects and interactions coefficients
  compact <- cbind(ix, c(main.coef,inter.coef))
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  
  out <- list(n = n,
              p = p,
              family = family,
              a0 = a0,
              compact = compact,
              call = match.call())
  class(out) <- "other"
  return(out)
}