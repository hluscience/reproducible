if (!require(ROCR)) install.packages("ROCR")
if (!require(MASS)) install.packages("MASS")
if (!require(glmnet)) install.packages("glmnet")
library(ROCR)
library(MASS)
library(glmnet)
library(sprintr)

# Function to generate synthetic data for theory proof with specified (mix) structure
theory_proof_parm_gen <- function(stru, n, p, coef_inter, ratio, corr_gen = 0) {

  # Construct the covariance matrix
  cov_matrix <- matrix(0, nrow = p, ncol = p)
  for (j in 1:p) {
    for (k in 1:p) {
      cov_matrix[j, k] <- corr_gen^abs(j - k)
    }
  }

  # Generate a random matrix 'x' of predictors
  x <- mvrnorm(n = n, mu = rep(0, p), Sigma = cov_matrix)

  # Define indices for main effects
  ix_main <- cbind(rep(0, 4), 1:4)

  # Define indices for interaction effects based on structure
  if (stru == "anti") {
    ix_inter <- rbind(c(5, 9), c(6, 10), c(7, 11), c(8, 12), c(13, 16), c(14, 17))
  } else if (stru == "mix") {
    ix_inter <- rbind(c(1, 5), c(2, 6), c(7, 8), c(8, 9), c(10, 11), c(12, 13))
  } else if (stru == "hier_weak") {
    ix_inter <- rbind(c(1, 5), c(1, 6), c(2, 7), c(2, 8), c(3, 9), c(3, 10))
  } else if (stru == "hier_strong") {
    ix_inter <- rbind(c(1, 2), c(1, 3), c(1, 4), c(2, 3), c(2, 4), c(3, 4))
  } else {
    stop("Invalid structure specified")
  }

  # Determine the number of main and interaction effects based on the indices
  num_main <- nrow(ix_main)
  num_inter <- nrow(ix_inter)

  # Calculate the interaction effects signal
  mu_inter <- if (num_inter == 0) 0 else rowSums(apply(ix_inter, 1, function(ix) coef_inter * x[, ix[1]] * x[, ix[2]]))

  # Calculate the linear predictors for main effects without coefficients
  mu_main <- if (num_main == 0) 0 else rowSums(x[, ix_main[, 2]])

  # Calculate interaction effects coefficient
  signal_inter <- sqrt(sum(mu_inter^2))
  signal_main <- ratio * signal_inter
  coef_main <- signal_main / sqrt(sum(mu_main^2))

  # Construct coefficients and indices
  coef <- c(rep(coef_main, length.out = num_main),
            rep(coef_inter, length.out = num_inter))
  ix <- rbind(ix_main, ix_inter)

  # Combine indices and coefficients into a 'compact' matrix
  compact <- cbind(ix, coef)
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  out <- list(x = x,
              n = n,
              p = p,
              corr_gen = corr_gen,
              compact = compact,
              num_main = num_main,
              num_inter = num_inter,
              coef_main = coef_main,
              coef_inter = coef_inter)
  return(out)
}

# Function to generate data based on the parameters
theory_proof_data_gen <- function(x, compact) {

  # Extract indices and coefficients from the 'compact' input
  ix <- compact[, 1:2]
  coef <- compact[, 3]

  # Count the number of main and interaction effects
  num_main <- sum(ix[, 1] == 0)
  num_inter <- sum(ix[, 1] != 0)

  # Calculate the linear predictors for main effects
  mu_main <- if (num_main == 0) 0 else rowSums(sapply(1:num_main, function(i) coef[i] * x[, ix[i, 2]]))

  # Calculate the linear predictors for interaction effects
  mu_inter <- if (num_inter == 0) 0 else rowSums(sapply(1:num_inter, function(k) coef[k + num_main] * x[, ix[k + num_main, 1]] * x[, ix[k + num_main, 2]]))

  # Calculate the logit transformation and generate the binary response 'y'
  mu <- mu_main + mu_inter
  pr <- 1 / (1 + exp(-mu))  # Apply the logistic (sigmoid) function
  y <- rbinom(nrow(x), 1, pr)  # Bernoulli response variable

  out <- list(x = x,
              y = y)

  return(out)
}

# Function to generate population data for identifying pure interactions
theory_proof_popu_data_gen <- function(n, p, corr_gen, compact){

  # Construct the covariance matrix
  cov_matrix <- matrix(0, nrow = p, ncol = p)
  for (j in 1:p) {
    for (k in 1:p) {
      cov_matrix[j, k] <- corr_gen^abs(j - k)
    }
  }

  # Generate a random matrix 'x' of predictors
  x <- mvrnorm(n = n, mu = rep(0, p), Sigma = cov_matrix)

  # Extract indices and coefficients from the 'compact' input
  ix <- compact[, 1:2]
  coef <- compact[, 3]

  # Count the number of main and interaction effects
  num_main <- sum(ix[, 1] == 0)
  num_inter <- sum(ix[, 1] != 0)

  # Calculate the linear predictors for main effects
  mu_main <- if (num_main == 0) 0 else rowSums(sapply(1:num_main, function(i) coef[i] * x[, ix[i, 2]]))

  # Calculate the linear predictors for interaction effects
  mu_inter <- if (num_inter == 0) 0 else rowSums(sapply(1:num_inter, function(k) coef[k + num_main] * x[, ix[k + num_main, 1]] * x[, ix[k + num_main, 2]]))

  # Calculate the logit transformation and generate the binary response 'y'
  mu <- mu_main + mu_inter
  pr <- 1 / (1 + exp(-mu))  # Apply the logistic (sigmoid) function
  y <- rbinom(nrow(x), 1, pr)  # Bernoulli response variable

  out <- list(x = x,
              y = y)
  return(out)
}

# Function to identify pure interactions that cannot be captured by main effects
pure_inter_get <- function(parm, kappa){

  # Set population data size
  n <- 50000
  p <- parm$p

  # Generate a large population data sample
  popu_data_approx <- theory_proof_popu_data_gen(n = n, p = p, corr_gen = parm$corr_gen, parm$compact)

  # Step 1: Fit lasso on main effects
  # Standardize the population data
  x_scale <- myscale(popu_data_approx$x)
  col_mean <- attr(x = x_scale, which = "scaled:center")
  col_sd <- attr(x = x_scale, which = "scaled:scale")

  # Fit on main effects
  popu_data_design <- data.frame(y = popu_data_approx$y, x_scale)
  model <- glm(y ~ ., data = popu_data_design, family = binomial)
  coef <- coef(model)[-1] / col_sd
  a0 <-  as.numeric(coef(model)[1] - crossprod(col_mean, coef))

  # Step 2: Screen interactions
  # Calculate the total number of possible interactions (square effects and pairwise interactions)
  q <- (p^2 + p) / 2

  # Obtain the offset
  offset <- popu_data_approx$x %*% coef + a0

  # Calculate the signal strength threshold for pure interactions
  signal <- n^(-kappa)

  # Screen interactions using MMLE for each interaction
  idx <- sprintr::screen_cpp_MMLE_binomial(x = popu_data_approx$x, y = popu_data_approx$y, offset = offset, num_keep = q, square = FALSE, main_effect = FALSE)
  idx <- idx[!is.na(idx[, 3]), , drop = FALSE] # remove NA if there is any
  colnames(idx) <- c("index_1", "index_2", "score")

  # Filter pure interaction indices where the score is greater than the signal strength threshold
  pure_inter_idx <- idx[idx[, "score"] > signal, ]

  out <- list(coef = coef,
              a0 = a0,
              pure_inter_idx = pure_inter_idx
  )

  return(out)
}

# Function to calculate True Positive Rate (TPR) and False Positive Rate (FPR)
TPR_FPR_get <- function(p, inter_idx, pure_inter_idx) {

  # Filter to find correctly identified pure interactions
  filter_true <- apply(inter_idx[, c("index_1", "index_2"), drop = FALSE], 1, function(row) {
    any(row[1] == pure_inter_idx[, "index_1"] & row[2] == pure_inter_idx[, "index_2"])
  })
  identified_inter_idx <- inter_idx[filter_true, , drop = FALSE]

  # Calculate True Positive Rate (TPR)
  TPR <- nrow(identified_inter_idx) / nrow(pure_inter_idx)

  # Filter to find false positive interactions
  filter_false <- apply(inter_idx[, c("index_1", "index_2"), drop = FALSE], 1, function(row) {
    !any(row[1] == pure_inter_idx[, "index_1"] & row[2] == pure_inter_idx[, "index_2"])
  })
  not_identified_inter_idx <- inter_idx[filter_false, , drop = FALSE]

  # Calculate False Positive Rate (FPR)
  q <- (p^2 + p) / 2  # Total possible interactions
  FPR <- nrow(not_identified_inter_idx) / (q - nrow(pure_inter_idx))

  out <- list(TPR = TPR,
              FPR = FPR)

  return(out)
}

# Function to run sprinter and evaluate the step 1 misspecification error and step 2 selecting interactions TPR & FPR
increasing_n_method_run_eval<- function(x, y, pure_inter) {

  # obtain n and p
  n <- nrow(x)
  p <- ncol(x)

  # Compute the main population quantity for the signal X'beta^M
  main_popu_quantity <- x %*% pure_inter$coef + pure_inter$a0

  # obtain pure interaction indices obtained from population data
  pure_inter_idx <- pure_inter$pure_inter_idx

  # Run sprinter
  # Step 1: Fit lasso on main effects to get the misspecification error from Step 1
  # Standardize the sample data
  x_scale <- myscale(x)
  col_mean <- attr(x = x_scale, which = "scaled:center")
  col_sd <- attr(x = x_scale, which = "scaled:scale")

  # Fit lasso on main effects
  lambda1 <- get_lambda(x = x_scale, y = y, family = "binomial", nlam = 100, lam_min_ratio = 0.01)
  fit <- glmnet::cv.glmnet(x = x_scale, y = y, family = "binomial",
                           lambda = lambda1,
                           intercept = TRUE,
                           standardize = FALSE,
                           type.measure = "deviance")

  # Compute the main sample quantity for the fitted main effects signal X'beta^hat
  ibest <- which.min(fit$cvm)
  beta <- as.numeric(fit$glmnet.fit$beta[, ibest])
  beta <- beta / col_sd
  a0 <- fit$glmnet.fit$a0[ibest] + as.numeric(- crossprod(col_mean, beta))
  main_sample_quantity <- x %*% beta + a0

  # Calculate the misspecification error
  misspec <- mean((main_sample_quantity - main_popu_quantity)^2)
  eval_vec$misspec_sprinter <- append(eval_vec$misspec_sprinter, misspec)

  # Step 2: Screen interactions to get the TPR & FPR from step 2
  #sprintr_idx <- screen_cpp(x = x_scale, y = y, nlev = 2, offset = main_sample_quantity,
  #                          num_keep = ceiling(n /log(n)), square = FALSE, family = "binomial")
  sprintr_idx <- screen_cpp(x = x_scale, y = y, nlev = 2, offset = main_sample_quantity,
                            num_keep = 22, square = FALSE, family = "binomial")
  sprintr_idx <- sprintr_idx[!is.na(sprintr_idx[, 3]), , drop = FALSE]
  sprintr_idx <- sprintr_idx[order(sprintr_idx[, 3], decreasing = TRUE), , drop = FALSE]
  colnames(sprintr_idx) <- c("index_1", "index_2", "score")

  # Calculate TPR and FPR for sprinter method
  TPR_FPR <- TPR_FPR_get(p, sprintr_idx, pure_inter_idx)
  eval_vec$FPR_sprinter <- append(eval_vec$FPR_sprinter, TPR_FPR$FPR)
  eval_vec$TPR_sprinter <- append(eval_vec$TPR_sprinter, TPR_FPR$TPR)

  return(eval_vec)
}

# Function to run sprinter and evaluate interactions strength as correlation changes
increasing_corr_run <- function(x, y, pure_inter) {

  # obtain n and p
  n <- nrow(x)
  p <- ncol(x)

  # obtain pure interaction indices obtained from population data
  pure_inter_idx <- pure_inter$pure_inter_idx

  # Run sprinter
  # Step 1: Fit lasso on main effects to get the misspecification error from Step 1
  # Standardize the sample data
  x_scale <- myscale(x)
  col_mean <- attr(x = x_scale, which = "scaled:center")
  col_sd <- attr(x = x_scale, which = "scaled:scale")

  # Fit lasso on main effects
  lambda1 <- get_lambda(x = x_scale, y = y, family = "binomial", nlam = 100, lam_min_ratio = 0.01)
  fit <- glmnet::cv.glmnet(x = x_scale, y = y, family = "binomial",
                           lambda = lambda1,
                           intercept = TRUE,
                           standardize = FALSE,
                           type.measure = "deviance")

  # Compute the main sample quantity for the fitted main effects signal X'beta^hat
  ibest <- which.min(fit$cvm)
  beta <- as.numeric(fit$glmnet.fit$beta[, ibest])
  beta <- beta / col_sd
  a0 <- fit$glmnet.fit$a0[ibest] + as.numeric(- crossprod(col_mean, beta))
  main_sample_quantity <- x %*% beta + a0

  # Step 2: Screen interactions to get the TPR & FPR from step 2
  #sprintr_idx <- screen_cpp(x = x_scale, y = y, nlev = 2, offset = main_sample_quantity,
  #                          num_keep = ceiling(n /log(n)), square = FALSE, family = "binomial")
  sprintr_idx <- screen_cpp(x = x_scale, y = y, nlev = 2, offset = main_sample_quantity,
                            num_keep = (p^2+p)/2, square = FALSE, family = "binomial")
  sprintr_idx <- sprintr_idx[!is.na(sprintr_idx[, 3]), , drop = FALSE]
  sprintr_idx <- sprintr_idx[order(sprintr_idx[, 3], decreasing = TRUE), , drop = FALSE]
  colnames(sprintr_idx) <- c("index_1", "index_2", "score")

  return(sprintr_idx)
}


# Function to select interaction effects using the sprinter method
sprinter_select_inter <- function(x, y, family = "gaussian", nlev = 1, type.measure = "deviance", square = FALSE, num_keep = NULL, lambda1 = NULL, nlam1 = 10, lam_min_ratio = ifelse(nrow(x) < ncol(x), 0.01, 1e-04)){

  n <- nrow(x)
  p <- ncol(x)
  q <- ifelse(square, p * (p - 1) / 2, p * (p - 1) / 2 + p)

  if(family == "binomial"){
    nlev <- 2
  }

  if(is.null(num_keep)){
    num_keep <- ceiling(n /log(n))
  }else{
    stopifnot(num_keep > 0 & num_keep <= q)}

  # we always standardize the design matrix to get main effects
  # squared effects and interactions are built upon standardized main effects
  x <- myscale(x)
  # xm is the (standardized) design matrix of main effects
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")
  xm <- x

  # The First Step
  # run lasso on
  # (1) main effects M (square == FALSE)
  # (2) main effects M + squared main effects M^2 (square == TRUE)
  # return the fitted value of response
  if(square){
    x_sq <- myscale(x^2)
    col_mean <- c(col_mean, attr(x_sq, which = "scaled:center"))
    col_sd <- c(col_sd, attr(x_sq, which = "scaled:scale"))
    x <- cbind(x, x_sq)
  }

  # initiate lambda 1
  if(is.null(lambda1)){
    lambda1 <- get_lambda(x = x, y = y, family = family, nlam = nlam1, lam_min_ratio = lam_min_ratio)
  }

  # by default, type.measure = "deviance" is used as the loss for CV
  # i.e., mse for "gaussian", deviance for logistic and Poisson
  fit <- glmnet::cv.glmnet(x = x, y = y, family = family,
                           lambda = lambda1,
                           intercept = TRUE,
                           standardize = FALSE,
                           type.measure = type.measure)

  # grab coefficient estimate
  theta <- matrix(fit$glmnet.fit$beta[, which.min(fit$cvm)], ncol = 1)
  a0 <- as.numeric(fit$glmnet.fit$a0[which.min(fit$cvm)])
  colnames(theta) <- NULL
  rownames(theta) <- NULL

  # fitted value of mu, i.e., the linear part
  mu <- matrix(a0 + as.numeric(x %*% theta), ncol = 1)

  # The Second Step
  # find num_keep higher order terms from
  # (1) squared main effects M^2 + Interaction effects I
  #     (square == FALSE)
  # (2) Interaction effects I (square == TRUE)
  if(inherits(x, "sparseMatrix")){
    idx <- screen_sparse_cpp(x = xm, y = y, yMat = Matrix::Matrix(0, sparse = TRUE), nlev = nlev, offset = mu, num_keep = num_keep, square = square, family = family)
  }
  else{
    idx <- screen_cpp(x = xm, y = y, nlev = nlev, offset = mu, num_keep = num_keep, square = square, family = family)
  }

  # remove NA if there is any
  idx <- idx[!is.na(idx[, 3]), , drop = FALSE]

  # preparing for Step 3
  idx <- idx[order(idx[, 3], decreasing = TRUE), , drop = FALSE]
  colnames(idx) <- c("index_1", "index_2", "score")

  return(idx)
}

# Function to select interaction effects using the oracle method
oracle_select_inter <- function(x, y, family = "gaussian", num_keep = NULL, type.measure = "deviance"){

  # x is the unstandardized main effects
  p <- ncol(x)
  n <- nrow(x)
  # Total number of square effects and pairwise interactions
  q <- (p^2 + p) / 2

  # Set default value for num_keep if not specified
  if(is.null(num_keep))
    num_keep <- ceiling(n / log(n))

  # Ensure num_keep does not exceed the number of possible interactions
  num_keep <- min(num_keep, q)

  # Standardize the main effects
  x <- myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")

  # idx contains indices for square effects and pairwise interactions
  idx <- rbind(cbind(seq(p), seq(p)), t(combn(p, 2)))

  # Initialize an empty matrix to store interaction strengths
  interaction_strengths <- matrix(NA, nrow = q, ncol = 3)

  # Iterate through each interaction index
  for (i in 1:q) {

    # Construct the design matrix
    xx <- myscale(x[, idx[i, 1]] * x[, idx[i, 2]])
    design <- cbind(x, xx)
    col_mean_cb <- c(col_mean, attr(x = xx, which = "scaled:center"))
    col_sd_cb <- c(col_sd, attr(x = xx, which = "scaled:scale"))

    # Fit cv.glmnet
    fit <- glmnet::cv.glmnet(x = design, y = y,
                             family = family,
                             type.measure = type.measure,
                             intercept = TRUE,
                             standardize = FALSE)

    # Extract coefficients at the best lambda
    ibest <- which.min(fit$cvm)
    beta <- as.numeric(fit$glmnet.fit$beta[, ibest])
    # beta <- beta / col_sd_cb

    # Store interaction strengths
    interaction_strengths[i, ] <- c(idx[i, 1], idx[i, 2], abs(beta[p + 1]))

  }

  # Select the top 'num_keep' interactions
  ordered_interactions <- interaction_strengths[order(interaction_strengths[, 3], decreasing = TRUE), ]
  top_interactions <- ordered_interactions[1:num_keep, ]
  top_interactions <- top_interactions[top_interactions[,3] != 0, ]
  colnames(top_interactions) <- c("index_1", "index_2", "score")

  return(top_interactions)
}

# Function to compare sprinter and oracle methods in terms of time, TPR, and FPR
compare_with_oracle_method_run_eval <- function(x, y, pure_inter_idx){

  # Run sprinter method
  start.time <- Sys.time()
  sprinter_idx <- sprinter_select_inter(x = x, y = y, family = "binomial", square = FALSE, nlam1 = 100, lam_min_ratio = 0.01)
  end.time <- Sys.time()
  time_spend <- as.numeric(difftime(end.time, start.time, units = "secs"))
  eval_vec$time_sprinter <- append(eval_vec$time_sprinter, time_spend)

  # Calculate TPR and FPR for sprinter method
  TPR_FPR <- TPR_FPR_get(ncol(x), sprinter_idx, pure_inter_idx)
  eval_vec$FPR_sprinter <- append(eval_vec$FPR_sprinter, TPR_FPR$FPR)
  eval_vec$TPR_sprinter <- append(eval_vec$TPR_sprinter, TPR_FPR$TPR)
  cat("sprinter Done", fill = TRUE)

  # Run oracle method
  start.time <- Sys.time()
  oracle_idx <- oracle_select_inter(x = x, y = y, family = "binomial")
  end.time <- Sys.time()
  time_spend <- as.numeric(difftime(end.time, start.time, units = "secs"))
  eval_vec$time_oracle <- append(eval_vec$time_oracle, time_spend)

  # Calculate TPR and FPR for oracle method
  TPR_FPR <- TPR_FPR_get(ncol(x), oracle_idx, pure_inter_idx)
  eval_vec$FPR_oracle <- append(eval_vec$FPR_oracle, TPR_FPR$FPR)
  eval_vec$TPR_oracle <- append(eval_vec$TPR_oracle, TPR_FPR$TPR)
  cat("oracle Done", fill = TRUE)

  return(eval_vec)
}


