# Binomial simulation
# Function to generate parameters based on different architectures
parm_gen <- function(stru, n, p, coef_inter, ratio) {

  # Generate predictor matrix
  x <- matrix(rnorm(n * p), n, p)

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

  # Calculate the main effects signal
  mu_main <- if (num_main == 0) 0 else rowSums(coef_main * x[, ix_main[, 2]])

  # Construct coefficients and indices
  coef <- c(rep(coef_main, length.out = num_main),
            rep(coef_inter, length.out = num_inter))
  ix <- rbind(ix_main, ix_inter)

  # Combine indices and coefficients into a 'compact' matrix
  compact <- cbind(ix, coef)
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  out <- list(x = x,
              compact = compact,
              num_main = num_main,
              num_inter = num_inter,
              coef_main = coef_main,
              coef_inter = coef_inter)
  return(out)
}

# Function to generate data based on the parameters
data_gen <- function(x, compact) {

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

# Functions to gereate deviance ratio
fit_main_oracle <- function(x, y, compact, family = "gaussian") {

  # Extract true main effects from compact
  true_main_idx <- compact[compact[, "index_1"] == 0, "index_2"]

  # Select the columns corresponding to true main effects
  x_true_main <- x[, true_main_idx, drop = FALSE]

  # Standardize the selected columns
  x_true_main <- myscale(x_true_main)
  col_mean <- attr(x = x_true_main, which = "scaled:center")
  col_sd <- attr(x = x_true_main, which = "scaled:scale")

  # Fit glm model
  fit <- glm(y ~ x_true_main, family = family)

  # Extract coefficients and intercept
  beta <- coef(fit)[-1]
  a0 <- coef(fit)[1]

  # Scale estimates back to the original scale of x
  beta <- beta / col_sd
  a0 <- a0 - sum(col_mean * beta)

  compact <- cbind(cbind(rep(0, length(beta)), true_main_idx)[beta != 0, , drop = FALSE], beta[beta != 0])
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  # Return result
  out <- list(n = nrow(x),
              p = ncol(x),
              a0 = a0,
              compact = compact,
              call = match.call())

  class(out) <- "other"
  return(out)
}

fit_main_inter_oracle <- function(x, y, compact, family = "gaussian") {

  # Extract true main and interaction effects from compact
  true_main_idx <- compact[compact[, "index_1"] == 0, "index_2"]
  true_inter_idx <- compact[compact[, "index_1"] != 0, c("index_1", "index_2")]

  # Standardize the design matrix x
  x <- myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")

  # Create interaction terms
  interaction_terms <- myscale(apply(true_inter_idx, 1, function(idx) x[, idx[1]] * x[, idx[2]]))
  col_mean_inter <- attr(interaction_terms, "scaled:center")
  col_sd_inter <- attr(interaction_terms, "scaled:scale")

  # Combine main effects and interaction terms
  x_true_main <- x[, true_main_idx, drop = FALSE]
  x_combined <- cbind(x_true_main, interaction_terms)

  # Combine means and standard deviations
  col_mean <- c(col_mean[true_main_idx], col_mean_inter)
  col_sd <- c(col_sd[true_main_idx], col_sd_inter)

  # Fit glm model
  fit <- glm(y ~ x_combined, family = family)

  # Extract coefficients and intercept
  beta <- coef(fit)[-1]
  a0 <- coef(fit)[1]

  # Scale estimates back to the original scale of x
  beta <- beta / col_sd
  a0 <- a0 - sum(col_mean * beta)

  compact_result <- cbind(rbind(cbind(rep(0, length(true_main_idx)), true_main_idx), true_inter_idx)[beta != 0, , drop = FALSE], beta[beta != 0])
  colnames(compact_result) <- c("index_1", "index_2", "coefficient")

  # Return result
  out <- list(n = nrow(x),
              p = ncol(x),
              a0 = a0,
              compact = compact_result,
              call = match.call())

  class(out) <- "other"
  return(out)
}

fit_inter_oracle <- function(x, y, compact, family = "gaussian") {

  # Extract true interaction effects from compact
  true_inter_idx <- compact[compact[, "index_1"] != 0, c("index_1", "index_2")]

  # Standardize the design matrix x
  x <- myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")

  # Create interaction terms
  interaction_terms <- myscale(apply(true_inter_idx, 1, function(idx) x[, idx[1]] * x[, idx[2]]))
  col_mean_inter <- attr(interaction_terms, "scaled:center")
  col_sd_inter <- attr(interaction_terms, "scaled:scale")

  # Fit glm model
  fit <- glm(y ~ interaction_terms, family = family)

  # Extract coefficients and intercept
  beta <- coef(fit)[-1]
  a0 <- coef(fit)[1]

  # Scale estimates back to the original scale of x
  beta <- beta / col_sd_inter
  a0 <- a0 - sum(col_mean_inter * beta)

  compact_result <- cbind(true_inter_idx[beta != 0, , drop = FALSE], beta[beta != 0])
  colnames(compact_result) <- c("index_1", "index_2", "coefficient")

  # Return result
  out <- list(n = nrow(x),
              p = ncol(x),
              a0 = a0,
              compact = compact_result,
              call = match.call())

  class(out) <- "other"
  return(out)
}

dev_ratio_gen <- function(n = 20000, p, compact){

  # Generate large dataset
  x <- matrix(rnorm(n * p), n, p)

  # Extract indices and coefficients for main and interaction effects from compact
  main_effects <- compact[compact[, "index_1"] == 0, ]
  interaction_effects <- compact[compact[, "index_1"] != 0, ]

  # Calculate the main effects signal
  mu_main <- rowSums(sapply(seq_len(nrow(main_effects)), function(i) {
    coef <- main_effects[i, "coefficient"]
    idx <- main_effects[i, "index_2"]
    coef * x[, idx]
  }))

  # Calculate the interaction effects signal
  mu_inter <- rowSums(sapply(seq_len(nrow(interaction_effects)), function(i) {
    coef <- interaction_effects[i, "coefficient"]
    idx1 <- interaction_effects[i, "index_1"]
    idx2 <- interaction_effects[i, "index_2"]
    coef * x[, idx1] * x[, idx2]
  }))

  # Generate y
  mu <- mu_main + mu_inter
  pr <- 1 / (1 + exp(-mu))  # Apply the logistic (sigmoid) function
  y <- rbinom(nrow(x), 1, pr)  # Bernoulli response variable

  # Split data
  train_size <- floor(0.5 * n)
  train_indx <- sample(seq_len(n), size = train_size)
  x_train <- x[train_indx,]
  y_train <- y[train_indx]
  x_test <- x[-train_indx,]
  y_test <- y[-train_indx]

  # Deviance ratio
  main_oracle <- fit_main_oracle(x_train, y_train, compact, family = "binomial")
  inter_oracle <- fit_inter_oracle(x_train, y_train, compact, family = "binomial")
  main_inter_oracle <- fit_main_inter_oracle(x_train, y_train, compact, family = "binomial")

  # on test set
  main_oracle_mu_test <- sprintr::predict.other(main_oracle, x_test) # predictions
  main_oracle_dev_test <- compute_deviance_logistic(y_test, main_oracle_mu_test) # deviance
  main_inter_oracle_mu_test <- sprintr::predict.other(main_inter_oracle, x_test) # predictions
  main_inter_oracle_dev_test <- compute_deviance_logistic(y_test, main_inter_oracle_mu_test) # deviance
  inter_oracle_mu_test <- sprintr::predict.other(inter_oracle, x_test) # predictions
  inter_oracle_dev_test <- compute_deviance_logistic(y_test, inter_oracle_mu_test) # deviance
  main_inter_ratio <- main_oracle_dev_test / inter_oracle_dev_test
  inter_main_ratio <- inter_oracle_dev_test / main_oracle_dev_test
  main_all_ratio <- main_oracle_dev_test / main_inter_oracle_dev_test

  out <- list(main_oracle_dev_test = main_oracle_dev_test,
              main_inter_oracle_dev_test = main_inter_oracle_dev_test,
              inter_oracle_dev_test = inter_oracle_dev_test,
              main_inter_ratio = main_inter_ratio,
              inter_main_ratio = inter_main_ratio,
              main_all_ratio = main_all_ratio)
  return(out)
}

# Function to generate main effects coefficients for different structures and ratio settings
ratio_gen_main <- function(stru, n, p, ratios){

  # Initialize a vector to store the main effects coefficients for each ratio
  coef_main <- numeric(length = length(ratios))

  for (i in seq(ratios)) {
    # Generate parameters based on the specified structure
    para <- parm_gen(stru = stru, n = n, p = p, coef_inter = 2, ratio = ratios[i])

    # Generate data based on the parameters
    data <- data_gen(x = para$x, compact = para$compact)

    # Extract the main effects coefficient from the generated data
    coef_main[i] <- para$coef_main
  }

  return(coef_main)
}


# Example usage:
# Set parameters
stru <- "hier_strong"  # can be "anti", "mix", "hier_weak" or "hier_strong"
n <- 200
p <- 150
ratios <- c(0.1, 0.4, 0.7, 1, 1.3)

# Get main effects coefficients for the specified structure and ratio settings
coef_main <- ratio_gen_main(stru, n, p, ratios)
# print(coef_main)

################################################################################
# Initialization and loading functions
# rm(list=ls())
# source("binomial_parm_generate.R") # Source the parameter generation script
# source("binomial_eval.R") # Source the evaluation script
# source("../method_run.R") # Source the method running script
source("../binomial_eval.R") # Source the evaluation script
source("../../method_run_test.R") # Source the method running script


# Setting the parameters for the simulation
n <- 200 # Number of samples (both training and test)
p <- 150 # Number of main effects
stru <- "hier_strong" # Structure type of the simulated data. Options are "anti", "mix", "hier_weak" and "hier_strong".
ratios <- c(0.1, 0.4, 0.7, 1, 1.3) # Different main vs. interaction effects signal strength
method <- c("sprinter", "glinternet", "MEL", "APL", "SIS", "RAMP", "hierFabs", "hierNet") # Methods to be evaluated
num_sim <- 50 # Number of simulations
family <- "binomial" # Family of the response variable

# Initialize lists to store evaluation results
eval_list <- vector("list", length(ratios))
main_coef <- c()

# Loop over each ratio setting
for (i in seq_along(ratios)){

  set.seed(405)
  start.time.i <- Sys.time()
  message(sprintf("Processing ratio: %s", ratios[i]))

  # Generate parameters based on the current ratio
  parm <- parm_gen(stru = stru, n = n, p = p, coef_inter = 2, ratio = ratios[i])
  main_coef <- c(main_coef, round(parm$coef_main, 2))

  # Initialize lists to store evaluation results for each simulation
  eval_vec <- list()

  # Loop over each simulation
  for(j in 1:num_sim){

    set.seed(j)
    start.time.j <- Sys.time()

    # Generate data based on the current parameters
    data <- data_gen(x = parm$x, compact = parm$compact)

    # Extract predictor and response variables from the generated data
    x <- data$x
    y <- data$y
    train_size <- floor(0.5 * n)
    train_indx <- sample(seq_len(n), size = train_size)
    x_train <- x[train_indx,]
    y_train <- y[train_indx]
    x_test <- x[-train_indx,]
    y_test <- y[-train_indx]

    # Run each method on the training data
    result <- method_run(x = x_train, y = y_train, method = method, family = family)

    # Evaluate each method based on the testing data
    eval_vec <- eval.vec(result, x_test, y_test, method)

    message(sprintf("Simulation %d completed for ratio %s in structure %s", j, ratios[i], stru))
    end.time.j <- Sys.time()
    time <- end.time.j - start.time.j
    message(sprintf("Time taken for simulation %d: %s", j, time))
  }

  # Save the evaluation results
  eval_list[[i]] <- eval_vec

  message(sprintf("Ratio %s completed for structure %s", ratios[i], stru))
  end.time.i <- Sys.time()
  time <- end.time.i - start.time.i
  message(sprintf("Time taken for ratio %s: %s", ratios[i], time))

  # Initialize vectors to store mean values
  mean_dev <- c()
  mean_auc <- c()

  # Calculate the mean for each method
  for (name in names(eval_vec)) {
    if (grepl("dev", name)) {
      mean_dev[name] <- mean(eval_vec[[name]])
    } else if (grepl("auc", name)) {
      mean_auc[name] <- mean(eval_vec[[name]])
    }
  }

  message(sprintf("dev for sprinter: %s", mean_dev["dev_sprinter"]))
  message(sprintf("dev for MEL: %s", mean_dev["dev_MEL"]))
  message(sprintf("dev for glinternet: %s", mean_dev["dev_glinternet"]))
  message(sprintf("auc for sprinter: %s", mean_auc["auc_sprinter"]))
  message(sprintf("auc for MEL: %s", mean_auc["auc_MEL"]))
  message(sprintf("auc for glinternet: %s", mean_auc["auc_glinternet"]))
}


# Save the evaluation results and ratio to disk
saveRDS(eval_list, file = paste0(stru, "_eval_list.rds"))
saveRDS(main_coef, file = paste0(stru, "_main_coef.rds"))
