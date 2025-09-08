if (!require(ROCR)) install.packages("ROCR")
library(ROCR)
library(sprintr)
source("../other_methods/cv.glinternet.R")

# Function to generate synthetic data for APL comparison with specified (mix) structure
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

# Function to run methods and evaluate their performance based on time, deviance and AUC.
compare_with_APL_method_run_eval <- function(x, y, eta_seq, family, x_test, y_test){

  # Run glinternet
  start.time <- Sys.time()
  glinternet <- cv.glinternet(x=x, y=y, family = family)
  end.time <- Sys.time()

  # Store time spent for glinternet
  time_spend <- as.numeric(difftime(end.time, start.time, units = "secs"))
  eval_vec$time_glinternet <- append(eval_vec$time_glinternet, time_spend)

  # Make predictions using the glinternet model
  mu_test <- sprintr::predict.other(glinternet, x_test)

  # Calculate deviance for glinternet
  dev <- compute_deviance_logistic(y_test, mu_test)
  eval_vec$dev_glinternet <- append(eval_vec$dev_glinternet, dev)

  # Calculate AUC for glinternet
  pp_test <- 1/(1+exp(-mu_test))
  pred <- ROCR::prediction(pp_test, y_test)
  auc.tmp <- ROCR::performance(pred, "auc")
  eval_vec$auc_glinternet <- append(eval_vec$auc_glinternet, as.numeric(auc.tmp@y.values))
  cat("glinternet Done", fill = TRUE)

  # Run sprinter
  start.time <- Sys.time()
  sprintr <- sprintr::cv.sprinter(x = x, y = y, family = family, cv_step1 = TRUE, square = FALSE, lam_min_ratio = 0.01, nlam1 = 100)
  end.time <- Sys.time()

  # Store time spent for sprinter
  time_spend <- as.numeric(difftime(end.time, start.time, units = "secs"))
  eval_vec$time_sprinter <- append(eval_vec$time_sprinter, time_spend)

  # Make predictions using the sprinter model
  mu_test <- sprintr::predict.cv.sprinter(sprintr, x_test)

  # Calculate deviance for sprinter
  dev <- compute_deviance_logistic(y_test, mu_test)
  eval_vec$dev_sprinter <- append(eval_vec$dev_sprinter, dev)

  # Calculate AUC for sprinter
  pp_test <- 1/(1+exp(-mu_test))
  pred <- ROCR::prediction(pp_test, y_test)
  auc.tmp <- ROCR::performance(pred, "auc")
  eval_vec$auc_sprinter <- append(eval_vec$auc_sprinter, as.numeric(auc.tmp@y.values))
  cat("sprinter Done", fill = TRUE)

  # Run APL method with different eta values which control the relative penalty between main effects and interactions
  for (i in seq_along(eta_seq)) {
    current_eta <- eta_seq[i]

    # Run APL
    start.time <- Sys.time()
    APL_value <- sprintr::apl(x = x, y = y, eta = current_eta, family = family)
    end.time <- Sys.time()
    time_spend <- as.numeric(difftime(end.time, start.time, units = "secs"))

    # Create dynamic names based on current eta value
    time_name <- paste0("time_APL_", current_eta)
    dev_name <- paste0("dev_APL_", current_eta)
    auc_name <- paste0("auc_APL_", current_eta)

    # Measure time taken for APL method with current eta
    eval_vec[[time_name]] <- append(eval_vec[[time_name]], time_spend)

    # Make predictions using the APL model with current eta
    mu_test <- sprintr::predict.other(APL_value, x_test)

    # Calculate deviance for the current APL model
    dev <- compute_deviance_logistic(y_test, mu_test)
    eval_vec[[dev_name]] <- append(eval_vec[[dev_name]], dev)

    # Calculate AUC for the current APL model
    pp_test <- 1 / (1 + exp(-mu_test))
    pred <- ROCR::prediction(pp_test, y_test)
    auc.tmp <- ROCR::performance(pred, "auc")
    eval_vec[[auc_name]] <- append(eval_vec[[auc_name]], as.numeric(auc.tmp@y.values))

    cat(paste("APL for eta =", current_eta, "Done"), fill = TRUE)
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
