# Run simulations to compare sprinter with APL

# Initialization and loading functions
rm(list=ls())
source("compare_APL_method_eval.R")

# Setting the parameters for the simulation
n <- 200  # Number of samples (both training and test)
p_seq <- c(150, 500, 1000, 1500)  # Sequence of different numbers of predictors
eta_seq <- c(0.3, 0.5, 0.8, 1)  # Sequence of eta values to control the relative penalty between main effects and interactions
num_sim <- 30  # Number of simulations
family <- "binomial"  # Family of the response variable

# Initialize lists to store evaluation results
eval_list <- vector("list", length(p_seq))

# Loop over each p setting
for (i in seq_along(p_seq)){

  set.seed(9)
  start.time.i <- Sys.time()
  message(sprintf("Processing p: %s", p_seq[i]))

  # Generate parameters
  parm <- parm_gen(stru = "mix", n = n, p = p_seq[[i]], coef_inter = 2, ratio = 1)

  # Initialize lists to store evaluation results for each simulation
  eval_vec <- list()

  # Loop over each simulation
  for(j in 1:num_sim){

    set.seed(j)  # Set the seed for reproducibility
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

    # Run each method on the training data and evaluate on the testing data
    eval_vec <- compare_with_APL_method_run_eval(
      x = x_train,
      y = y_train,
      eta_seq = eta_seq,
      family = family,
      x_test = x_test,
      y_test = y_test
    )

    message(sprintf("Simulation %d completed for p = %d", j, p_seq[i]))
    end.time.j <- Sys.time()
    time.j <- as.numeric(difftime(end.time.j, start.time.j, units = "secs"))
    message(sprintf("Time taken for simulation %d: %s seconds", j, round(time.j, 2)))
  }

  # Save the evaluation results
  eval_list[[i]] <- eval_vec

  message(sprintf("Completed for p = %d", p_seq[i]))
  end.time.i <- Sys.time()
  time.i <- as.numeric(difftime(end.time.i, start.time.i, units = "secs"))
  message(sprintf("Time taken for p = %d: %s seconds", p_seq[i], round(time.i, 2)))

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

  message(sprintf("dev for glinternet: %s", mean_dev["dev_glinternet"]))
  message(sprintf("dev for sprinter: %s", mean_dev["dev_sprinter"]))
  message(sprintf("dev for APL_0.3: %s", mean_dev["dev_APL_0.3"]))
  message(sprintf("dev for APL_0.5: %s", mean_dev["dev_APL_0.5"]))
  message(sprintf("dev for APL_0.8: %s", mean_dev["dev_APL_0.8"]))
  message(sprintf("dev for APL_1: %s", mean_dev["dev_APL_1"]))
  message(sprintf("auc for glinternet: %s", mean_auc["auc_glinternet"]))
  message(sprintf("auc for sprinter: %s", mean_auc["auc_sprinter"]))
  message(sprintf("auc for APL_0.3: %s", mean_auc["auc_APL_0.3"]))
  message(sprintf("auc for APL_0.5: %s", mean_auc["auc_APL_0.5"]))
  message(sprintf("auc for APL_0.8: %s", mean_auc["auc_APL_0.8"]))
  message(sprintf("auc for APL_1: %s", mean_auc["auc_APL_1"]))
}

# Save the evaluation results to disk
saveRDS(eval_list, file = "eval_list.rds")
