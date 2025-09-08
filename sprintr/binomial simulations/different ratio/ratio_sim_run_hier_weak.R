# Binomial simulation

# Initialization and loading functions
rm(list=ls())
source("ratio_binomial_parm_generate.R") # Source the parameter generation script
source("../binomial_eval.R") # Source the evaluation script
source("../../method_run.R") # Source the method running script

# Setting the parameters for the simulation
n <- 200 # Number of samples (both training and test)
p <- 150 # Number of main effects
stru <- "hier_weak" # Structure type of the simulated data. Options are "anti", "mix", "hier_weak" and "hier_strong".
ratios <- c(0.1, 0.4, 0.7, 1, 1.3) # Different main vs. interaction effects signal strength
method <- c("sprinter", "glinternet", "MEL", "APL", "SIS", "RAMP", "hierFabs", "hierNet") # Methods to be evaluated
num_sim <- 50 # Number of simulations
family <- "binomial" # Family of the response variable

# Initialize lists to store evaluation results
eval_list <- vector("list", length(ratios))
main_coef <- c()

# Loop over each ratio setting
for (i in seq_along(ratios)){

  set.seed(154)
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
