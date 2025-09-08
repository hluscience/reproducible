# Run simulations to compare sprinter with oracle method

# Initialization and loading functions
rm(list=ls())
source("../theory_proof_method_eval.R")

# Setting the parameters for the simulation
n <- 100 # Number of samples
p_seq <- c(30, 50, 70, 100, 150)  # Sequence of different numbers of predictors
num_sim <- 30  # Number of simulations
kappa <- 0.25 # Control signal strength

# Initialize lists to store evaluation results
eval_list <- vector("list", length(p_seq))

# Loop over each p setting
for (i in seq_along(p_seq)){

  set.seed(6)
  start.time.i <- Sys.time()
  message(sprintf("Processing p: %s", p_seq[i]))

  # Generate parameters
  parm <- theory_proof_parm_gen(stru = "mix", n = n, p = p_seq[[i]], coef_inter = 4, ratio = 0.5)

  # Obtain pure interactions from population data
  pure_inter <- pure_inter_get(parm = parm, kappa = kappa)

  # Initialize lists to store evaluation results for each simulation
  eval_vec <- list()

  # Loop over each simulation
  for(j in 1:num_sim){

    set.seed(j)
    start.time.j <- Sys.time()

    # Generate data based on the current parameters
    data <- theory_proof_data_gen(x = parm$x, compact = parm$compact)

    # Run each method and evaluate
    eval_vec <- compare_with_oracle_method_run_eval(
      x = data$x,
      y = data$y,
      pure_inter_idx = pure_inter$pure_inter_idx
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
  mean_TPR <- c()

  # Calculate the mean for each method
  for (name in names(eval_vec)) {
    if (grepl("TPR", name)) {
      mean_TPR[name] <- mean(eval_vec[[name]])
    }
  }

  message(sprintf("TPR for oracle: %s", mean_TPR["TPR_oracle"]))
  message(sprintf("TPR for sprinter: %s", mean_TPR["TPR_sprinter"]))
}

# Save the evaluation results to disk
saveRDS(eval_list, file = "eval_list.rds")
