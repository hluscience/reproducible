# Run simulations for the theory proof on step 1 misspecification error and step 2 selecting interactions TPR & FPR

# Initialization and loading functions
rm(list=ls())
source("../theory_proof_screen_eval_corr_n.R")

# Setting the parameters for the simulation
n_seq <- c(100, 200, 500, 1000, 2000, 5000)  # Sequence of different numbers of samples
p <- 150 # Number of main effects
num_sim <- 30  # Number of simulations
kappa <- 0.25 # Control signal strength

# Initialize lists to store evaluation results
eval_list <- vector("list", length(n_seq))

# Loop over each p setting
for (i in seq_along(n_seq)){

  set.seed(100)
  start.time.i <- Sys.time()
  message(sprintf("Processing n: %s", n_seq[i]))

  # Generate parameters
  parm <- theory_proof_parm_gen(stru = "mix", n = n_seq[[i]], p = p, coef_inter = 4, ratio = 0.5)
  test <- theory_proof_test_data_gen(n = 1000, p = p, compact = parm$compact)

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

    # Run sprinter and evaluate
    eval_vec <- increasing_n_method_run_eval(
      x = data$x,
      y = data$y,
      x_test = test$x,
      y_test = test$y,
      pure_inter = pure_inter
    )

    message(sprintf("Simulation %d completed for n = %d", j, n_seq[i]))
    end.time.j <- Sys.time()
    time.j <- as.numeric(difftime(end.time.j, start.time.j, units = "secs"))
    message(sprintf("Time taken for simulation %d: %s seconds", j, round(time.j, 2)))
  }

  # Save the evaluation results
  eval_list[[i]] <- eval_vec

  message(sprintf("Completed for n = %d", n_seq[i]))
  end.time.i <- Sys.time()
  time.i <- as.numeric(difftime(end.time.i, start.time.i, units = "secs"))
  message(sprintf("Time taken for n = %d: %s seconds", n_seq[i], round(time.i, 2)))
}

# Save the evaluation results to disk
saveRDS(eval_list, file = "eval_list.rds")
