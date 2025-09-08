# Run simulations for the theory proof

# Initialization and loading functions
rm(list=ls())
source("../theory_proof_screen_eval_corr_n.R")

# Setting the parameters for the simulation
n <- 100 # Number of samples
p <- 150 # Number of main effects
correlation <- c(-0.8, -0.4, 0, 0.4, 0.7, 0.8)  # Different correlation levels for main effects to be tested
num_sim <- 1  # Number of simulations
kappa <- 0.25 # Control signal strength

# Initialize lists to store evaluation results
eval_list <- vector("list", length(correlation))
ratio_mean_vec <- c()

# Loop over each p setting
for (i in seq_along(correlation)){

  set.seed(10)
  start.time.i <- Sys.time()
  message(sprintf("Processing correlation: %s", correlation[i]))

  # Generate parameters
  parm <- theory_proof_parm_gen(stru = "mix", n = n, p = p, coef_inter = 4, ratio = 0.5, corr_gen = correlation[i])

  # Obtain pure interactions from population data
  pure_inter <- pure_inter_get(parm = parm, kappa = kappa)
  saveRDS(pure_inter, paste0("pure_inter_corr_", correlation[i], ".rds"))

  # Initialize lists to store evaluation results for each simulation
  eval_vec <- list()

  # Loop over each simulation
  for(j in 1:num_sim){

    set.seed(j)

    # Generate data based on the current parameters
    data <- theory_proof_popu_data_gen(n=50000, p=p, corr_gen=correlation[i], compact=parm$compact)

    # Run sprinter and evaluate
    eval_vec <- increasing_corr_run(
      x = data$x,
      y = data$y
    )
  }

  # Save the evaluation results
  saveRDS(eval_vec, file = paste0("inter_strength_gen_corr_", correlation[i], ".rds"))
  eval_list[[i]] <- eval_vec

  message(sprintf("Completed for correlation = %s", correlation[i]))
  end.time.i <- Sys.time()
  time.i <- as.numeric(difftime(end.time.i, start.time.i, units = "secs"))
  message(sprintf("Time taken for correlation = %s: %s seconds", correlation[i], round(time.i, 2)))
}

# Save the evaluation results to disk
saveRDS(eval_list, file = "increasing_corr_eval_list.rds")
