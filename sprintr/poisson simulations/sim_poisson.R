# Poisson simulation

# Loading the functions for parameter generation and evaluation.
source("parm_gen_poisson.R")
source("eval_poisson.R")
source("../method_run.R")

# Setting the parameters for the simulation
n <- 200
p <- 150
stru <- "anti"
num_ratio <- 6
num_sim <- 50
method <- c("sprinter", "MEL", "APL", "SIS")
family <- "poisson"
set.seed(50)

# Initialize lists to store parameters, training indexes, and evaluation results
parm_list <- vector("list", num_ratio)
train_indx_list <- vector("list", num_ratio)
eval_list <- vector("list", num_ratio)

# Loop over each ratio setting
for (i in 1:num_ratio){
  
  start.time.i <- Sys.time()
  # Generate parameters based on the current ratio
  parm_list[[i]] <- parm_gen_main(stru = stru, n = n, p = p, ratio_gen = i)
  
  # Initialize lists to store data, training indexes, and results for each simulation
  data_list <- vector("list", num_sim)
  train_indx_list[[i]] <- vector("list", num_sim)
  result_list <- vector("list", num_sim) 
  eval_vec <- list()
  
  # Loop over each simulation
  for(j in 1:num_sim){
    start.time.j <- Sys.time()
    
    # Generate data based on the current parameters
    data <- data_gen(n = parm_list[[i]]$n, p = parm_list[[i]]$p, compact = parm_list[[i]]$compact)
    
    # Control the explosion of poisson response variable
    k <- 1
    while(sum(data$pr > 30) > 0 && k < 10000){
      data <- data_gen(n = parm_list[[i]]$n, p = parm_list[[i]]$p, compact = parm_list[[i]]$compact)
      k <- k + 1
    }
    if(k >= 10000) {
      stop(paste(
        "Execution stopped because the 'pr' rate parameter for the exponential distribution is too large.",
        "This could potentially cause an explosion of Poisson response variables.",
        "Please consider decreasing the standard deviation when generating the normal random matrix for predictors in the parm_gen_poisson.R file."
      ))
    } 
    
    data_list[[j]] <- data
    x <- data$x
    y <- data$y
    n <- data$n
    
    # Split data into training and testing sets
    train_size <- floor(0.5 * n)
    train_indx <- sample(seq_len(n), size = train_size)
    train_indx_list[[i]][[j]] <- train_indx
    x_train <- x[train_indx,]
    y_train <- y[train_indx]
    x_test <- x[-train_indx,]
    y_test <- y[-train_indx]
    
    # Run each method on the training data
    result <- method_run(x = x_train, y = y_train, ix = parm_list[[i]]$compact[,c(1,2)], method = method, family = family)
    result_list[[j]] <- result
    # Evaluate each method based on the testing data
    eval_vec <- eval.vec(result, x_test, y_test, method, ix_int = data.frame(parm_list[[i]]$compact[parm_list[[i]]$compact[,1] != 0, c(1,2)]))
    
    print(j)
    end.time.j <- Sys.time()
    time <- end.time.j - start.time.j
    print(time)
  }
  
  # Save the data, results, evaluations to disk
  eval_list[[i]] <- eval_vec
  saveRDS(data_list, file = paste(stru, i, "data list"))
  saveRDS(result_list, file = paste(stru, i, "result list"))
  saveRDS(eval_vec, file = paste(stru, i, "eval list"))
  
  print(i)
  end.time.i <- Sys.time()
  time <- end.time.i - start.time.i
  print(time)
}

# Save the overall parameters, training indexes, and evaluation results to disk
saveRDS(parm_list, file = paste(stru, "parm list"))
saveRDS(train_indx_list, file = paste(stru, "train_indx list"))
saveRDS(eval_list, file = paste(stru, "eval list"))
