# binomial simulation

n <- 1000
p <- 150
stru <- "hier"
num_ratio <- 5
num_sim <- 20
method <- c("sprinter","MEL","APL","RAMP","glinternet","hierNet")
family <- "binomial"

parm_list <- vector("list", num_ratio)
train_indx_list <- vector("list", num_ratio)
eval_list <- vector("list", num_ratio)
for (i in 1:num_ratio){
  
  start.time.i <- Sys.time()
  parm_list[[i]] <- parm_gen_main(stru = stru, n = n, p = p, ratio_gen = i)
  data_list <- vector("list", num_sim)
  train_indx_list[[i]] <- vector("list", num_sim)
  result_list <- vector("list", num_sim) 
  eval_vec <- list()
  for(j in 1:num_sim){
    start.time.j <- Sys.time()
    
    data <- data_gen(n = parm_list[[i]]$n, p = parm_list[[i]]$p, compact = parm_list[[i]]$compact)
    data_list[[j]] <- data
    x <- data$x
    y <- data$y
    n <- data$n
    
    # split to train set and test set
    train_size <- floor(0.1 * n)
    train_indx <- sample(seq_len(n), size = train_size)
    train_indx_list[[i]][[j]] <- train_indx
    x_train <- x[train_indx,]
    y_train <- y[train_indx]
    x_test <- x[-train_indx,]
    y_test <- y[-train_indx]
    
    result <- method_run(x = x_train, y = y_train, ix = parm_list[[i]]$compact[,c(1,2)], method = method, family = family)
    result_list[[j]] <- result
    eval_vec <- eval.vec(result, x_test, y_test, method, ix_int = data.frame(parm_list[[i]]$compact[parm_list[[i]]$compact[,1] != 0, c(1,2)]))
    
    print(j)
    end.time.j <- Sys.time()
    time <- end.time.j - start.time.j
    print(time)
  }
  
  eval_list[[i]] <- eval_vec
  saveRDS(data_list, file = paste(stru,i,"data list"))
  saveRDS(result_list, file = paste(stru,i,"result list"))
  saveRDS(eval_vec, file = paste(stru,i,"eval list"))
  saveRDS(train_indx_list[[i]], file = paste(stru,i,"train_indx list"))
  
  print(i)
  end.time.i <- Sys.time()
  time <- end.time.i - start.time.i
  print(time)
}

saveRDS(parm_list, file = paste(stru,"parm list"))
saveRDS(train_indx_list, file = paste(stru,"train_indx list"))
saveRDS(eval_list, file = paste(stru,"eval list"))


# measure deviance on the test dataset with 100 observations
n <- 1000
p <- 150
stru <- "hier"
num_ratio <- 5
num_sim <- 20

train_indx_list <- readRDS(paste(stru, "train_indx list"))
parm_list <- readRDS(paste(stru, "parm list"))
eval_list <- vector("list", num_ratio)
for (i in 1:num_ratio){
  data_list <- readRDS(paste(stru, i, "data list"))
  result_list <- readRDS(paste(stru,i,"result list"))
  eval_vec <- list()
  for(j in 1:num_sim){
    data <- data_list[[j]]
    x <- data$x
    y <- data$y
    train_indx <- train_indx_list[[i]][[j]]
    # random select test set with size 100 
    test_indx <- sample(seq_len(n)[-train_indx], size = 100)
    x_test <- x[test_indx,]
    y_test <- y[test_indx]
    result <- result_list[[j]]
    
    eval_vec <- eval.vec(result, x_test, y_test, method, ix_int = data.frame(parm_list[[i]]$compact[parm_list[[i]]$compact[,1] != 0, c(1,2)]))
  }
  eval_list[[i]] <- eval_vec
}

saveRDS(eval_list, file = paste(stru, "dev eval list"))