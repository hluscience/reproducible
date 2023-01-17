n <- 200
p <- 150
stru <- "anti"
num_ratio <- 6
num_sim <- 80
method <- c("MEL","APL", "SIS", "sprinter") 
family <- "poisson"

parm_list <- vector("list", num_ratio)
train_indx_list <- vector("list", num_ratio)
eval_list <- vector("list", num_ratio)
for (i in 5:num_ratio){
  
  start.time.i <- Sys.time()
  # parm_mix, parm_hier, parm_anti_hier
  parm_list[[i]] <- parm_gen_coef_change(stru = stru, n = n, p = p, ratio_gen = i)
  data_list <- vector("list", num_sim)
  train_indx_list[[i]] <- vector("list", num_sim)
  result_list <- vector("list", num_sim)
  eval_vec <- list()
  for(j in 1:num_sim){
    start.time.j <- Sys.time()
    
    data <- data_gen(n = parm_list[[i]]$n, p = parm_list[[i]]$p, compact = parm_list[[i]]$compact)
    k <- 0
    while(sum(data$pr > 30) > 0){
      data <- data_gen(n = parm_list[[i]]$n, p = parm_list[[i]]$p, compact = parm_list[[i]]$compact)
      k <- k + 1
      print(k)
    }
    
    data_list[[j]] <- data
    x <- data$x
    y <- data$y
    n <- data$n
    mu <- data$mu
    pr <- data$pr
    
    # split to train set and test set
    train_size <- floor(0.5 * n)
    train_indx <- sample(seq_len(n), size = train_size)
    train_indx_list[[i]][[j]] <- train_indx
    x_train <- x[train_indx,]
    y_train <- y[train_indx]
    x_test <- x[-train_indx,]
    y_test <- y[-train_indx]
    mu_test <- mu[-train_indx]
    pr_test <- pr[-train_indx]
    
    result <- method_run(x = x_train, y = y_train, ix = parm_list[[i]]$compact[,c(1,2)], method = method, family = family)
    result_list[[j]] <- result
    eval_vec <- eval.vec(result, x_test, y_test, mu_test, pr_test, method, ix_int = data.frame(parm_list[[i]]$compact[parm_list[[i]]$compact[,1] != 0, c(1,2)]))
    
    print(j)
    end.time.j <- Sys.time()
    time <- end.time.j - start.time.j
    print(time)
  }
  
  eval_list[[i]] <- eval_vec
  saveRDS(data_list, file = paste(stru,i,"data list"))
  saveRDS(result_list, file = paste(stru,i,"result list"))
  saveRDS(eval_vec, file = paste(stru,i,"eval list"))
  
  print(i)
  end.time.i <- Sys.time()
  time <- end.time.i - start.time.i
  print(time)
}

saveRDS(parm_list, file = paste(stru,"parm list"))
saveRDS(train_indx_list, file = paste(stru,"train_indx list"))
saveRDS(eval_list, file = paste(stru,"eval list"))


