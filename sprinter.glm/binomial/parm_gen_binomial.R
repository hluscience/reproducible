# different architectures
parm_gen_main <- function(stru, n, p, ratio_gen){
  if(stru == "anti"){
    para <- parm_anti_main(n = n, p = p, ratio_gen = ratio_gen)
  }
  if(stru == "hier"){
    para <- parm_hier_main(n = n, p = p, ratio_gen = ratio_gen)
  }
  if(stru == "mix"){
    para <- parm_mix_main(n = n, p = p, ratio_gen = ratio_gen)
  }
  
  return(para)
}

# anti-hier
parm_anti_main <- function(n, p,  num_main = 3, num_sq = 0, num_inter = 5, ratio_gen){
  
  # main effects ix
  ix_main <- cbind(rep(0, num_main), 1:3)
  
  # square effects ix
  ix_sq <- NULL
  
  # interaction effects ix
  ix_inter <- rbind(c(4,5), c(6,7), c(8,9), c(10,11), c(12,13))
  
  ix <- rbind(ix_main, ix_sq, ix_inter)
  coef.pool <- rbind(c(1, 4), c(1.5, 4), c(2, 4), c(2.5,4), c(3,4))
  coef <- c(rep(coef.pool[ratio_gen,][1], length.out = num_main), rep(coef.pool[ratio_gen,][2], length.out = (num_sq + num_inter)))
  
  compact <- cbind(ix, coef)
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  
  out <- list(n = n,
              p = p,
              compact = compact,
              num_main = num_main,
              num_sq = num_sq,
              num_inter = num_inter)
  return(out)
}

# mix
parm_mix_main <- function(n, p, num_main = 3, num_sq = 0, num_inter = 5, ratio_gen){
  
  # main effects ix
  ix_main <- cbind(rep(0,num_main), 1:3)
  
  # square effects ix
  ix_sq <- NULL
  
  # interaction effects ix
  ix_inter <- rbind(c(1,4), c(2,5), c(6,7), c(8,9), c(10,11))
  
  ix <- rbind(ix_main, ix_sq, ix_inter)
  
  coef.pool <- rbind(c(1, 4), c(2, 4), c(3, 4), c(4,4), c(5,4))
  coef <- c(rep(coef.pool[ratio_gen,][1], length.out = num_main), rep(coef.pool[ratio_gen,][2], length.out = (num_sq + num_inter)))
  
  compact <- cbind(ix, coef)
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  
  out <- list(n = n,
              p = p,
              compact = compact,
              num_main = num_main,
              num_sq = num_sq,
              num_inter = num_inter)
  return(out)
}

# hier
parm_hier_main <- function(n, p,  num_main = 3, num_sq = 0, num_inter = 5, ratio_gen){
  
  # main effects ix
  ix_main <- cbind(rep(0, num_main), 1:3)
  
  # square effects ix
  ix_sq <- NULL
  
  # interaction effects ix
  ix_inter <- rbind(c(1,3), c(1,4), c(2,5), c(3,6), c(1,7))
  
  ix <- rbind(ix_main, ix_sq, ix_inter)
  
  coef.pool <- rbind(c(1, 4), c(1.5, 4), c(2, 4), c(2.5,4), c(3,4))
  coef <- c(rep(coef.pool[ratio_gen,][1], length.out = num_main), rep(coef.pool[ratio_gen,][2], length.out = (num_sq + num_inter)))
  
  compact <- cbind(ix, coef)
  colnames(compact) <- c("index_1", "index_2", "coefficient")
  
  out <- list(n = n,
              p = p,
              compact = compact,
              num_main = num_main,
              num_sq = num_sq,
              num_inter = num_inter)
  return(out)
}

# data generation
data_gen <- function(n, p, compact){
  
  ix <- compact[,c(1,2)]
  coef <- compact[,3]
  
  x <- matrix(rnorm(n * p), n, p)
  num_main <- sum(ix[,1]==0)
  num_sq <- sum(ix[,1]==ix[,2])
  num_inter <- sum(ix[,1]!=0 & ix[,1]!=ix[,2])
  
  if(num_main==0){mu_m <- 0}else{
    mu_m <- 0
    for (i in 1:num_main){
      mu_m <- mu_m +coef[i]*x[, ix[i,2]]
    }}
  
  if(num_sq==0){mu_sq <- 0}else{
    mu_sq <- 0
    for (j in 1:num_sq){
      mu_sq <- mu_sq + coef[j+num_main]*x[, ix[j+num_main,1]]* x[, ix[j+num_main,2]] 
    }}
  
  if(num_inter==0){mu_i <- 0}else{
    mu_i <- 0
    for (k in 1:num_inter){
      mu_i <- mu_i + coef[k+num_main+num_sq]*x[, ix[k+num_main+num_sq,1]]* x[, ix[k+num_main+num_sq,2]] 
    }}
  
  
  if(num_main==0 | num_inter==0){
    ratio <- "NaN"
  } else {
    ratio <- sqrt(sum(mu_m^2)/(sum(mu_i^2)+sum(mu_sq^2)))
  }
  
  mu <- mu_m + mu_sq + mu_i
  pr = 1/(1+exp(-mu))         # pass through an inv-logit function plogis(z)
  y = rbinom(n,1,pr)         # Bernoulli response variable
  
  out <- list(n = n,
              p = p,
              x = x,
              y = y,
              ratio = ratio)
  
  return(out)
}
