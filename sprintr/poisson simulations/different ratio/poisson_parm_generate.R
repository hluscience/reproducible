# Poisson simulation
# Function to generate parameters based on different architectures
parm_gen <- function(stru, n, p, coef_inter, ratio) {

  # Generate predictor matrix
  x <- matrix(rnorm(n * p, mean = 0, sd = 0.5), n, p)

  # Define indices for main effects
  ix_main <- cbind(rep(0, 2), 1:2)

  # Define indices for interaction effects based on structure
  if (stru == "anti") {
    ix_inter <- rbind(c(3, 4), c(5, 6), c(7, 8))
  } else if (stru == "mix") {
    ix_inter <- rbind(c(1, 3), c(4, 5), c(6, 7))
  } else if (stru == "hier_weak") {
    ix_inter <- rbind(c(1, 3), c(2, 4), c(1, 5))
  } else if (stru == "hier_strong") {
    ix_inter <- rbind(c(1, 2))
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

# Function to generate data based on the parameters
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

  # Generate Poisson response variable y
  mu <- mu_main + mu_inter
  pr = exp(mu)
  y = rpois(n=n, lambda=pr)

  out <- list(x = x,
              y = y,
              pr = pr)

  return(out)
}

# Function to generate main effects coefficients for different structures and ratio settings
ratio_gen_main <- function(stru, n, p, ratios){

  # Initialize a vector to store the main effects coefficients for each ratio
  coef_main <- numeric(length = length(ratios))

  for (i in seq(ratios)) {
    # Generate parameters based on the specified structure
    para <- parm_gen(stru = stru, n = n, p = p, coef_inter = 2, ratio = ratios[i])

    # Generate data based on the parameters
    data <- data_gen(x = para$x, compact = para$compact)

    # Extract the main effects coefficient from the generated data
    coef_main[i] <- para$coef_main
  }

  return(coef_main)
}

# Example usage:
# Set parameters
stru <- "anti"  # can be "anti", "mix", "hier_weak" or "hier_strong"
n <- 200
p <- 150
ratios <- c(0.1, 0.4, 0.7, 1, 1.3)

# Get main effects coefficients for the specified structure and ratio settings
coef_main <- ratio_gen_main(stru, n, p, ratios)
# print(coef_main)
