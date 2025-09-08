
# Function to perform strong hierarchical ordinal regression
strong_hier_ordinal <- function(x, y, beta, nlam = 10, lam_min_ratio = ifelse(nrow(x) < ncol(x), 0.01, 1e-04)){

  # Basic input checks
  n <- nrow(x)
  p <- ncol(x)
  stopifnot(n == length(y))
  if (!is.factor(y)) stop("y should be a factor in ordinal family")
  nlev <- nlevels(y)

  # Standardize the design matrix
  x <- myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")

  # Absolute values of coefficients to be main effects importance
  main_effects_importance <- abs(beta)

  # Get indices of top predictors based on importance
  main_idx <- head(order(main_effects_importance, decreasing = TRUE), 65)

  # Filter out zero importance predictors from the top predictors
  main_idx <- main_idx[main_effects_importance[main_idx] != 0]

  cat("Extracting the ordinal model with main effects is done.\n")

  # Generate interaction indices
  int_idx <- t(combn(main_idx, 2))
  xx <- sprintr::myscale(x[, int_idx[, 1]] * x[, int_idx[, 2]])
  col_mean <- c(col_mean[main_idx], attr(x = xx, which = "scaled:center"))
  col_sd <- c(col_sd[main_idx], attr(x = xx, which = "scaled:scale"))
  design = cbind(x[, main_idx], xx)

  # Initialize lambda values for penalized regression
  lambda2 <- get_lambda(x = design, y = y, family = "ordinal", intercept = TRUE, nlam = nlam, lam_min_ratio = lam_min_ratio)

  # Fit the ordinal model with all main effects and constructed (hierarchical) interactions
  # note that we use penalty.factor to make sure that main effects are not penalized
  start <- Sys.time()
  fit <- cv.ordinet(design, y, lambdaVals = lambda2, intercept = TRUE, standardize=FALSE, penaltyFactors = c(rep(0, length(main_idx)), rep(1, nrow(int_idx))))
  end <- Sys.time()

  # Extract the coefficients
  theta <- matrix(fit$fit$coefs[fit$bestLambdaIndex, ], ncol = 1)
  colnames(theta) <- NULL
  rownames(theta) <- NULL

  # Rescale coefficients to original scale
  intercepts0 <- matrix(theta[1:(nlev-1)], ncol = 1)
  nonintercepts0 <- matrix(theta[-(1:(nlev-1))], ncol = 1)
  unscaleFact <- col_mean / col_sd
  intAdjust <- matrix((unscaleFact %*% nonintercepts0)[rep(1, nlev-1)], ncol = 1)
  intercepts <- intercepts0 - intAdjust
  rownames(intercepts) <- paste0("Intercept:", 1:(nlev-1))
  nonintercepts <- nonintercepts0 / col_sd

  # Save the results
  saveRDS(fit, file="hierordinal.step2.fit.10per.rds")
  saveRDS(nonintercepts, file="hierordinal.step2.nonintercepts.10per.rds")
  saveRDS(intercepts, file="hierordinal.step2.intercepts.10per.rds")
  print(end - start)

  # Create the index matrix for coefficients
  idx <- rbind(cbind(rep(0, length(main_idx)), main_idx), int_idx)

  # Create a compact data frame with non-zero coefficients
  compact <- cbind(idx[nonintercepts != 0, , drop = FALSE], nonintercepts[nonintercepts != 0])
  colnames(compact) <- c("index_1", "index_2", "coefficient")

  cat("Fitting the ordinal model with all main effects and constructed (hierarchical) interactions is done.\n")

  # finally return the best lambda
  out <- list(n = n,
              p = p,
              fit = fit,
              a0 = as.numeric(a0),
              compact = compact,
              call = match.call())

  return(out)
}


# Load data
data <- load("data.RData")
bin <- get(data[2])
rate <- get(data[4])
# Remove rows with zero rate
drop <- which(rate == 0)
rate <- rate[-drop]
bin <- bin[-drop, , drop = FALSE]
x = bin
y = rate
n = nrow(bin)

# Set a random seed for reproducibility
set.seed(10)
# Split the data
train_size <- round(nrow(bin)*0.001)
train_indx <- sample(seq_len(n), size = train_size)
x_train <- x[train_indx,]
y_train <- y[train_indx]
x_test <- x[-train_indx,]
y_test <- y[-train_indx]
# saveRDS(x_test, file="x_test10per")
# saveRDS(y_test, file="y_test10per")
cat("Reading data is done.\n")

# Load the pre-fitted ordinal regression model
cv.fit.ordinet <- readRDS(file="cv.fit.ordinet.10per")
# Extract the coefficients from the first step of the model
beta <- cv.fit.ordinet$fit$step1$beta

# Fit the strong hierarchical ordinal model on the training set
hier.ordinal <- strong_hier_ordinal(x = x_train, y = factor(y_train), beta = beta)
saveRDS(hier.ordinal, file = "hier.ordinal.10per.rds")

