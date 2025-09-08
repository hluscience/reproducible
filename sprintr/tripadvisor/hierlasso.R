if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

hier_lasso_multinomial <- function(x, y, type.multinomial = "grouped", lam_choice = "min"){

  n <- nrow(x)
  p <- ncol(x)
  stopifnot(n == length(y))

  # Scale the data
  x <- sprintr::myscale(x)
  col_mean <- attr(x = x, which = "scaled:center")
  col_sd <- attr(x = x, which = "scaled:scale")

  # Fit the multinomial model
  fit <- cv.glmnet(x = x, y = y, standardize = FALSE, family = "multinomial", type.multinomial = type.multinomial)
  saveRDS(fit, file="hierlasso.step1.10per.rds")

  # Choose lambda based on criterion (either "min" or "1se")
  if (lam_choice == "min") {
    lambda_index <- which.min(fit$cvm)
  } else if (lam_choice == "1se") {
    lambda_index <- which(fit$lambda == fit$lambda.1se)
  } else {
    stop("Invalid lam_choice. Use 'min' or '1se'.")
  }

  # Extract the intercept and best coefficients for each response category
  beta <- lapply(fit$glmnet.fit$beta, function(beta_matrix) beta_matrix[, lambda_index])
  a0 <- fit$glmnet.fit$a0[, lambda_index]

  # Sum absolute coefficients across all categories for each predictor
  aggregated_importance <- rowSums(sapply(beta, function(beta_matrix) abs(beta_matrix)))

  # Get indices of top predictors based on aggregated importance
  main_idx <- head(order(aggregated_importance, decreasing = TRUE), 65)

  # Filter out zero importance predictors from the top predictors
  main_idx <- main_idx[aggregated_importance[main_idx] != 0]

  cat("Fitting the multinomial model with main effects is done.\n")

  if(length(main_idx) != 0){
    int_idx <- t(combn(main_idx, 2))
    # cat(paste("number of constructed interactions = ", nrow(int_idx)), fill = TRUE)
    # construct design matrix of pairwise interactions
    xx <- sprintr::myscale(x[, int_idx[, 1]] * x[, int_idx[, 2]])
    col_mean <- c(col_mean[main_idx], attr(x = xx, which = "scaled:center"))
    col_sd <- c(col_sd[main_idx], attr(x = xx, which = "scaled:scale"))

    # step 2: fit cv.glmnet with all main effects and constructed (hierarchical) interactions
    # note that we use penalty.factor to make sure that main effects are not penalized
    fit <- cv.glmnet(x = cbind(x[, main_idx], xx), y = y, penalty.factor = c(rep(0, length(main_idx)), rep(1, nrow(int_idx))), standardize = FALSE, family = "multinomial", type.multinomial = type.multinomial)
    saveRDS(fit, file="hierlasso.step2.10per.rds")

    # Choose lambda based on criterion (either "min" or "1se")
    if (lam_choice == "min") {
      lambda_index <- which.min(fit$cvm)
    } else if (lam_choice == "1se") {
      lambda_index <- which(fit$lambda == fit$lambda.1se)
    } else {
      stop("Invalid lam_choice. Use 'min' or '1se'.")
    }

    # Extract the intercept and best coefficients for each response category
    beta <- lapply(fit$glmnet.fit$beta, function(beta_matrix) beta_matrix[, lambda_index])
    a0 <- fit$glmnet.fit$a0[, lambda_index]

    # Scale back the coefficients and intercepts
    beta <- lapply(beta, function(beta_matrix) beta_matrix / col_sd)
    a0 <- sapply(1:length(a0), function(i) {
      a0[i] - crossprod(col_mean, beta[[i]])
    })

    # Create the index matrix
    idx <- rbind(cbind(rep(0, length(main_idx)), main_idx), int_idx)

    # Find predictors with non-zero coefficients in any response category
    non_zero_indices <- unique(unlist(lapply(beta, function(beta) which(beta != 0))))

    # Filter the index matrix to keep only non-zero coefficients
    idx_non_zero <- idx[non_zero_indices, , drop = FALSE]

    # Create a compact data frame
    compact <- data.frame(index_1 = idx_non_zero[, 1], index_2 = idx_non_zero[, 2])

    # Add coefficients for each response category to the compact data frame
    for (i in seq_along(beta)) {
      compact[paste0("coefficient_", i)] <- beta[[i]][non_zero_indices]
    }

    cat("Fitting the multinomial model with all main effects and constructed (hierarchical) interactions is done.\n")

  }else{
    # Scale back the coefficients and intercepts
    beta <- lapply(beta, function(beta_matrix) beta_matrix / col_sd)
    a0 <- sapply(1:length(a0), function(i) {
      a0[i] - crossprod(col_mean, beta[[i]])
    })

    compact <- NULL
  }

  # finally return the best lambda
  out <- list(n = n,
              p = p,
              fit = fit,
              a0 = as.numeric(a0),
              compact = compact,
              call = match.call())

  return(out)
}

predict_multinomial <- function(object, newdata){

  # input check
  stopifnot(ncol(newdata) == object$p)

  idx <- object$compact[, 1:2, drop = FALSE]
  # selected indices for main effects
  idxm <- idx[idx[, 1] == 0, 2]
  # selected index pairs for interactions
  idxi <- idx[idx[, 1] != 0, , drop = FALSE]

  # need to standardize the main effects to construct interactions
  xm <- myscale(newdata)

  # Construct interaction terms if any
  if(nrow(idxi) == 1) {
    xint <- matrix(xm[, idxi[, 1]] * xm[, idxi[, 2]], ncol = 1)
  } else {
    xint <- xm[, idxi[, 1]] * xm[, idxi[, 2]]
  }

  # Combine main effects and interaction terms
  design <- cbind(newdata[, idxm], xint)

  # Compute linear predictors for each category
  mu <- matrix(0, nrow = nrow(design), ncol = length(object$a0))
  for (k in seq_along(object$a0)) {
    mu[, k] <- object$a0[k] + as.vector(design %*% object$compact[, paste0("coefficient_", k)])
  }

  return(mu)
}

# multinomial
data <- load("data.RData")
bin <- get(data[2])
rate <- get(data[4])
drop <- which(rate == 0)
rate <- rate[-drop]
bin <- bin[-drop,]
x = bin
y = rate
n = nrow(bin)
set.seed(10)
train_size <- round(nrow(bin)*0.1)
train_indx <- sample(seq_len(n), size = train_size)
x_train <- x[train_indx,]
y_train <- y[train_indx]
x_test <- x[-train_indx,]
y_test <- y[-train_indx]
saveRDS(x_test, file="x_test10per")
saveRDS(y_test, file="y_test10per")
cat("Reading data is done.\n")

start.linear <- Sys.time()
hier.lasso <- hier_lasso_multinomial(x = x_train, y = y_train)
end.linear <- Sys.time()
print(end.linear-start.linear)
saveRDS(hier.lasso, file="hierlasso.10per.rds")

