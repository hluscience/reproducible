cv.fit.ordinet <- readRDS(file="cv.fit.ordinet.10per.rds")
cv.fit.linear <- readRDS(file="cv.fit.linear.10per.rds")
cv.fit.hierlasso <- readRDS(file="hierlasso.10per.rds")
x_test <- readRDS(file="x_test10per")
y_test <- readRDS(file="y_test10per")

calc_pr <- function(x) 1 / (1+exp(-x))

# ordinal
predict.ordinet.mu <- sprintr::predict.cv.sprinter(object = cv.fit.ordinet, newdata = x_test)
delta = t(apply(predict.ordinet.mu,1,calc_pr))
probMat = delta - cbind(0, delta[,-ncol(delta)])
probMat <- cbind(probMat, 1-rowSums(probMat))
y_pred_ordinet <- max.col(probMat)

# linear
y_pred_linear <- sprintr::predict.cv.sprinter(object = cv.fit.linear, newdata = x_test)

# hier lasso
predict.hierlasso.mu <- predict_multinomial(object = cv.fit.hierlasso, newdata = x_test)
exp_mu <- exp(predict.hierlasso.mu)
probMat <- exp_mu / rowSums(exp_mu)
y_pred_hierlasso <- max.col(probMat)


# measure 1: loss function
# ordinal
sqrt(mean((y_test - y_pred_ordinet)^2))
# linear
sqrt(mean((y_test - y_pred_linear)^2))
# hierlasso
sqrt(mean((y_test - y_pred_hierlasso)^2))

# measure 2: round
# 2.1 proportion of accuracy observations
# ordinet
mean(y_test == y_pred_ordinet)

# linear
y_pred_linear_round <- ifelse(y_pred_linear < 1.5, 1,
                              ifelse(1.5 <= y_pred_linear & y_pred_linear< 2.5, 2,
                                     ifelse(2.5 <= y_pred_linear & y_pred_linear< 3.5, 3,
                                            ifelse(3.5 <= y_pred_linear & y_pred_linear< 4.5, 4, 5))))
mean(y_test == y_pred_linear_round)

# hierlasso
mean(y_test == y_pred_hierlasso)

# distribution plot
# Load necessary libraries
library(ggplot2)
library(reshape2)

df <- data.frame(
  rating = c(1, 2, 3, 4, 5),
  test = c(table(y_test)),
  linear = c(table(y_pred_linear_round)),
  hierlasso = c(table(y_pred_hierlasso)),
  ordinal = c(table(y_pred_ordinet))
)

# Create a Matrix which will help in creating the plot
value_matrix <- matrix(, nrow = 4, ncol = 5)
# An empty matrix is a necessary requirement prior to copying data
value_matrix[1,] = df$test
value_matrix[2,] = df$ordinal
value_matrix[3,] = df$hierlasso
value_matrix[4,] = df$linear

# Note that the "beside" argument has to be kept "TRUE" in order to place the bars side by side
png(file="rating_distribution.png")
barplot(value_matrix, names.arg = df$rating, beside = TRUE, col = c("gray89", "#dfe2fb", "#bdc5fa", "#a3b5ff"), xlab = "Rating", ylab = "Frequency", cex.axis = 1.3, cex.lab = 1.3)
legend("topleft",
       legend = c("test", "ordinal", "hierlasso", "linear"),
       bty = "n", cex = 1.3, fill = c("gray89", "#dfe2fb", "#bdc5fa", "#a3b5ff"))
dev.off()

# Create vectors for correctly predicted labels
correct_linear <- y_test == y_pred_linear_round
correct_ordinal <- y_test == y_pred_ordinet
correct_hierlasso <- y_test == y_pred_hierlasso

# Create a table of correct predictions
df <- data.frame(
  rating = c(1, 2, 3, 4, 5),
  test = c(table(y_test)),
  linear_correct = c(table(factor(y_test[correct_linear], levels = 1:5))),
  ordinal_correct = c(table(factor(y_test[correct_ordinal], levels = 1:5))),
  hierlasso_correct = c(table(factor(y_test[correct_hierlasso], levels = 1:5)))
)

# Create a Matrix which will help in creating the plot
value_matrix <- matrix(, nrow = 4, ncol = 5)
# An empty matrix is a necessary requirement prior to copying data
value_matrix[1,] = df$test
value_matrix[2,] = df$ordinal_correct
value_matrix[3,] = df$hierlasso_correct
value_matrix[4,] = df$linear_correct

# Note that the "beside" argument has to be kept "TRUE" in order to place the bars side by side
png(file="rating_distribution_correct.png")
barplot(value_matrix, names.arg = df$rating, beside = TRUE, col = c("gray89", "#dfe2fb", "#bdc5fa", "#a3b5ff"), xlab = "Rating", ylab = "Frequency", cex.axis = 1.3, cex.lab = 1.3)
legend("topleft",
       legend = c("test", "ordinal_correct", "hierlasso_correct", "linear_correct"),
       bty = "n", cex = 1.3, fill = c("gray89", "#dfe2fb", "#bdc5fa", "#a3b5ff"))
dev.off()
