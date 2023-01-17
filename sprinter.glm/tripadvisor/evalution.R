# predictions
# ordinal 
predict.ordinal <- predict.cv.sprinter(object = cv.fit.ordinal, newdata = x_test)
delta = t(apply(predict.ordinal, 1, invLogit)) 
probMat = delta - cbind(0, delta[,-ncol(delta)])
probMat <- cbind(probMat, 1-rowSums(probMat))
y_pred_ordinal <- max.col(probMat)
# linear
y_pred_linear <- predict.cv.sprinter(object = cv.fit.linear, newdata = x_test)

# accuracy
# ordinal
mean(y_test == y_pred_ordinal)  
# linear
y_pred_linear_round <- ifelse(y_pred_linear < 1.5, 1, 
                              ifelse(1.5 <= y_pred_linear & y_pred_linear< 2.5, 2, 
                                     ifelse(2.5 <= y_pred_linear & y_pred_linear< 3.5, 3, 
                                            ifelse(3.5 <= y_pred_linear & y_pred_linear< 4.5, 4, 5))))
mean(y_test == y_pred_linear_round)

# plot the distribution of rating 
# create a Matrix which will help in creating the plot
df = data.frame(rating = c(1,2,3,4,5), 
                test = c(table(y_test)), 
                linear = c(table(y_pred_linear_round)),
                ordinal = c(table(y_pred_ordinal)))
value_matrix = matrix(, nrow = 3, ncol = 5)
value_matrix[1,] = df$test 
value_matrix[2,] = df$ordinal
value_matrix[3,] = df$linear

barplot(value_matrix, names.arg = df$rating, beside = TRUE, col = c("gray89", "#dfe2fb","#bdc5fa"), xlab="Rating", ylab = "Frequency",cex.axis = 1.3, cex.lab=1.3)
legend("topleft",
       legend = c("test", "ordinal", "linear"),
       bty = "n",cex=1.3,fill = c("gray89", "#dfe2fb","#bdc5fa"))