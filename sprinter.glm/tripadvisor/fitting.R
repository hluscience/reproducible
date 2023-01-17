# read tripadvisor data
data <- load("/Users/lucylu/tripadvisor/data.RData")
bin <- get(data[2])
rate <- get(data[4])
# remove 0 rating
drop <- which(rate == 0) 
rate <- rate[-drop]
bin <- bin[-drop,]
# split traing and testing set
x = bin
y = factor(rate)
n = nrow(bin)
set.seed(10)
train_size <- round(nrow(bin) * 0.1)
train_indx <- sample(seq_len(n), size = train_size)
x_train <- x[train_indx,]
y_train <- y[train_indx]
x_test <- x[-train_indx,]
y_test <- y[-train_indx]


# ordinal fit
cv.fit.ordinal <- sprintr.glm::cv.sprinter(x = x_train, y = y_train, family = "ordinal", 
                                           nlam1 = 10, nlam3 = 50, square = FALSE, 
                                           num_keep = NULL, cv_step1 = TRUE)
# linear fit
cv.fit.linear <- sprintr.glm::cv.sprinter(x = x_train, y = y_train, family = "gaussian", 
                                          nlam1 = 10, nlam3 = 50, square = FALSE, 
                                          num_keep = NULL, cv_step1 = TRUE)