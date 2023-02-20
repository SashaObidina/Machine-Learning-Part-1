#task 1
library(adabag)
library(rpart)
library(mlbench)

data(Vehicle)
strings_number<-length(Vehicle[,1])
train_num<-sample(1:strings_number,0.7*strings_number)
misclass <- c()

k=1
for (i in seq(1, 301, 10))
{
  print(i)
  model <- boosting(Class ~ ., data = Vehicle[train_num,], mfinal = i)
  pr <- predict(model, Vehicle[-train_num,])
  misclass[k] <- pr$error
  k=k+1
}

print(misclass)
plot(seq(1, 301, 10), misclass, col = "steelblue", xlab="Number of trees", ylab="Error", pch = 20, cex = 1.2, type="b")

#task 2
library(adabag)
library(rpart)
library(mlbench)

data(Glass)
strings_number<-length(Glass[,1])
train_num<-sample(1:strings_number,0.7*strings_number)
misclass <- c()

k=1
for (i in seq(1, 201, 10))
{
  print(i)
  model <- bagging(Type ~ ., data = Glass[train_num,], mfinal = i)
  pr <- predict(model, Glass[-train_num,])
  misclass[k] <- pr$error
  k=k+1
}

print(misclass)
plot(seq(1, 201, 10), misclass, col = "steelblue", xlab="Number of trees", ylab="Error", pch = 20, cex = 1.2, type="b")

#task 3
AdaBoost <- function(train, Y, t, k) {
  weak_cls <- list() #list with weak classifiers
  strings_number <- nrow(train)
  weights <- rep(1/strings_number, strings_number)
  a <- c() # alphas - weights of weak classifiers
  
  for (i in 1:t) {
    labels <- train[, Y]
    Y_without_repeating <- levels(train[, Y])
    model <- list(Y = Y, train = train, levels = Y_without_repeating, weights = weights, k = k)
    weak_cls[[i]] <- model
    
    # kknn---
    predict <- kknn_pr(model, train)
    print(predict)
    print(train[,Y])
    #---
    
    e = 0 # sum of errors
    for (j in 1:strings_number) {
      if (labels[j] != predict[j]) {
        e <- e + weights[j]
      }
    }
    
    if (e != 0)
      a[[i]] <- 0.5*log((1-e)/e)
    else
      stop("Error = 0")
    
    for (l in 1:strings_number) {
      if (predict[l] != train[[Y]][l]) misCl = 1
      else misCl = -1
        weights[l] <- weights[l]*exp(a[[i]]*misCl)
    }
  }
  
  r <- list(weak_cls=weak_cls, a=a, levels = levels(train[, Y]))
  return(r)
}

AdaBoost_pred <- function(model, test_data) {
  strings_number_test <- nrow(test_data)
  pred = c()
  
  for (i in 1:strings_number_test) {
    myfreq <- data.frame(names = model$levels, freq = rep(0, length(model$levels)))
    
    for (j in 1:length(model$weak_cls)) {
      prediction <- kknn_pr(model$weak_cls[[j]], test_data[i, ])
      myfreq[myfreq$names == prediction, ][2] <- myfreq[myfreq$names == prediction, ][2] + model$a[j]
    }
    most_frequent = model$levels[myfreq$freq == max(myfreq$freq)]
    pred[i] <- sample(most_frequent, 1)
  }
  return(pred)
}

kknn_pr <- function(model, test_data) {
  strings_number_test <- nrow(test_data)
  pred <- c()
  features <- !(names(model$train) %in% model$Y)
  
  train <- model$train[, features]
  dim_dist<- ncol(train) - 1 # how many features
  
  strings_number_train <- nrow(train)
  test <- test_data[, features]
  
  for (i in 1:strings_number_test) {
    #взяли i-й пример из тестовой выборки
    dist <- c()
    for (j in 1:strings_number_train) {
      #взяли j-й пример из обучающей выборки
      dist[j] <- 0
      for (l in 1:dim_dist) {
        dist[j] <- dist[j]+(test[i,l] - train[j,l])^2
      }
    }
    order_dist <- order(dist)
    max_frq <- 0
    labels <- model$train[, model$Y]
    y <- labels[order_dist[1]]
    for (j in 1:model$k) {
      frq=model$weights[order_dist[j]]
      for (l in (j+1):model$k) {
        if (labels[order_dist[j]] == labels[order_dist[l]]) {
          frq=frq+model$weights[order_dist[l]]
        }
      }
      if (frq > max_frq) {
        max_frq = frq
        y <- labels[order_dist[j]]
      }
    }
    pred<-append(pred, y)
  }
  return(pred)
}

#----------------------------------------------------------------------------
library(mlbench)
library(adabag)
data("Glass")

strings_number <- nrow(Glass)
n_train <- as.integer(strings_number*0.7)
data_rand <- Glass[order(runif(strings_number)), ]
train_data <- data_rand[1:n_train, ]
test_data <- data_rand[n_train:strings_number, ]
model <- AdaBoost(train_data, 'Type', t = 1, k = 7)
predict <- AdaBoost_pred(model, test_data)
predict_table <- table(test_data$Type, predict)
predict_table
miscl <- 1 - sum(diag(predict_table))/sum(predict_table)
miscl

data("Vehicle")
strings_number <- nrow(Glass)
n_train <- as.integer(strings_number*0.7)
data_rand <- Vehicle[order(runif(strings_number)), ]
train_data <- data_rand[1:n_train, ]
test_data <- data_rand[n_train:strings_number, ]
model <- AdaBoost(train_data, 'Class', t = 1, k = 7)
predict <- AdaBoost_pred(model, test_data)
predict_table <- table(test_data$Class, predict)
predict_table
miscl <- 1 - sum(diag(predict_table))/sum(predict_table)
miscl

