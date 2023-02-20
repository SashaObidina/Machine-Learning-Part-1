#task 1
install.packages("e1071")
library(e1071)
first_data <- read.table("D:\\Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
first_data
strings_num<-dim(first_data)[1]
rand_data<-first_data[order(runif(strings_num)), ]

train<-c()
prediction<-c()
acc<-c()
start<-0.1
finish<-1

for_one_volume<-function(incr, idx, fl){
  pr<-0
  tr<-0
  for(i in seq(1, repeats_num)){
    if (fl == 0) {
      test_data<-rand_data[(incr+1):strings_num, ]
      train_data<-rand_data[1:incr, ]
      A_classifier<-naiveBayes(V10 ~ ., data = train_data)
      A_predicted<-predict(A_classifier, test_data)
      res <- table(A_predicted, test_data$V10)
      pr <- pr + res[1] + res[4]
    }
    else{
      test_data <- spam[idx,]
      train_data <- spam[-idx,]
      model <- naiveBayes(type ~ ., data = train_data)
      predict(model, test_data)
      res <- table(predict(model, test_data), test_data$type)
      predict(model, test_data, type = "raw")
      tr <- dim(train_data)[1]
      tr
      pr <- pr + res[1] + res[4]
      pr
    }
   
  }
  return(list(tr,pr))
}

#tic-tac-toe
repeats_num<-50
s<-start
k<-1
while (s<finish) {
  incr<-as.integer(strings_num*s)
  train[k]<-incr
  tr_pr = as.numeric(for_one_volume(incr,0,0))
  prediction[k]<-tr_pr[2]
  acc[k] <- prediction[k] / (repeats_num*(strings_num-incr))
  s<-s+0.05
  k<-k+1
}
train
acc
plot(train, acc, type = "b", xlab = "Train data size", ylab = "Качество")

#spam
library(kernlab)
library(e1071)
data(spam)
train<-c()
prediction<-c()
acc<-c()
spam[0:1,]
start<-100
finish<-1000
repeats_num<-50
k<-1
s<-start
while (s<finish) {
  idx <- sample(1:dim(spam)[1], s)
  test_data <- spam[idx,]
  tr_pr<-as.numeric(for_one_volume(0,idx,1))
  train[k]<-tr_pr[1]
  prediction[k]<-tr_pr[2]
  acc[k] <- prediction[k] / (repeats_num*dim(test_data)[1])
  s<-s+100
  k<-k+1
}
plot(train, acc, type = "b", xlab = "Train data size", ylab = "Качество")

#task 2
library(e1071)
n<-100
train_part<-0.8
m_x11=10
m_x12=20
m_x21=14
m_x22=18
s1=4
s2=3

X1<-c(rnorm(n/2,m_x11,s1),rnorm(n/2,m_x12,s2))
X2<-c(rnorm(n/2,m_x21,s1),rnorm(n/2,m_x22,s2))
type <- c(rep(-1, n/2), rep(1, n/2))
X1_X2_type <- data.frame(X1, X2, type) #объединение
X1_X2_type_rand <- X1_X2_type[order(runif(n)), ] #перемешивание

nt <- as.integer(n*train_part)
train_data <- X1_X2_type_rand[1:nt, ]
test_data <- X1_X2_type_rand[(nt+1):n, ]

A_classifier <- naiveBayes(type ~ ., data=train_data)
A_predicted <- predict(A_classifier, test_data) 
result <- table(A_predicted, test_data$type)
acc <- (result[1]+result[4])/(n-nt)

plot(X1_X2_type$X1,X1_X2_type$X2,
     col=ifelse(X1_X2_type$type==-1,"green","blue"),
     xlab="X1",ylab="X2")

plot(test_data$X1,test_data$X2,
     col=ifelse(test_data$type==-1,"green","blue"),
     xlab="X1",ylab="X2")

points(test_data$X1,test_data$X2,
       pch=5,col=ifelse(A_predicted==-1,"green","blue"),
       xlab="X1",ylab="X2")

result
acc

#task 3

A_test = read.csv(file = 'D:\\All_Labs\\test.csv', sep=',', header=TRUE, na.strings='NA', stringsAsFactors=F)
A_train = read.csv(file = 'D:\\All_Labs\\train.csv', sep=',', header=TRUE, na.strings='NA', stringsAsFactors=F)
A_train[0,]
A_train
A_classifier <- naiveBayes(A_train[,-12], as.factor(A_train$Survived))
A_predicted <- predict(A_classifier, A_test)
result <- table(A_predicted, A_test$Survived)
acc = (result[1] + result[4])/dim(A_test)[1]
result
acc
