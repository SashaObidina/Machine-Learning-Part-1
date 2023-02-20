#task 1
library(kknn)
first_data <- read.table("D:\\Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
first_data
strings_num<-dim(first_data)[1]
rand_data<-first_data[order(runif(strings_num)), ]
rand_data

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
      train_data
      A_classifier<-kknn(V10 ~ .,
                         train = train_data,
                         test = test_data,
                         distance = 1,
                         k=round(sqrt(incr), digits = 0),
                         kernel = "triangular")
      A_predicted<-A_classifier$fitted.values
      res <- table(A_predicted, test_data$V10)
      pr <- pr + res[1] + res[4]
    }
    else{
      test_data <- spam[idx,]
      train_data <- spam[-idx,]
      model <- kknn(type ~ .,
                    train = train_data,
                    test = test_data,
                    distance = 1,
                    k=round(sqrt(incr), digits = 0),
                    kernel = "triangular")
      res <- table(model$fitted.values, test_data$type)
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
  tr_pr<-as.numeric(for_one_volume(s,idx,1))
  train[k]<-tr_pr[1]
  prediction[k]<-tr_pr[2]
  acc[k] <- prediction[k] / (repeats_num*dim(test_data)[1])
  s<-s+100
  k<-k+1
}
plot(train, acc, type = "b", xlab = "Train data size", ylab = "Качество")

#task 2
library(kknn)
library(kernlab)
data(glass)
glass<-glass[,-1]
strings_num<-dim(glass)[1]
glass<-glass[order(runif(strings_num)),]
ker = c("rectangular",
        "triangular",
        "epanechnikov",
        "biweight",
        "triweight",
        "cos",
        "inv",
        "gaussian",
        "rank",
        "optimal")

fit.knn1<-train.kknn(Type ~.,glass, kmax=round(sqrt(strings_num), digits = 0), kernel=ker, distance=1)
fit.knn1
plot(fit.knn1)

fit.knn2<-train.kknn(Type ~.,glass, kmax=round(sqrt(strings_num), digits = 0), kernel=ker, distance=2)
fit.knn2
plot(fit.knn2)

fit.knn3<-train.kknn(Type ~.,glass, kmax=round(sqrt(strings_num), digits = 0), kernel=ker, distance=3)
fit.knn3
plot(fit.knn3)

fit.knn4<-train.kknn(Type ~.,glass, kmax=round(sqrt(strings_num), digits = 0), kernel=ker, distance=7)
fit.knn4
plot(fit.knn4)

fit.knn5<-train.kknn(Type ~.,glass, kmax=round(sqrt(strings_num), digits = 0), kernel=ker, distance=10)
fit.knn5
plot(fit.knn5)

#----------------------------
frame<-data.frame("RI" = 1.516,
                    "Na" = 11.7,
                    "Mg" = 1.01,
                    "Al" = 1.19,
                    "Si" = 72.59,
                    "K" = 0.43,
                    "Ca" = 11.44,
                    "Ba" = 0.02,
                    "Fe" = 0.1
)
res <- kknn(Type ~ ., glass, frame, distance = 1, kernel = "biweight")
fitted(res)

#-----------------------------
glass<-glass[order(runif(strings_num)),]
train_data<-glass[1:170,]
test_data<-glass[(170+1):strings_num,]

acc<-c()
features1<-c("Nothing", "RI","Na","Mg","Al","Si","K","Ca","Ba","Fe")
features<-c(0,1,2,3,4,5,6,7,8,9)
for(j in 1:10)
{
  i=j-1
  i
  pr=0
  train<-train_data
  test<-test_data
  if (i > 0) {
    train<-train_data[,-i]
    test<-test_data[,-i]
  }
  
  knn<-kknn(Type ~ .,train,test,k=round(sqrt(strings_num),digits=0),distance=1,kernel="biweight")
  
  glass_fit<-fitted(knn)
  for(k in 1:length(glass_fit)){
    if(glass_fit[k]==test$Type[k])
      pr=pr+1
  }
  print(pr/dim(test)[1])
  acc[j]<-pr/dim(test)[1]
}
acc
features
plot(features, acc, type = "b", xlab = "Removed feature", ylab = "Качество")
library(ggplot2)
ggplot(data.frame(features1,acc)) +
  labs(x = "Removed feature", y = "Качество") +
  geom_point(aes(x = features1, y = acc), color = "blue", lwd = 3)

#task 3
library(kknn)
train_data<-read.table("D:\\svmdata4.txt",stringsAsFactors = TRUE)
test_data<-read.table("D:\\svmdata4test.txt",stringsAsFactors = TRUE)
strings_number<-dim(train_data)[1]
plot(train_data$X1, train_data$X2, pch=21, bg=c("red","blue") [unclass(train_data$Colors)],  main="My train data", xlab = "X1", ylab="X2")

knn<-train.kknn(Colors ~ .,train_data,kmax=round(sqrt(strings_num),digits=0),distance=2,kernel="optimal")
knn
plot(knn)

#task 4
library(kknn)
train_data<-read.csv(file="D:\\All_Labs\\train.csv")
train_data
test_data<-read.csv(file="D:\\All_Labs\\test.csv")
strings_num<-dim(train_data)[1]
knn<-train.kknn(Survived ~ ., train_data,
                           kmax=round(sqrt(strings_num),digits=0),
                           kernel=c("rectangular",
                                    "triangular",
                                    "epanechnikov",
                                    "biweight",
                                    "triweight",
                                    "cos",
                                    "inv",
                                    "gaussian",
                                    "rank",
                                    "optimal"),
                           distance=2)
knn
plot(knn)
