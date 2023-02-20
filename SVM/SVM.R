#task 1
library(e1071)
train<-read.table("D:\\svmdata1.txt",stringsAsFactors = TRUE)
test<-read.table("D:\\svmdata1test.txt",stringsAsFactors = TRUE)

features<-data.frame(X1=test$X1,X2=test$X2)

svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1000,kernel = "linear")

pr <- predict(svm, features)
res<-table(test$Color, pr)
res
acc<-(res[1,1]+res[2,2])/sum(res)
acc
plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")

#task 2
library(e1071)
train<-read.table("D:\\svmdata2.txt",stringsAsFactors = TRUE)
test<-read.table("D:\\svmdata2test.txt",stringsAsFactors = TRUE)

features<-data.frame(X1=test$X1,X2=test$X2)
costs<-c(1,5,10,30,50,100,300,500,1000)
for (c in costs) {
  print(c)
  svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "linear")
  pr <- predict(svm, features)
  res<-table(test$Colors, pr)
  print(res)
  acc<-(res[1,1]+res[2,2])/sum(res)
  acc
  plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")
}
plot(X1 ~ X2,train, col = c("green","pink"))
plot(X1 ~ X2,test, col = c("green","pink"))

#task 3
library(e1071)
data<-read.table("D:\\svmdata3.txt",stringsAsFactors = TRUE)
num<-nrow(data)
num
nt<-as.integer(num*0.8)
nt
rdata<-data[order(runif(num)),]
rdata
train=rdata[1:nt, ]
train
test=rdata[(nt+1):100, ]
test
features<-data.frame(X1=test$X1,X2=test$X2)

degr<-c(1,5,10,15,25,50,2)
for (d in degr) {
  print(d)
  svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "polynomial", degree=d)
  pr <- predict(svm, features)
  res<-table(test$Colors, pr)
  print(res)
  acc<-(res[1,1]+res[2,2])/sum(res)
  print(acc)
  summary(svm)
  plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")
}

svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "radial")
pr <- predict(svm, features)
res<-table(test$Colors, pr)
print(res)
acc<-(res[1,1]+res[2,2])/sum(res)
acc
summary(svm)
plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")

svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "sigmoid")
pr <- predict(svm, features)
res<-table(test$Colors, pr)
print(res)
acc<-(res[1,1]+res[2,2])/sum(res)
acc
summary(svm)
plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")

#task 4
library(e1071)
train<-read.table("D:\\svmdata4.txt",stringsAsFactors = TRUE)
test<-read.table("D:\\svmdata4test.txt",stringsAsFactors = TRUE)
features<-data.frame(X1=test$X1,X2=test$X2)

svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "polynomial")
pr <- predict(svm, features)
res<-table(test$Colors, pr)
print(res)
acc<-(res[1,1]+res[2,2])/sum(res)
acc
summary(svm)
plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")

svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "radial")
pr <- predict(svm, features)
res<-table(test$Colors, pr)
print(res)
acc<-(res[1,1]+res[2,2])/sum(res)
acc
summary(svm)
plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")

svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "sigmoid")
pr <- predict(svm, features)
res<-table(test$Colors, pr)
print(res)
acc<-(res[1,1]+res[2,2])/sum(res)
acc
summary(svm)
plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")

#task 5
library(e1071)
train<-read.table("D:\\svmdata5.txt",stringsAsFactors = TRUE)
test<-read.table("D:\\svmdata5test.txt",stringsAsFactors = TRUE)
features<-data.frame(X1=test$X1,X2=test$X2)

gamm<-c(1,50)
for (g in gamm) {
  print(g)
  svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "polynomial", gamma=g)
  pr <- predict(svm, features)
  res<-table(test$Colors, pr)
  print(res)
  acc<-(res[1,1]+res[2,2])/sum(res)
  print(acc)
  summary(svm)
  plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")
}

gamm<-c(1,50)
for (g in gamm) {
  print(g)
  svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "radial", gamma=g)
  pr <- predict(svm, features)
  res<-table(test$Colors, pr)
  print(res)
  acc<-(res[1,1]+res[2,2])/sum(res)
  print(acc)
  summary(svm)
  plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")
}

gamm<-c(1,50)
for (g in gamm) {
  print(g)
  svm <- svm(factor(Colors)~.,data=train,type = "C-classification",cost = 1,kernel = "sigmoid", gamma=g)
  pr <- predict(svm, features)
  res<-table(test$Colors, pr)
  print(res)
  acc<-(res[1,1]+res[2,2])/sum(res)
  print(acc)
  summary(svm)
  plot(svm,test,grid = 250,col=c("green","pink"), dataSymbol="+", svSymbol="V")
}

#task 6
library(Metrics)
library(e1071)
train<-read.table("D:\\svmdata6.txt",stringsAsFactors = TRUE)
features<-data.frame(X=train$X)
train
features

mse_vector<-vector()
epsilon<-c(0.1, 0.3, 0.5, 0.7, 1)
for (e in epsilon) {
  print(e)
  svm <- svm(train$X, train$Y,type = "eps-regression",cost = 1,kernel = "radial", eps = e)
  pr <- predict(svm, features)
  res<-table(train$Y, pr)
  mse<-mse(pr,train$Y)
  mse_vector<-append(mse_vector, mse)
  print(mse_vector)
  acc<-(res[1,1]+res[2,2])/sum(res)
  print(acc)
  summary(svm)
  x<-train$X
  y<-train$Y
  plot(x, y, xlab="X", ylab="Y", ylim=c(-2,2), main = paste("epsilon = ", e))
  
  points(x[svm$index], y[svm$index], col = "pink")
  lines(x, pr, col = "red", lwd = 2)
  lines(x, pr + svm$epsilon, col = "blue")
  lines(x, pr - svm$epsilon, col = "blue")
}
mse_vector
plot(y=mse_vector, x=epsilon, col="blue", lwd=3, type="l")

