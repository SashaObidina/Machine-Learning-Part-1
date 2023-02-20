#task 1
data<-read.table("D:\\reglab1.txt",header=TRUE)
colnames(data)=c("z","x","y")

lrx<-lm(x ~.,data=data,model=TRUE)
coef(lrx)
summary(lrx)

lry<-lm(y ~.,data=data,model=TRUE)
coef(lry)
summary(lry)

lrz<-lm(z ~.,data=data,model=TRUE)
coef(lrz)
summary(lrz)

#task 2
data<-read.table("D:\\reglab2.txt",header=TRUE)
colnames(data)=c("y","x1","x2","x3","x4")
col<-c("x1","x2","x3","x4")
n<-dim(data)[1]
rss<-double(20)
i<-1

for(d in 1:4){
  com<-combn(col, d)
  com
  num_com<-dim(com)[2]
  for (k in 1:num_com){
    c<-com[,k]
    lr1<-lm(as.formula(paste("y~",paste(c, collapse="+"))),data=data,model=TRUE)
    rss[i]=deviance(lr1)
    cat("RSS for", c, ":", rss[i], "\n")
    i<-i+1
  }
}
rss

#task 3
data<-read.table("D:\\cygage.txt",header = TRUE)
colnames(data)=c("A","D","W")
n<-dim(data)[1]
data_rand<-data[order(runif(n)), ]
n<-dim(data)[1]
delim<-as.integer(n*0.7)
train<-data_rand[1:delim, ]
test<-data_rand[(delim+1):n, ]
lr<-lm(A~D,data=train,model=TRUE)
pr<-predict(lr, test)
coef(lr)
summary(lr)
plot(lr)

RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
LR_R <- RSQUARE(test[,3], pr)
LR_R

#task 4
library(datasets)
library(MASS)
library(Metrics)
data("longley")
data=longley
data
lr<-lm(Employed ~.,data=data)
print(lr)
summary(lr)

n<-dim(data)[1]
data_rand<-data[order(runif(n)), ]
n<-dim(data)[1]
delim<-as.integer(n*0.5)
train<-data_rand[1:delim, ]
test<-data_rand[(delim+1):n, ]
num<-dim(test)[1]
num
train<-train[-5]
test<-test[-5]

lamb<-double(26)
for(i in seq(0,25,1)){
  lamb[i+1]=10^(-3+0.2*i)
}
lamb

lr<-lm.ridge(Employed ~.,data=train,lambda=lamb)
print(lr)
summary(lr)
plot(lr)
train_er<-double()
test_er<-double()
n_coef<-ncol(lr$coef)

for (d in 1:n_coef){
  cfs<-lr$coef[, d]
  all_rmse<-0
  for(d in 1:ncol(train)) {
    all_rmse<-all_rmse+rmse(train[d,6], sum(cfs*train[d, 1:5]))
  }
  all_rmse<-all_rmse/ncol(train)  
  train_er<-append(train_er, all_rmse)
}
plot(x=lamb, y=train_er)
lamb
train_er

for (d in 1:n_coef){
  cfs<-lr$coef[, d]
  all_rmse<-0
  for(d in 1:ncol(test)) {
    all_rmse<-all_rmse+rmse(test[d,6], sum(cfs*test[d, 1:5]))
  }
  all_rmse<-all_rmse/ncol(test)  
  test_er<-append(test_er, all_rmse)
}
plot(x=lamb, y=test_er)
lamb
test_er

#task 5
library(datasets)
data("EuStockMarkets")
data<-as.data.frame(EuStockMarkets)
print(data)
plot(EuStockMarkets)

DAX <- EuStockMarkets[, 1]
SMI <- EuStockMarkets[, 2]
CAC <- EuStockMarkets[, 3]
FTSE <- EuStockMarkets[, 4]
t<-time(EuStockMarkets)

DAXdata<-data.frame(time=t, DAX=DAX)
SMIdata<-data.frame(time=t, SMI=SMI)
CACdata<-data.frame(time=t, CAC=CAC)
FTSEdata<-data.frame(time=t, FTSE=FTSE)
all_data<-data.frame(time=t, all=DAX+SMI+CAC+FTSE)

lrDAX<-lm(DAX~ ., data=DAXdata)
coef(lrDAX)
summary(lrDAX)

lrSMI<-lm(SMI~ ., data=SMIdata)
coef(lrSMI)
summary(lrSMI)

lrCAC<-lm(CAC~ ., data=CACdata)
coef(lrCAC)
summary(lrCAC)

lrFTSE<-lm(FTSE~ ., data=FTSEdata)
coef(lrFTSE)
summary(lrFTSE)

lr<-lm(all~ ., data=all_data)
coef(lr)
summary(lr)

#task 6
library(datasets)
data("JohnsonJohnson")
data<-as.data.frame(JohnsonJohnson)
plot(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
     JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
     type = "l", col="green", main = "Прибыль ~ время", xlab="Год",
     ylab="Прибыль")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="darkorange")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="black")
lines(time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
      JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)],
      type = "l", col="red")
n<-length(JohnsonJohnson)
t1<-time(JohnsonJohnson)[seq(1,n,4)]
t2<-time(JohnsonJohnson)[seq(2,n,4)]
t3<-time(JohnsonJohnson)[seq(3,n,4)]
t4<-time(JohnsonJohnson)[seq(4,n,4)]
q1<-JohnsonJohnson[seq(1, n, 4)]
q2<-JohnsonJohnson[seq(2, n, 4)]
q3<-JohnsonJohnson[seq(3, n, 4)]
q4<-JohnsonJohnson[seq(4, n, 4)]

lr1<-lm(q1~., data=data.frame(time=t1, q1=q1))
lr2<-lm(q2~., data=data.frame(time=t2, q2=q2))
lr3<-lm(q3~., data=data.frame(time=t3, q3=q3))
lr4<-lm(q4~., data=data.frame(time=t4, q4=q4))
lr_all<-lm(JohnsonJohnson[seq(from = 1, to = length(JohnsonJohnson), by = 4)]+
     JohnsonJohnson[seq(from = 2, to = length(JohnsonJohnson), by = 4)]+
     JohnsonJohnson[seq(from = 3, to = length(JohnsonJohnson), by = 4)]+
     JohnsonJohnson[seq(from = 4, to = length(JohnsonJohnson), by = 4)]~
     time(JohnsonJohnson)[seq(from = 1, to = length(JohnsonJohnson), by = 4)],
   JohnsonJohnson)
lr1
lr2
lr3
lr4
lr_all

pr1<-lr1$coefficients[1]+lr1$coefficients[2]*2016
pr1
pr2<-lr2$coefficients[1]+lr2$coefficients[2]*2016
pr2
pr3<-lr3$coefficients[1]+lr3$coefficients[2]*2016
pr3
pr4<-lr4$coefficients[1]+lr4$coefficients[2]*2016
pr4
pr_all<-(1/4)*(lr_all$coefficients[1]+lr_all$coefficients[2]*2016)
pr_all

#task 7
library(datasets)
data("sunspot.year")
data<-as.data.frame(sunspot.year)

years<-time(sunspot.year)
lr_data<-data.frame(time=years, spots=sunspot.year)
lr_data
lr<-lm(spots~., lr_data)
plot(lr_data$time, lr_data$spots, col="darkgreen", type="l", main = "Количество солнечных пятен ~ время", xlab = "Год", ylab = "Количество")
coef(lr)
summary(lr)

#task 8
library(datasets)
gas <- read.csv("D://UKgas.csv", sep = ',', dec = '.', header=TRUE, stringsAsFactors = FALSE)
plot(gas)
n<-length(gas$time)
n
t1<-gas$time[seq(1,n,4)]
t2<-gas$time[seq(2,n,4)]
t3<-gas$time[seq(3,n,4)]
t4<-gas$time[seq(4,n,4)]
q1<-gas$UKgas[seq(1, n, 4)]
q2<-gas$UKgas[seq(2, n, 4)]
q3<-gas$UKgas[seq(3, n, 4)]
q4<-gas$UKgas[seq(4, n, 4)]

lr1<-lm(q1~., data=data.frame(time=t1, q1=q1))
lr2<-lm(q2~., data=data.frame(time=t2, q2=q2))
lr3<-lm(q3~., data=data.frame(time=t3, q3=q3))
lr4<-lm(q4~., data=data.frame(time=t4, q4=q4))
lr_all<-lm(gas$UKgas[seq(from = 1, to = n, by = 4)]+
             gas$UKgas[seq(from = 2, to = n, by = 4)]+
             gas$UKgas[seq(from = 3, to = n, by = 4)]+
             gas$UKgas[seq(from = 4, to = n, by = 4)]~
             gas$time[seq(from = 1, to = n, by = 4)],
           gas)
lr1
lr2
lr3
lr4
lr_all

pr1<-lr1$coefficients[1]+lr1$coefficients[2]*2016
pr1
pr2<-lr2$coefficients[1]+lr2$coefficients[2]*2016
pr2
pr3<-lr3$coefficients[1]+lr3$coefficients[2]*2016
pr3
pr4<-lr4$coefficients[1]+lr4$coefficients[2]*2016
pr4
pr_all<-(1/4)*(lr_all$coefficients[1]+lr_all$coefficients[2]*2016)
pr_all

#task 9
library(datasets)
data("cars")
data=cars
lr<-lm(dist ~ .,data=data)
coef(lr)
summary(lr)
test<-data.frame("speed"=40,"dist"="")
predict(lr,test)
