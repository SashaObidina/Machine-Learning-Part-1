#task 1, 5
library(mlbench)
library(kknn)
library(maptree)
library(tree)

data(Glass)
glass <- Glass[, -1] #убрать признак Id
m <- dim(glass)[1]
glass.tr <- tree(Type ~., Glass)
print(summary(glass.tr))
draw.tree(glass.tr, cex = 0.7)
glass.tr

glass.tr1 <- snip.tree(glass.tr, nodes = c(8,9,20,21,26,27,52,53,54,55,14,15,108,63))
draw.tree(glass.tr1)

glass.tr2 <- prune.tree(glass.tr,best=8) 
draw.tree(glass.tr2)

str <- data.frame("RI" = 1.516, "Na" = 11.7, "Mg" = 1.01, "Al" = 1.19, "Si" = 72.59, 
                      "K" = 0.43, "Ca" = 11.44, "Ba" = 0.02, "Fe" = 0.1, "Type" = "" )

predict(gl.tr2, str)

#task 2
library(maptree)
library(tree)
install.packages("DAAG")
install.packages("jpeg")
library(DAAG)

data(spam7)

spam.tr<-tree(yesno ~., spam7) 
draw.tree(spam.tr) 
print(summary(spam.tr))

spam.tr1<-prune.tree(spam.tr, method = "misclass", best = 5) 
draw.tree(spam.tr1)
print(summary(spam.tr1))

#task 3
library(maptree)
library(tree)
library(DAAG)
library(e1071)

data(nsw74psid1)
num<-dim(nsw74psid1)[1]
ns_r<-nsw74psid1[order(runif(num)),]
ns_r

num_train <- as.integer(num*0.7)
train <- ns_r[1:num_train, ]
test <- ns_r[(num_train+1):num, ]

ns.tr <- tree(re78~., train) 
draw.tree(ns.tr)
summary(ns.tr)

ns.lr <- lm(re78~., train)
summary(ns.lr)

ns.svm <- svm(re78~., data=train, type = "eps-regression",eps=0.5,cost = 1)
summary(ns.svm)

ns_tr_predict <- predict(ns.tr, test[-10])
ns_lr_predict <- predict(ns.lr, test[-10])
ns_svm_predict <- predict(ns.svm, test[-10])


tr_mistake <- sd(test$re78 - ns_tr_predict)
lr_mistake <- sd(test$re78 - ns_lr_predict)
svm_mistake <- sd(test$re78 - ns_svm_predict)

tr_mistake
lr_mistake
svm_mistake

#task 4
library(tree)
library(maptree)

features <- c("F1", "F2", "F3", "F4", "F5", "Class")
lenses = read.table("D://Lenses.txt", col.names = features)
lenses
lenses = lenses[, -1] #delete ID (F1)
lenses

len.tr <- tree(Class~., lenses)
draw.tree(len.tr)

str <- data.frame("F2" = 2, "F3" = 1, "F4" = 2, "F5" = 1, "Class" = "")
predict(len.tr, str)

summary(len.tr)
len.tr

#task 6

library(tree)
library(maptree)

train <- read.table("D://svmdata4.txt", stringsAsFactors = TRUE) 
test <- read.table("D://svmdata4test.txt", stringsAsFactors = TRUE) 
test

svmdata.tr <- tree(Colors ~., train) 
draw.tree(svmdata.tr) 
svmdata.tr

svmdata.tr1 <- snip.tree(svmdata.tr, nodes = 4) 
draw.tree(svmdata.tr1, cex = 0.7)
res <- as.matrix(predict(svmdata.tr1, test)) 
predict(svmdata.tr1, test) 
num_test <- dim(test)[1]
num_correct <- 0
i <- 1
while (i <= num_test){
  if(res[[i,1]] < res[[i,2]]) type = "red"
  else type = "green"
  
  if(type == test[[i, 3]]) num_correct = num_correct +1
  i = i+1
}

acc=num_correct/num_test
acc

#task 7
library(tree)
library(maptree)

train<-read.csv(file="D://train.csv")
test<-read.csv(file="D://test.csv")
test

titanic.tr<-tree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, train)
draw.tree(titanic.tr, cex = 0.7)
summary(titanic.tr)

titanic.pred<-predict(titanic.tr, test)
titanic.pred
res <- as.matrix(titanic.pred)
res

titanic.sol<-data.frame(PassengerId = test$PassengerId, Survived = titanic.pred)
num_test <- dim(test)[1]
num_correct <- 0
i <- 1
while (i <= num_test){
  if(res[i] < 0.5) type = 0
  else type = 1
  
  if(type == test[i, 12]) num_correct = num_correct +1
  i = i+1
}
acc=num_correct/num_test
acc
