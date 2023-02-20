library(Rtsne)
library(kknn)
library(mlbench)
library(adabag)
library(rpart)
library(cluster)
library(glmnet)

col_names=c("BI-RADS","Age","Shape","Margin","Density","Severity")
mammography<-read.table("D:\\mammographic_masses_data.txt", sep=',', header=F,col.names=col_names,na.strings='?')
mammography
#remove rows with NA value in any column data frame
mammography<-mammography[complete.cases(mammography),]
mammography
strings_num<-dim(mammography)[1]
data<-mammography[order(runif(strings_num)),]
data

tsne<-Rtsne(as.matrix(data),pca=TRUE,dim=2,check_duplicates = FALSE)
plot(tsne$Y,pch=21,bg=c("Blue","Red"),main="Visualization of mammography data")

#knn
train_data<-data[1:(0.8*strings_num),]
test_data<-data[(0.8*strings_num+1):strings_num,]
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

kknn_model1<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=1)

kknn_model2<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=3)

kknn_model3<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=7)

kknn_model1
plot(kknn_model1, main="Distance = 1")
kknn_model2
plot(kknn_model2, main="Distance = 3")
kknn_model3
plot(kknn_model3, main="Distance = 7")

#bagging
train_data<-data[1:(0.8*strings_num),]
test_data<-data[((0.8*strings_num)+1):strings_num,]
train_data
test_data

trees_num <- seq(1, 201, 10)
misClass <- c()
it_num <- 4

k=1
for (i in trees_num)
{
  misCl <- c()
  for(j in 1:it_num)
  {
    bagging_model <- bagging(Severity ~ ., data = train_data, mfinal = i)
    pr <- predict(bagging_model, test_data)
    misCl[j] <- pr$error
  }
  misClass[k] <- mean(misCl)
  k=k+1
}

mean(misClass)
misClass
print(min(misClass))

plot(seq(1, 201, 10), misClass, col = "steelblue", xlab="Number of trees", ylab="Error", pch = 20, cex = 1.2, type="b")

#boosting
train_data<-data[1:(0.8*strings_num),]
test_data<-data[(0.8*strings_num+1):strings_num,]

trees_num <- seq(1, 201, 10)
misClass <- c()
it_num <- 4

k=1
for (i in trees_num)
{
  misCl <- c()
  for(j in 1:it_num)
  {
    boosting_model <- boosting(Severity ~ ., train_data, mfinal = i)
    pr <- predict(boosting_model, test_data)
    misCl[j] <- pr$error
  }
  misClass[k] <- mean(misCl)
  k=k+1
}

mean(misClass)
misClass
print(min(misClass))

plot(seq(1, 201, 10), misClass, col = "steelblue", xlab="Number of trees", ylab="Error", pch = 20, cex = 1.2, type="b")

#k-means
cluster_data<-data[,-6]
model<-kmeans(cluster_data, 2, iter.max = 100)
model$cluster[model$cluster == 1] <- 0
model$cluster[model$cluster == 2] <- 1

misCl = 0
for (i in 1:strings_num)
{
  if (model$cluster[i] != data$Severity[i])
    misCl = misCl + 1
  #print(model$cluster[i])
  #print(data$Severity[i])
}
misCl<-misCl/strings_num
misCl

plot(agnes(cluster_data))

matr <- dist(scale(cluster_data))
hc <- hclust(matr)
hcd <- as.dendrogram(hc)
plot(hcd, cex = 0.7)

#main features
X<-as.matrix(data[,-6])
Y<-data[,6]
glm<-glmnet(X,Y,family="binomial",nlambda =97,alpha =1)
res<-as.matrix(glm$beta)
res



#autocoder 1 
library(h2o)
h2o.init()
features <- as.h2o(data[-6])

ae <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features,
  autoencoder = TRUE,
  hidden = 3, # 6 to 3 features
  activation = 'Rectifier'
)

ae_codings <- h2o.deepfeatures(ae1, features, layer = 1)
ae_codings
data_2 <- as.data.frame(ae_codings)
data_2$Severity <- data$Severity

tsne<-Rtsne(as.matrix(data_2),pca=TRUE,dim=2,check_duplicates = FALSE)
plot(tsne$Y,pch=21,bg=c("Blue","Red"),main="Visualization of coding mammography data")

strings_num <- dim(data_2)[1]
rand_data <- data_2[ order(runif(strings_num)),]
train_data<-rand_data[1:(0.8*strings_num),]
test_data<-rand_data[(0.8*strings_num+1):strings_num,]

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

kknn_model1<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=1)

kknn_model2<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=3)

kknn_model3<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=7)

kknn_model1
plot(kknn_model1, main="Distance = 1")
kknn_model2
plot(kknn_model2, main="Distance = 3")
kknn_model3
plot(kknn_model3, main="Distance = 7")


#autocoder 2
library(h2o)
h2o.init()
features <- as.h2o(data[-6])

ae <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = features,
  autoencoder = TRUE,
  hidden = 100, # 6 to 100 features
  activation = 'Rectifier'
)

ae_codings <- h2o.deepfeatures(ae, features, layer = 1)
ae_codings
data_2 <- as.data.frame(ae_codings)
data_2$Severity <- data$Severity

tsne<-Rtsne(as.matrix(data_2),pca=TRUE,dim=2,check_duplicates = FALSE)
plot(tsne$Y,pch=21,bg=c("Blue","Red"),main="Visualization of coding mammography data")

strings_num <- dim(data_2)[1]
rand_data <- data_2[ order(runif(strings_num)),]
train_data<-rand_data[1:(0.8*strings_num),]
test_data<-rand_data[(0.8*strings_num+1):strings_num,]

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

kknn_model1<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=1)

kknn_model2<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=3)

kknn_model3<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=7)

kknn_model1
plot(kknn_model1, main="Distance = 1")
kknn_model2
plot(kknn_model2, main="Distance = 3")
kknn_model3
plot(kknn_model3, main="Distance = 7")

#autocoder 3
library(h2o)
h2o.init()
features <- as.h2o(data[-6])
noise_features <- as.h2o(data[-6] + rnorm(nrow(data), mean = 0, sd = 0.1))
denoise_ae <- h2o.deeplearning(
  x = seq_along(features),
  training_frame = noise_features,
  validation_frame = features,
  autoencoder = TRUE,
  hidden = 60,
  activation = 'Rectifier',
  sparse = TRUE
)

ae_codings <- h2o.deepfeatures(ae, features, layer = 1)
ae_codings
data_2 <- as.data.frame(ae_codings)
data_2$Severity <- data$Severity

tsne<-Rtsne(as.matrix(data_2),pca=TRUE,dim=2,check_duplicates = FALSE)
plot(tsne$Y,pch=21,bg=c("Blue","Red"),main="Visualization of coding mammography data")

strings_num <- dim(data_2)[1]
rand_data <- data_2[ order(runif(strings_num)),]
train_data<-rand_data[1:(0.8*strings_num),]
test_data<-rand_data[(0.8*strings_num+1):strings_num,]

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

kknn_model1<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=1)

kknn_model2<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=3)

kknn_model3<-train.kknn(Severity ~., train_data, kmax=round(sqrt(strings_num),digits=0), kernel=ker, distance=7)

kknn_model1
plot(kknn_model1, main="Distance = 1")
kknn_model2
plot(kknn_model2, main="Distance = 3")
kknn_model3
plot(kknn_model3, main="Distance = 7")
