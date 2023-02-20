#task 1
library(cluster)
data(pluton)

clust<-kmeans(pluton,3,iter.max = 5)
plot(pluton,col=clust$cluster,main="5 iterations")

clust<-kmeans(pluton,3,iter.max = 10)
plot(pluton,col=clust$cluster,main="10 iterations")

clust<-kmeans(pluton,3,iter.max = 100)
plot(pluton,col=clust$cluster,main="100 iterations")

clust<-kmeans(pluton,3,iter.max = 300)
plot(pluton,col=clust$cluster,main="300 iterations")

clust<-kmeans(pluton,3,iter.max = 500)
plot(pluton,col=clust$cluster,main="500 iterations")

clust<-kmeans(pluton,3,iter.max = 700)
plot(pluton,col=clust$cluster,main="700 iterations")

#task 2
n = 100
X1 = rnorm(n, mean = 400, sd = 400)
Y1 = rnorm(n, mean = 0, sd = 40)
X2 = rnorm(n, mean = 300, sd = 80)
Y2 = rnorm(n, mean = 500, sd = 130)
X3 = rnorm(n, mean = 550, sd = 450)
Y3 = rnorm(n, mean = 1000, sd = 40)
data <- cbind(as.matrix(c(X1,X2,X3)),as.matrix(c(Y1,Y2,Y3)))
colnames(data) <- c("x", "y")
data

clust1 <- clara(data, 3, stand = FALSE, metric = "manhattan")
plot(data, col = clust1$clustering, xlab = "x", ylab = "y",main="manhattan, false stand")
clust1$clusinfo

clust2 <- clara(data, 3, stand = FALSE, metric = "euclidean")
plot(data, col = clust2$clustering, xlab = "x", ylab = "y",main="euclidean, false stand")
clust2$clusinfo

clust3 <- clara(data, 3, stand = TRUE, metric = "manhattan")
plot(data, col = clust3$clustering, xlab = "x", ylab = "y",main="manhattan, true stand")
clust3$clusinfo

clust4 <- clara(data, 3, stand = TRUE, metric = "euclidean")
plot(data, col = clust4$clustering, xlab = "x", ylab = "y",main="euclidean, true stand")
clust4$clusinfo

#task 3
library(cluster)
data(votes.repub)
plot(agnes(votes.repub))

#task 4
library(cluster)
data(animals)
plot(agnes(animals))

#task 5
library(cluster)
data <- read.table("D:\\seeds_dataset.txt")
clust <- kmeans(data, 3)
plot(data, col = clust$cluster)
points(clust$centers, col = 1:2, pch = 8, cex=2)
data
data <- data[,-3]
clust <- kmeans(data, 3)
plot(data, col = clust$cluster)
points(clust$centers, col = 1:2, pch = 8, cex=2)




