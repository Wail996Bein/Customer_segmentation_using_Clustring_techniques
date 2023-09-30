
getwd()
setwd("C:\\Users\\Acc\\Desktop\\Customer Segmentation")

customer <- read.csv('Wholesale customers data.csv')
head(customer)
str(customer)

customer1<- customer
customer1<- na.omit(customer1)
customer1$Channel <- NULL
customer1$Region <- NULL

customer1 <- scale(customer1)

wss <- (nrow(customer1)-1)*sum(apply(customer1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(customer1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(customer1, 6) # fit the model
aggregate(customer1,by=list(fit$cluster),FUN=mean) # get cluster means
customer1 <- data.frame(customer1, fit$cluster) #append cluster assignment

library(cluster)
clusplot(customer1, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

customer2 <- customer[, 3:8]
kmeans.result <- kmeans(customer2, centers=6)
kmeans.result$centers

kmeans.result$cluster # print out cluster IDs 
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((customer2 - centers)^2)) # calculate distances
outliers <- order(distances, decreasing=T)[1:5] # pick up top 5 distances
print(outliers)

print(customer2[outliers,])

idx <- sample(1:dim(customer)[1], 40)
customerSample <- customer[idx,]
customerSample$Region <- NULL
customerSample$Channel <- NULL

d <- dist(customerSample, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into 6 clusters

rect.hclust(fit, k=6, border="red")

