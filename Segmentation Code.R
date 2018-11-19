##################################
# Code for Segmentation and Classification 

# @Ceren Kolsarici, 2015
#################################################################

#### Playing around with an R data set #####################

# Motor Trend Car Road Tests
# The data was extracted from the 1974
# Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile 
# design and performance for 32 automobiles (1973-74 models).

data(mtcars)
mydata<-mtcars
str(mtcars)


# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

# Ward Hierarchical Clustering with mtcars data  

d <- dist(mydata, method = "euclidean") # distance matrix

# hclust function is available in the stats package	
fit <- hclust(d, method="ward.D")
str(fit)
plot(fit) # display dendogram

# Observe dendogram and decide on the number of clusters
groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")


# K-Means Cluster Analysis

# Determine number of clusters
# My goal is to minimize the within cluster variance

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) # within group variance for 1 group
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Five Cluster solution seems reasonable

fit <- kmeans(mydata, 5) # 5 cluster solution

# get cluster means 

aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster assignment

mydata <- data.frame(mydata, fit$cluster)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# 
# # Centroid Plot against 1st 2 discriminant functions
# install.packages("fpc")
# library(fpc)
# plotcluster(mydata, fit$cluster)

# ###########################################################
# Analysing the Subscription Data
# ###########################################################


# read the raw data from an online source
seg.raw <- read.csv("http://goo.gl/qw303p")
# remove the column with the known segment assignments
seg.df  <- seg.raw[ , -7] 

summary(seg.df)
head(seg.df) # info on 300 customers and their subscription decisions

# removing negative incomes
seg.df[seg.df$income < 0, ]
seg.df <- seg.df[!(seg.df$income < 0),]
seg.raw <- seg.raw[!(seg.raw$income < 0), ]

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)


#### HCLUST

# # distance example -- by hand
# c(1,2,3) - c(2,3,2) # vector of differences 
# sum((c(1,2,3) - c(2,3,2))^2) # sum of squared differences  
# sqrt(sum((c(1,2,3) - c(2,3,2))^2)) # Root sum of squared differences (Euclidean distance) 
# 
# # using dist()
# dist(rbind(c(1,2,3), c(2,3,2)))

# Euclidean distance only works with numeric data types
# distances using numeric columns of seg.df (first 5 observations)
d <- dist(seg.df[, c("age", "income", "kids")])
as.matrix(d)[1:5, 1:5]
rm(d)   # clean up - remove the distance object d from the workspace

# now the real hclust() work

library(cluster)      

# We cannot assume factor variables are irrelevant for segmentation
# So we have to check the distance for all the variables
seg.dist <- daisy(seg.df)     # daisy works with mixed data types
# inspect some of the results
as.matrix(seg.dist)[1:5, 1:5]

seg.hc <- hclust(seg.dist, method="complete")

plot(seg.hc)

# very hard to read
# zoom in on just part of it
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[2]])

# check some of the proposed similarities
seg.df[c(204, 215), ]  # similar
seg.df[c(95, 185), ]  # dissimilar
seg.df[c(173, 141), ]  # less similar

# examine cophenetic correlation (i.e. how well the dendogram matches the true distance metric)
cor(cophenetic(seg.hc), seg.dist) #interpreted similar to R2


# see hclust's proposal for 4 groups
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")

# actually get 4 groups
seg.hc.segment <- cutree(seg.hc, k=4)     # membership vector for 4 groups
table(seg.hc.segment)


# what did hclust come up with?
seg.summ(seg.df, seg.hc.segment)

# plot this
plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)), 
     col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))



#### K-MEANS
# convert factor variables to numeric (kmeans requires). OK b/c all are binary.
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)



# Determine number of clusters
# My goal is to minimize the within cluster variance
wss <- (nrow(seg.df.num)-1)*sum(apply(seg.df.num,2,var))  # this is a manual test of cluster with centroid number = 1, aka. 1 cluster
for (i in 2:15) wss[i] <- sum(kmeans(seg.df.num,          # note here, that's why it's started with index 2 rather than 1
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",      # and here, it plots them all
     ylab="Within groups sum of squares")

set.seed(96743) # set seed to make sure the random assignment starts at the same point
seg.k <- kmeans(seg.df.num, centers=4)

# inspect it
seg.summ(seg.df, seg.k$cluster)
seg.summ(seg.df.num, seg.k$cluster)

# plot one of the variables
boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")


# plot the result
library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")


#### MCLUST  --- Model based clustering 

# do mclust for segments
install.packages("mclust")
library(mclust)

# ###
# # convert factor variables to numeric (mclust requires). OK b/c all are binary.
# # these lines are the same as above for k-means 
# seg.df.num <- seg.df
# seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
# seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
# seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
# summary(seg.df.num)
# ###
# 

# fit the model
seg.mc <- Mclust(seg.df.num)
summary(seg.mc)

# what if we estimate 4 clusters?
seg.mc3 <- Mclust(seg.df.num, G=3)
summary(seg.mc3)

# what if we estimate 4 clusters?
seg.mc4 <- Mclust(seg.df.num, G=4)
summary(seg.mc4)

# compare the two models
print(c(seg.mc$bic, seg.mc4$bic))


# examine the 3-cluster model
seg.summ(seg.df.num, seg.mc3$class)


# plot the 3-cluster model
library(cluster)
clusplot(seg.df, seg.mc3$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")




#### CLASSIFICATION

#### NAIVE BAYES
install.packages("e1071")   # if needed
library("e1071")
set.seed(04625)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[train.cases, ]
seg.df.test  <- seg.raw[-train.cases, ]

library(e1071)
seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train) # use naiveBayes to understand segment membership
summary(seg.nb)
seg.nb.class <- predict(seg.nb, seg.df.test) # predict the segment membership in the  new data

# frequencies in predicted data
prop.table(table(seg.nb.class))
table(seg.nb.class)

# plot it
clusplot(seg.df.test[, -7], seg.nb.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, 
         main="Naive Bayes classification, holdout data")


# compare to known segments (which we can do with this test data)
mean(seg.df.test$Segment==seg.nb.class)

# look into the matches- confusion matrix
table(seg.df.test$Segment,seg.nb.class)

# instead of raw agreement, one should assess performance
# above chance. 
# adjusted for chance
library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment)
# 
# In this case, we see that NB was able to recover the segments in the
# # test data imperfectly but substantially better than chance
# zero is the expected value, negative ARI points out to the lack of accuracy

# summary data for proposed segments in the test data
seg.summ(seg.df.test, seg.nb.class)
# summary data for the known segments in the test data
seg.summ(seg.df.test, seg.df.test$Segment)

# predict raw probabilities
predict(seg.nb, seg.df.test, type="raw")


