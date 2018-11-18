# clust finial hw
library(splines)
library(dplyr)
library(mclust)
library(factoextra)
times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)

#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$EVER_SMOKE)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

# a) Perform a principal components analysis on columns 2 through 65. List the standard
# deviations for the first 5 components.
set.seed(12345)
pca = princomp(cdata[ , 2:65])
pca$sdev
# 1. 45.2117509
# 2. 31.2901007
# 3. 22.3753978
# 4. 17.3299734
# 5. 13.0471355


# b) Using all pca scores compute the optimal number of clusters using kmeans using both
# "wss" and the "silhouette" method. What is the optimal number of components using each 
# method. Why may this number be different?
fviz_nbclust(pca$scores, kmeans, method = "wss") # 4 or 5
fviz_nbclust(pca$scores, kmeans, method = "silhouette") # 2
# the number is different because silhouette measures how similar an object is to
# its own cluster compared to other clusters
# WSS is a measure of the homogeneity within the cluster. they measure differnt things


# not sure where to go from here. question c below is confusing.







# c) Run the command "set.seed(12345)" and run a k-means clustering algorithm using the
# pca scores
set.seed(12345)
spiro <- read_csv("spiro.csv", col_names = FALSE)
sp <- spiro[1:10000,18:151] #get the actual data only worry about the first
#10,000 observations
times <- seq(1,134)/100 # Observations in 1/100th of a second
# transform time to seconds

X <- bs(times,intercept=TRUE,df=40) #create a spline to
#model the data

temp <- lm(as.numeric(sp[3,])~X-1) #-1 removes the natural intercept
plot(times,X%*%matrix(coefficients(temp)),type='l',xlab="Time",ylab="ML")
points(times,as.numeric(sp[3,]))
###########################################
betas <- matrix(0,ncol=40,nrow = 10000)
# run a linear regression on each data set
# here I am manipulating my data so I can cluster
nalist <- rep(0,10000)
for (ii in 1:10000){
  temp <- lm(as.numeric(sp[ii,])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
  nalist[ii] <- sum(is.na(betas[ii,]))
}

bet <- betas[nalist == 0,]

#Figure out the number of "clusters"
temp <- sample_frac(data.frame(bet),0.15)
fviz_nbclust(scale(temp), kmeans, method = "wss")
fviz_nbclust(scale(temp), kmeans, method = "silhouette")

cmeans <- matrix(colMeans(bet),40,1)
stdev  <- matrix(apply(bet,2,sd),40,1)

k_means4 <- kmeans(scale(bet),4,nstart = 25)

cl1 <- matrix(k_means4$centers[1,],40,1)
bl1 <- cl1 * stdev + cmeans
plot(times,X%*%bl1,ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,90))
cl2 <- matrix(k_means4$centers[2,],40,1)
bl2 <- cl2 * stdev + cmeans
lines(times,X%*%bl2,lwd=2,col=2)
cl3 <- matrix(k_means4$centers[3,],40,1)
bl3 <- cl3 * stdev + cmeans
lines(times,X%*%bl3,lwd=2,col=3)
cl4 <- matrix(k_means4$centers[4,],40,1)
bl4 <- cl4 * stdev + cmeans
lines(times,X%*%bl4,lwd=2,col=4)


