library(readr)
library(dplyr)
library(purrr)
library(mclust)
library(ggplot2)

load("data/final_data.Rdata")



# Dr. Wheeler's Set-up code ===================================================

library(splines)
times <- seq(1,295) / 100 # Observations in 1/100th of a second
X <- bs(times, intercept = TRUE, df = 60) # create a spline to model the data
betas <- matrix(0, nco = 60, nrow = 6792)

# run a linear regression on each data set
# here I am manipulating my data you I can cluster
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii, 6:300]) ~ X - 1) # -1 removes the natural intercept
  betas[ii, ]  <- coefficients(temp)
}
cdata <- cbind(final_data[, 1:5], betas)

#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

# =============================================================================



# k-means section -------------------------------------------------------------

pca <- princomp(cdata[, 2:65])

# c) Run the command "set.seed(12345)" and run a k-means clustering algorithm 
#    using the pca scores.

#   a) Compute the graph of mean spirometry for the 4 clusters (all 4 on one graph). 
#   b) Look at cluster 3. Plot the graph of this cluster and give the mean values (on
#      the original scale) for columns 2-65. What makes this cluster different from 
#      the other clusters?  Describe this cluster so a physician can better understand
#      important characteristics of these clusters.  
#   c) Looking at clusters 1,2, and 4 which clusters has the largest lung capacity?
#      which one has the least lung capacity? Describe these three groups in terms of 
#      the curves as well as the additional variables that are available in the data 
#      frame cdata. Provide figures with your descriptions. 

set.seed(12345)
km <- kmeans(pca$scores, 4)
cluster <- km$cluster
cdata_km <- cbind(cdata, cluster)

km_clust_means <- 
  cdata_km %>%
  group_by(cluster) %>%
  summarise_at(6:65, mean) %>%
  rename_at(2:61 , ~ paste0("b", 1:60))

# function for calcuating y-values for each cluster
calc_y <- function(row) {
  b <- matrix(as.numeric(row[2:61]), ncol = 1)
  X %*% b
}

Y_km <- apply(km_clust_means, 1, calc_y)
km_plot_df <- 
  data_frame(
    time = rep(times, 4),
    cluster = unlist(purrr::map(as.list(1:4), ~ rep(.x, 295))),
    y = c(Y_km)
  )

# plotting results (all clusters)
ggplot(km_plot_df, aes(time, y, color = as.factor(cluster))) +
  geom_line() +
  scale_color_discrete("Cluster") +
  ylab("ML")

# plotting cluster 3 only
km_plot_df %>%
  filter(cluster == 3) %>%
  ggplot(aes(time, y)) +
  geom_line() +
  ylab("ML")

# plotting clusters 1, 2, and 4
km_plot_df %>%
  filter(cluster %in% c(1, 2, 4)) %>%
  ggplot(aes(time, y, color = as.factor(cluster))) +
  geom_line() +
  scale_color_discrete("Cluster") +
  ylab("ML")


# MCLUST section --------------------------------------------------------------

# a) Using mclustbic() and columns 10-20 of cdata (NOT the principal component values).
# estimate the optimal number of  cluster components using the BIC and only with 
# modelNames='VVV' and G = 1:20. Show a graph of the estimate. Is this number different than 
# the ones given above, why? (This will take a while).

set.seed(12345)
BIC <- mclustBIC(cdata[, 10:20], G = 1:20, modelNames = "VVV")
plot(BIC)
summary(BIC)

# b) Now using G = 6 and modelNames='VVV' and the same columns, 
# provide a graph of each cluster's mean curve (USING ALL OF THE DATA COLUMNS). 
# Put all plots on one graph.

set.seed(12345)
mm <- Mclust(cdata[, 10:20], G = 6, modelNames = "VVV")
cluster <- mm$classification
cdata_mm <- cbind(cdata, cluster)

mm_clust_means <- 
cdata_mm %>%
  group_by(cluster) %>%
  summarise_at(6:65, mean) %>%
  rename_at(2:61 , ~ paste0("b", 1:60))

# creating df for plotting
Y_mm <- apply(mm_clust_means, 1, calc_y)
mm_plot_df <- 
  data_frame(
    time = rep(times, 6),
    cluster = unlist(purrr::map(as.list(1:6), ~ rep(.x, 295))),
    y = c(Y_mm)
  )

# plotting results
ggplot(mm_plot_df, aes(time, y, color = as.factor(cluster))) +
  geom_line() +
  scale_color_discrete("Cluster") +
  ylab("ML")


# TO-DO ***********************************************************************
# To answer the remaining questions I think we just need to look at the 
# dataframes cdata_mm and cdata_km and calculate some basic summary stats 
# to compare between clusters



