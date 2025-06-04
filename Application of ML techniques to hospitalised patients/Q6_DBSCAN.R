# Research Question 6 : 
#Use DBSCAN to detect anomalies in patient data, focusing on
# extreme cases in respiration rates (rr_min, rr_max) and oxygen saturation (spo2).
# Analyze the potential significance of these outliers
#and how they relate to critical
# patient conditions and hospitalization outcomes.
rm(list=ls())
# Install and load required libraries
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}
packages <- c("car", "dplyr", "tidyverse", "summarytools",
              "mice","ggplot2","MLmetrics","pROC","vtable","ggpubr",
              "outliers",
              "GGally","caret","randomForest","factoextra",
              "cluster","dbscan","e1071","ROCR")
load_packages(packages)


#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)
alldata = rbind(traindata,testdata)

#keep the data we need for the models only
names(alldata)
variable_needed <-c("rr_min","rr_max" ,"spo2","death")

alldata <- alldata[,variable_needed]

#Checking the distributions of data and for unusual values
glimpse(alldata)
str(alldata)
view(dfSummary(alldata))

############### FOR LATER USE IN LATEX
st_options(
  plain.ascii = FALSE, 
  style = "rmarkdown",
  dfSummary.style = "grid",
  dfSummary.valid.col = FALSE,
  dfSummary.graph.magnif = .60,
  tmp.img.dir = "/tmp"
)
# 
define_keywords(title.dfSummary = "Data Frame Summary in PDF Format")
dfSummary(alldata)
#colnames(alldata)<- c("Resp.Rate Min","Resp.Rate Max","SpO2","Death")
ggpairs(alldata,aes(color = Death, alpha = 0.5))

md.pattern(alldata,plot = TRUE)

target = alldata[, "death"]#Death
data = alldata[, 1:3] #removing Death


d = dist(scale(data))
hc_ward2 <- hclust(d, method = 'ward.D2')
hc_ward <-  hclust(d,method = 'ward.D')


hc = hc_ward2


plot(hc)

slc = c()
for (i in 2:20){
  clusters = cutree(hc, k = i)
  slc [i-1] = mean(silhouette(clusters, d)[, 3])
}
plot(2:20, slc, type="b", xlab="Number of Clusters", ylab="Silhouette")

clusters = cutree(hc, k = 2)

plot(data, col = clusters, pch = 15, main = "Ward's D2 Method")
text(data, labels = row.names(data), pos = 2)
plot(hc)
rect.hclust(hc, k = 2)

plot(data, col = clusters) # death
plot(data, col = target) #compar

# k means cluster and testing the sorted distances in the clusters so we can take right parameters
model = kmeans(data,centers= 2)
plot(data, col = model$cluster + 1)

knndist = kNNdist(data, k = 2)
kNNdistplot(data,k=2)

plot(sort(knndist), type = 'l', xlab = "Points sorted by distance", ylab = "2-NN distance")

model = dbscan(data, eps = 5, minPts = 50)
plot(data, col = model$cluster + 1, pch = ifelse(model$cluster, 1, 4))
model

#DBSCAN
epc =c(3, 4,5,6,9,10,20,30)
minPts <- 20
# Iterate over each eps value
for (eps in epc) {
  # Run DBSCAN
  model <- dbscan(data, eps = eps, minPts = minPts)
  
  # Plot the results
  plot(data, col = model$cluster + 1, pch = ifelse(model$cluster, 1, 4),
       main = paste("DBSCAN with eps =", eps))
  
  # Add a legend
  #legend("topright", legend = c("Noise", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
        # col = c("black", "red", "green", "blue", "purple"), pch = c(4, 1, 1, 1, 1))
}

model = dbscan(data, eps = 3.5, minPts = 20)
plot(data, col = model$cluster + 1, pch = ifelse(model$cluster, 1, 4),   main = paste("DBSCAN with eps =", eps))

# Add a legend
legend("topleft", legend = c("Noise", "Cluster 1"),
       col = c("red","black"), pch = c(4, 1, 1, 1, 1))
model$cluster

clusters = model$cluster

plot(data, col = clusters+1, pch = 15, main = "DBSCAN Method")


plot(data, col = clusters+2) # death
plot(data, col = target) #compare
# 
# For supervised classification we have a variety of measures to evaluate how good our model is
# •
# Accuracy, precision, recall
library(MLmetrics)

clusters_str <- ifelse(clusters=="1","NO","YES")
Accuracy(clusters_str,target)
ConfusionMatrix(clusters_str,target)
Precision(clusters_str,target,"NO") 
Precision(clusters_str,target,"YES")
Recall(clusters_str,target)
F1_Score(clusters_str,target)
F1_Score(clusters_str,target,"YES")




#points(model$centers, col = 4, pch = "+", cex = 2)

model_silhouette = silhouette(clusters, dist(alldata))
model_silhouette
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])

####test dbscan cluster to predict
# Load dataset (example: healthcare data with target 'mortality')
data <- alldata
names(data)
X <- data[, !(names(data) %in% c("death"))]  # Features
y <- data$death                            # Target (binary: 0/1)
#DBSCAN is sensitive to feature scales, so standardize the data:
X_scaled <- scale(X)  # Standardize features (mean=0, sd=1)



# Find optimal epsilon (distance threshold) using k-NN distance plot
knn_dist <- kNNdist(X_scaled, k = 4)  # k = minPts - 1
plot(sort(knn_dist), type = 'l', ylab = "k-NN distance (k=4)", xlab = "Points")
abline(h = 0.5, col = "red")  # Choose epsilon where the curve bends (e.g., 0.5)

# Run DBSCAN
dbscan_result <- dbscan(X_scaled, eps = 0.5, minPts = 5)
X$cluster <- as.factor(dbscan_result$cluster)  # Add cluster labels

#Principal Component Analysis (PCA) is a dimensionality reduction technique that transforms high-dimensional data into a lower-dimensional space while preserving as much variance as possible.
# Reduce dimensions for plotting
pca <- prcomp(X_scaled, rank = 2)
X_pca <- as.data.frame(pca$x)
X_pca$cluster <- as.factor(dbscan_result$cluster)
X_pca$mortality <- y

# Plot clusters
ggplot(X_pca, aes(x = PC1, y = PC2, color = cluster, shape = as.factor(mortality))) +
  geom_point(alpha = 0.7) +
  labs(title = "DBSCAN Clusters (PCA Reduced)", color = "Cluster", shape = "Mortality") +
  theme_minimal()

#Step 5: Train Classifier (With vs. Without Clusters)
# Split data into train/test sets
set.seed(42)
train_idx <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Model 1: Without clusters
rf_baseline <- randomForest(x = X_train[, !(names(X_train) %in% c("cluster"))], 
                            y = as.factor(y_train))
y_pred_baseline <- predict(rf_baseline, X_test[, !(names(X_test) %in% c("cluster"))])

# Model 2: With clusters
rf_with_clusters <- randomForest(x = X_train, y = as.factor(y_train))
y_pred_clusters <- predict(rf_with_clusters, X_test)

# Compare performance
confusionMatrix(y_pred_baseline, as.factor(y_test))$overall["Accuracy"]
confusionMatrix(y_pred_clusters, as.factor(y_test))$overall["Accuracy"]

varImpPlot(rf_with_clusters)

#sessionInfo()
