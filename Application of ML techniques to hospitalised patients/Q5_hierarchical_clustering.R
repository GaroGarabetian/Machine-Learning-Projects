# 
# Research Question 5 :
#Employ hierarchical clustering to understand the hierarchical
# structure of patient groups using the numerical features 
#age, weight, glucose levels, (glu_min, glu_max) and SOFA score. 
#Discuss how these hierarchical relationships can
# inform targeted treatment strategies.
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
              "GGally",
              "cluster","scatterplot3d","e1071","ROCR")
load_packages(packages)

#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)

#keep the data we need for the models only
names(traindata)
variable_needed <-c("weight","age","glu_min","glu_max" ,"SOFA" ,"death")

traindata <- traindata[,variable_needed]
testdata <- testdata[,variable_needed]

glimpse(traindata)
str(traindata)
str(testdata)

#Checking the distributions of data and for unusual values
alldata = rbind(traindata,testdata)
view(dfSummary(alldata))


############### FOR LATER USE IN LATEX
st_options(
  plain.ascii = FALSE, 
  style = "rmarkdown",
  dfSummary.style = "grid",
  dfSummary.valid.col = FALSE,
  dfSummary.graph.magnif = .52,
  tmp.img.dir = "/tmp"
)
# 
define_keywords(title.dfSummary = "Data Frame Summary in PDF Format")
dfSummary(alldata)


ggpairs(alldata,aes(color = death, alpha = 0.5))

str(alldata)
md.pattern(alldata,plot = TRUE)



target = alldata[, 6]#Death
alldata = alldata[, 1:5] #removing Death

# 
# #methods for hierarchical clustering 	
# the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#Cluster Similarity: Ward’s Method
# • Similarity of two clusters is based on the increase in squared error when
# two clusters are merged
# • Similar to group average if distance between points is distance squared
# • Less susceptible to noise and outliers
# • Biased towards globular clusters
# • Hierarchical analogue of K-means
# • Can be used to initialize K-means

# Tall merges in the dendrogram suggest strongly distinct clusters.
# 
# Short merges indicate clusters that are very similar.

d = dist(scale(alldata))
hc_single <- hclust(d, method = 'single') 
hc_average <- hclust(d, method = 'average') 
hc_complete <- hclust(d, method = 'complete') 
hc_ward2 <- hclust(d, method = 'ward.D2')
hc_ward <-  hclust(d,method = 'ward.D')#pretty bad clustrering for 2
hc_mcq <- hclust(d, method = 'mcquitty')
hc= hc_single
hc = hc_average
hc<-hc_complete
hc = hc_ward
hc<- hc_ward2
hc = hc_mcq
plot(hc)

slc = c()
for (i in 2:20){
  clusters = cutree(hc, k = i)
  slc [i-1] = mean(silhouette(clusters, d)[, 3])
}
plot(2:20, slc, type="b", xlab="Number of Clusters", ylab="Silhouette")



clusters = cutree(hc_single,k=2)   # detect small groups
table(cutree(hc_single,k=2))
clusters = cutree(hc_complete,k=2) # detects small groups
table(cutree(hc_complete,k=2))

clusters = cutree(hc_average,k=2)  # detects small groups
table( cutree(hc_average,k=2))

clusters = cutree(hc_ward,k=2) # good cluster
table(cutree(hc_ward, k=2))  
clusters = cutree(hc_ward2,k=2)  #good better silhouette
table(cutree(hc_ward2, k=2))  
clusters = cutree(hc_mcq,k=2) #imbalanced
table(cutree(hc_mcq, k=2))  

clusters = cutree(hc, k = 2)

plot(alldata, col = clusters, pch = 15, main = "Ward D2 Method")
#text(data, labels = row.names(hdata), pos = 2)

hc<-hc_ward2
plot(alldata, col = clusters, pch = 15, main = "Ward D2 Method")
legend(x = 70, y = 45,  # Upper-right quadrant
       legend = c(paste("Cluster 1: 5653 obs"), 
                  paste("Cluster 2: 1403 obs")),
       fill = c("blue", "green"),  # Match your plot colors
       cex = 0.8)  # Adjust text size # Remove legend box

plot(hc)
rect.hclust(hc, k = 2)

plot(alldata, col = clusters) # death
plot(alldata, col = target) #compare with the real death
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

# Assuming 'target' contains true labels ("YES"/"NO")
metrics <- data.frame(
  Metric = c("Accuracy", 
             "Precision (NO)", 
             "Precision (YES)", 
             "Recall (Macro)", 
             "F1 Score (Macro)",
             "F1 Score (YES)"),
  Value = c(
    Accuracy(clusters_str, target),
    Precision(clusters_str, target, "NO"),
    Precision(clusters_str, target, "YES"),
    Recall(clusters_str, target),      # Macro-average recall
    F1_Score(clusters_str, target),    # Macro-average F1
    F1_Score(clusters_str, target, "YES")
  )
)

# Print formatted table (using knitr for reports)
knitr::kable(metrics, 
             caption = "Classification Performance of Ward's Clustering",
             digits = 3,
             align = c('l','c'))



model_silhouette = silhouette(clusters, dist(alldata))
model_silhouette
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])


model_silhouette = silhouette(clusters, d)
plot(model_silhouette)

###heatmap
cdata_ord = alldata[order(clusters),]
heatmap(as.matrix(dist(cdata_ord)), Rowv = NA, Colv = NA,
        col = heat.colors(256), revC = TRUE)

###


##ROC curves

pred_obj <- prediction(clusters,target)
# Calculate ROC curve for adjusted predictions

ROCcurve <- performance(pred_obj, "tpr", "fpr")
# Plot the ROC curve
plot(ROCcurve, col = "blue", main = "ROC Curve for Adjusted Predictions")
abline(0, 1, col = "grey", lty = 2)  # Add a diagonal reference line

# Calculate AUC for adjusted predictions
auc_adjusted <- performance(pred_obj, "auc")@y.values[[1]]
legend("topleft",paste("AUC for Predictions:", round(auc_adjusted,3)))







library(cluster)
library(factoextra)
library(ggplot2)
library(patchwork)

# Data preparation
scaled_data <- scale(alldata)
d <- dist(scaled_data)

# Define clustering methods to compare
methods <- c("single", "average", "complete", "ward.D", "ward.D2", "mcquitty")

# Create a list to store all hclust objects
hc_list <- lapply(methods, function(m) hclust(d, method = m))
names(hc_list) <- methods

# Visualize all dendrograms
par(mfrow = c(2, 3))
for (i in seq_along(hc_list)) {
  plot(hc_list[[i]], 
       main = paste("Method:", methods[i]),
       xlab = "",
       sub = "",
       hang = -1)
}
par(mfrow = c(1, 1))  # Reset plot layout

# Select best method based on cophenetic correlation
coph_cor <- sapply(hc_list, function(hc) cor(d, cophenetic(hc)))
best_method <- methods[which.max(coph_cor)]
hc <- hc_list[[best_method]]

cat("Best method based on cophenetic correlation:", best_method, "\n")

# Silhouette analysis for optimal k
sil_scores <- sapply(2:20, function(k) {
  clusters <- cutree(hc, k = k)
  mean(silhouette(clusters, d)[, 3])
})

# Create comprehensive plot
p1 <- fviz_dend(hc, 
                k = which.max(sil_scores) + 1,
                main = paste("Dendrogram (", best_method, "method)"),
                xlab = "Observations",
                ylab = "Height",
                k_colors = "jco",
                rect = TRUE)
p1
p2 <- ggplot(data.frame(K = 2:20, Silhouette = sil_scores), 
             aes(x = K, y = Silhouette)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_vline(xintercept = which.max(sil_scores) + 1, 
             linetype = "dashed", color = "red") +
  labs(title = "Silhouette Analysis",
       x = "Number of Clusters",
       y = "Average Silhouette Width") +
  scale_x_continuous(breaks = seq(2, 20, by = 2)) +
  theme_minimal()

p2
# Combine plots
p1 + p2 + plot_layout(ncol = 1, heights = c(2, 1))

# Optimal cluster info
optimal_k <- which.max(sil_scores) + 1
cat("\nOptimal number of clusters:", optimal_k, 
    "\nAverage silhouette width:", round(max(sil_scores), 3), "\n")

# Visualize clusters
fviz_cluster(list(data = scaled_data, cluster = cutree(hc, optimal_k)),
             ellipse.type = "norm",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = paste("Cluster Plot (k =", optimal_k, ")"))