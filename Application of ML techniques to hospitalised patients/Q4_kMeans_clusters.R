# Research Question 4 :
#Can k-Means clustering effectively group patients based on
# their weight,
#white blood cell count (wbc_min, wbc_max),
#and glucose level (glu_min,  glu_max)? 

#Describe the cluster characteristics and explore potential correlations with
# patient risk levels.

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
packages <- c("car", "dplyr", "tidyverse", "summarytools","patchwork",
              "mice","ggplot2","MLmetrics","pROC","vtable","ggpubr",
              "outliers",
              "GGally",
              "cluster","e1071","ROCR")
load_packages(packages)

#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)

#keep the data we need for the models only
names(traindata)
variable_needed <-c("weight","wbc_min","glu_min","glu_max" ,"wbc_max" ,"death")

traindata <- traindata[,variable_needed]
testdata <- testdata[,variable_needed]
alldata = rbind(traindata,testdata)
glimpse(traindata)
str(traindata)
str(testdata)

#Checking the distributions of data and for unusual values
view(dfSummary(traindata))
view(dfSummary(testdata))
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

#wbc_min & wbc_max can be problematic
plot(traindata$wbc_min)
hist(traindata$wbc_min)

summary(traindata$wbc_min)
library(mice)
md.pattern(testdata,plot = TRUE)
md.pattern(traindata,plot = TRUE)
imp <- mice(traindata[,variable_needed], m = 3, print = FALSE, seed = 28)
traindata<-complete(imp,2)
imp <- mice(testdata[,variable_needed], m = 3, print = FALSE, seed = 28)
testdata <-complete(imp,2)

summary(testdata$wbc_max)
summary(testdata$wbc_min)
summary(traindata$wbc_max)
summary(testdata$wbc_min)
alldata = rbind(traindata,testdata)
dfSummary(alldata)
#### Setting the datasets

xtrain = traindata[,c( "weight","wbc_min","glu_min","glu_max" ,"wbc_max")]
ytrain = traindata[,"death"]
xtest = testdata[,c("weight","wbc_min","glu_min","glu_max" ,"wbc_max")]
ytest = testdata[,"death"]

################## Create pairwise plots
ggpairs(traindata, aes(color = death, alpha = 0.5))
ggpairs(testdata,aes(color = death, alpha = 0.5))
ggpairs(alldata,aes(color = death, alpha = 0.5))
alldata = rbind(traindata,testdata)
str(alldata)
# # Transforming the factor death to numeric YES = 2, No = 1
# alldata$death <-ifelse(alldata$death == "YES", 2, 1)
#colnames(cdata) <- c( "Weight","wbc Min","Glu Min","Glu Max" ,"wbc Max","Death")
cdata = alldata
target = cdata[, 6]#Death
cdata = cdata[, 1:5]

plot(cdata, col = target) #color 1 = black NO , color = 2 YES Death

SSE <- c()
SSE_separation<-c()
for (i in 1:10){ 
  SSE[i] <- kmeans(cdata, centers = i)$tot.withinss
  SSE_separation[i] <- kmeans(cdata, centers = i)$betweenss
}
# Create the first plot for SSE
plot(1:10, SSE, type="b", col="red", xlab="Number of Clusters", ylab="Sum of Squares", ylim=c(min(SSE, SSE_separation), max(SSE, SSE_separation)))
# Add the second plot for SSE_separation using lines()
lines(1:10, SSE_separation, type="b", col="blue")
# Add a legend
legend("bottomright", legend=c("SSE (Within Cluster)", "SSE (Between Cluster)"), col=c("red", "blue"), lty=1, pch=1)

model = kmeans(cdata, centers = 2,nstart = 25)

model$centers
model$cluster

cohesion = model$tot.withinss
separation = model$betweenss


plot(cdata, col = model$cluster) # death
plot(cdata, col = target) #compare



#points(model$centers, col = 4, pch = "+", cex = 2)

model_silhouette = silhouette(model$cluster, dist(cdata))
model_silhouette
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])


#Measuring Cluster Validity Via Correlation





cdata_ord = cdata[order(model$cluster),]
heatmap(as.matrix(dist(cdata_ord)), Rowv = NA, Colv = NA,
        col = heat.colors(256), revC = TRUE)


#risk calculation

# Create contingency table
mortality_table <- table(Cluster = model$cluster, Death = target)

# Calculate risk in each cluster
risk_cluster1 <- mortality_table[1,2] / sum(mortality_table[1,]) # Cluster 1 mortality risk
risk_cluster2 <- mortality_table[2,2] / sum(mortality_table[2,]) # Cluster 2 mortality risk

# Calculate relative risk (RR)
relative_risk <- risk_cluster2 / risk_cluster1

# Calculate risk difference
risk_difference <- risk_cluster2 - risk_cluster1

# Print results
cat(sprintf("Cluster 1 Mortality Risk: %.1f%%\n", risk_cluster1*100))
cat(sprintf("Cluster 2 Mortality Risk: %.1f%%\n", risk_cluster2*100))
cat(sprintf("Relative Risk (Cluster2/Cluster1): %.2f\n", relative_risk))
cat(sprintf("Risk Difference: %.2f%%\n", risk_difference*100))









