# Course Assignment – 
#Application of techniques for predicting the outcome of a
# hospitalized patient
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
packages <- c("car", "dplyr", "tidyverse", "summarytools", "mice","ggplot2")
load_packages(packages)


traindata = read.csv("train.csv",stringsAsFactors = TRUE)
testdata = read.csv("test.csv",stringsAsFactors = TRUE)
alldata = rbind(traindata,testdata)
str(traindata)
str(testdata)

glimpse(traindata[,-1])

#MISSING DATA

md.pattern(traindata,plot = TRUE)
md.pattern(testdata,plot = TRUE)

#Checking the distributions of data and for unusual values
#https://cran.r-project.org/web/packages/summarytools/vignettes/rmarkdown.html
######## FOR LATER USE IN LATEX
# st_options(
#   plain.ascii = FALSE, 
#   style = "rmarkdown",
#   dfSummary.style = "grid",
#   dfSummary.valid.col = FALSE,
#   dfSummary.graph.magnif = .52,
#   tmp.img.dir = "/tmp"
# )
# 
# define_keywords(title.dfSummary = "Data Frame Summary in PDF Format")
# dfSummary(traindata[-1])
###################
view(dfSummary(traindata[-1]))

view(dfSummary(testdata[-1]))

#Anomally Detected on the Weight Variable that has some extreme values 
#both in traindata and testdata

plot(traindata[,"weight"])
plot(testdata[,"weight"])
summary(testdata[,"weight"])
summary(traindata[,"weight"])

#detects only 1 outlier depends on normal distribution
library(outliers)
grubbs.test(traindata$weight)

#just for reference Tukey’s Fences, a method in his 1977 book Exploratory Data Analysis.
    # Calculate the IQR for weight IQR = Q3 - Q1
#extreme outliers as those beyond 3 times the IQR. ref1
#concluding that these extreme outliers
    #IQR is not affected by extreme values
    iqr_value <- IQR(traindata$weight, na.rm = TRUE)
    
    # Calculate the first and third quartiles
    q1 <- quantile(traindata$weight, 0.25, na.rm = TRUE)
    q3 <- quantile(traindata$weight, 0.75, na.rm = TRUE)
    
    # Calculate the lower and upper bounds for outliers
    lower_bound <- q1 - 1* iqr_value
    upper_bound <- q3 +3 * iqr_value
    
    # Check for outliers
    outliers <- traindata$weight[traindata$weight < lower_bound | traindata$weight > upper_bound]
    
    # Display the outlier values
   cat("The lower bound is : ", lower_bound)
   cat("The upper bound is : ", upper_bound)
  
    outliers
    grubbs.test(traindata$weight)


#Finding the outliers or wrongly inserted data
datasets <- list(traindata = traindata, testdata = testdata)  
outliers_list <- list()  # Initialize a list to store outlier indices

upper_bound <- 185
lower_bound<- 40


for (name in names(datasets)) {
  data <- datasets[[name]] #extract dataset
  
  # Find outlier indices and values
  outlier_indices_max <- which(data$weight > upper_bound)  
  outlier_values_max <- data$weight[outlier_indices_max]  # Extract actual values
  
  outlier_indices_low <- which(data$weight < lower_bound)  
  outlier_values_low <- data$weight[outlier_indices_low]  # Extract actual values
  
  # Print results
  cat("\nDataset:", name, 
      "\nOutliers (>",upper_bound,") - Indices:", outlier_indices_max, 
      "\nOutliers (>",upper_bound,") - Values:", outlier_values_max, 
      "\nOutliers (<",lower_bound,") - Indices:", outlier_indices_low, 
      "\nOutliers (<",lower_bound,") - Values:", outlier_values_low, "\n")
  # Combine indices into a data frame
  outliers_df <- data.frame(
    dataset = name,
    outlier_indices = c(outlier_indices_max, outlier_indices_low),
    outlier_values = c(data$weight[outlier_indices_max], data$weight[outlier_indices_low])
  )
  
  # Store the data frame in the list
  outliers_list[[name]] <- outliers_df
}


# Combine all outlier data frames into one
outliers_all <- do.call(rbind, outliers_list)
cat("All outliers for variable Weight: ")
print(outliers_all)  

# Detailed Inspection
# Avoid removing the wrong values and checking other columns
traindata[outliers_list$traindata$outlier_indices, -1]
testdata[outliers_list$testdata$outlier_indices,-1 ]

w_count1<-dim(traindata[outliers_list$traindata$outlier_indices, -1])[1]
w_count2<-dim(testdata[outliers_list$testdata$outlier_indices, -1])[1]

#Keeping the old values and subject_id for a potential change and comparison
# we proceed accordingly:
modified_subjects_test <-data.frame(modified_subject_id=testdata[outliers_list$testdata$outlier_indices, "subject_id"],
                                    original_weight = testdata[outliers_list$testdata$outlier_indices, "weight"])

modified_subjects_train <-data.frame(modified_subject_id=traindata[outliers_list$traindata$outlier_indices, "subject_id"],
                                     original_weight = traindata[outliers_list$traindata$outlier_indices, "weight"])

modified_subjects_id <- rbind(modified_subjects_train, modified_subjects_test)


cat("Wrong Weight Registries are ",
    round((dim(modified_subjects_id)[1]/dim(alldata)[1])*100,3),"% of all data.")

cat("In the training data the wrong registries for Weight are ",w_count1,
    "which is :",round((w_count1/dim(traindata)[1])*100,3), " % of all data.")

cat("In the test data the wrong registries for Weight are ",w_count2,
    "which is :",round((w_count2/dim(testdata)[1])*100,3), " % of all data.")

cat("In order to preserve the complete characteristics of the subjects, we are going to 
    NA the weight outliers and do a linear imputation for the continuous variables.
    This method is preferrable for much larger percentage of missing data than
    this one, but we aim to maintain the integrity of the dataset by preserving relationships
    between variables. Even small amounts of missing data can introduce bias or
    distort model results if not properly handled. Linear imputation provides
    a straightforward way to estimate missing values based on existing data,
    ensuring that the analysis is more robust and consistent.")

### METHODS TO PROCCED
  ## 1. OMIT THEM, 
      #if it is a really small percentage in our population(LESS THAN 4%)
      #data <-data %>%
      #         filter(data$weight >= 40 & data$weight <= 240)
      
      # # Remove outliers from both datasets
      # traindata_clean <- traindata[-outliers_list$traindata$outlier_indices, ]
      # testdata_clean <- testdata[-outliers_list$testdata$outlier_indices, ]
      # 
      # # Print dataset sizes after cleaning
      # cat("\nTrain data size after cleaning:", nrow(traindata_clean))
      # cat("\nTest data size after cleaning:", nrow(testdata_clean))

  ## 2. WRONG REGISTRY DUE TO DECIMAL POINT
      #It is common to have the wrong decimal point in some cases.
      #Check the row nearby
      #Manually change it like
      
  ## 3.Replace missing values with Mean or Median (bias)     

   ## 4.Use k-Nearest Neighbors (KNN) Imputation (complex situations)
    # library(DMwR)
    # combined_data <- knnImputation(combined_data, k = 5)  # k=5 nearest neighbors

  ## 4. TREATING as missing data, the wrong registries/outliers
    #preserves a lot of information 
     #4.1 Multiple Imputation (linear reggression )
        #weight -> continuous variable
  #apply NA to these values and multiple imputation
  #comments on MI
oldtestdata<-testdata
oldtraindata<-traindata

testdata[outliers_list$testdata$outlier_indices, "weight"] <- NA
traindata[outliers_list$traindata$outlier_indices, "weight"] <- NA
names(testdata)

library(mice)
md.pattern(testdata,plot = TRUE)
md.pattern(traindata,plot = TRUE)
# TARMOS FRAMEWORK - analysis how missing data is handled
  # Multiple Imputation on continuous variable -> linear regression
####################### 
# 2.Multiple Imputation for testdata
#impute the missing values
# pmm method used
imp <- mice(traindata[,c("weight","age","gender")], m = 3, print = FALSE, seed = 28)
imp

#check imputed values
imp$imp$weight

#Visualizing missing values for continuous variables
stripplot(imp, col=c("grey", "blue"), pch = c(1, 20))

#get complete data (2nd out of 3)
completeTrainData <- complete(imp,2)
summary(completeTrainData)

#building the model
m1.mi <- with(imp, lm(weight ~ age + gender))
summary(m1.mi$analyses[[2]])

t(sapply(m1.mi$analyses, coef))
#Multiple imputation with U and B
summary(pool(m1.mi), conf.int = TRUE)



# Apply multiple imputation on testdata
imp_test <- mice(testdata[, c("weight", "age", "gender")], m = 2, print = FALSE, seed = 28)

# Check the imputed values for weight
imp_test$imp$weight

# Visualize missing values in testdata
stripplot(imp_test, col = c("grey", "blue"), pch = c(1, 20))

# Extract a complete dataset (2nd imputed set) for testdata
completeTestData2 <- complete(imp_test, 2)
summary(completeTestData2)

# Build a model for testdata
m1_test.mi <- with(imp_test, lm(weight ~ age + gender))
summary(m1_test.mi$analyses[[2]])

# Extract coefficients from all imputed datasets
t(sapply(m1_test.mi$analyses, coef))

# Pool results from multiple imputations
summary(pool(m1_test.mi), conf.int = TRUE)

#Checking the distribution before and after
par(mfrow=c(1,2))  # Split plot area

hist(traindata$weight,
     main = "After Imputation",
     col = "blue",
     xlab = "Weight")

hist(completeTrainData$weight, 
     main = "Before Imputation",
     col = "red",
     xlab = "Weight")
mtext("Test Data - Weight ", side = 3, line = -1.5, outer = TRUE, font = 1, cex = 1.8)

par(mfrow=c(1,1))  # Reset plot area

par(mfrow = c(1, 2))  # Split plot area into 2 columns

# Histogram for testdata after imputation
hist(testdata$weight, 
     main = " After Imputation", 
     col = "blue", 
     xlab = "Weight", 
     ylab = "Frequency")

# Histogram for testdata before imputation
hist(completeTestData2$weight, 
     main = " Before Imputation", 
     col = "red", 
     xlab = "Weight", 
     ylab = "Frequency")
mtext("Training Data - Weight ", side = 3, line = -1.5, outer = TRUE, font = 1, cex = 1.8)

par(mfrow = c(1, 1))  # Reset plot area


# Replace missing values in testdata with imputed values
traindata[,"weight"] <-completeTrainData[,"weight"]
testdata[,"weight"]  <- completeTestData2[,"weight"]

# Summary of testdata after imputation
summary(traindata[,-1])
summary(testdata[,-1])

# Plot the distribution of weight in testdata
plot(traindata[,"weight"],ylab="Weight in TrainData")
plot(testdata[,"weight"],ylab="Weight in TestData")

# View detailed summary (excluding first column)
view(dfSummary(traindata[-1]))
view(dfSummary(testdata[-1]))



write.csv(traindata, "cleaned_traindata.csv", row.names = FALSE)
write.csv(testdata, "cleaned_testdata.csv", row.names = FALSE)


############# INSPECTION OF WBC_MAX

#The normal white blood cell (WBC)


#count in adults typically ranges from 4,500 to 11,000 cells per microliter 
#(4.5 to 11.0 × 10⁹/L)  #ref

#Hyperleukocytosis: Defined as a WBC count greater than 100,000 cells/µL, hyperleukocytosis is often associated with certain leukemias and other hematologic disorders.
#ref



boxplot(traindata$wbc_max, main = "Boxplot of wbc_max", col = "blue")
 plot(traindata$wbc_max)


 #Finding the outliers or wrongly inserted data
 datasets <- list(traindata = traindata, testdata = testdata)  
 outliers_list <- list()  # Initialize a list to store outlier indices
 
 upper_bound <- 95
 lower_bound<- 0
 
 
 for (name in names(datasets)) {
   data <- datasets[[name]] #extract dataset
   
   # Find outlier indices and values
   outlier_indices_max <- which(data$wbc_max > upper_bound)  
   outlier_values_max <- data$wbc_max[outlier_indices_max]  # Extract actual values
   
   outlier_indices_low <- which(data$wbc_max < lower_bound)  
   outlier_values_low <- data$wbc_max[outlier_indices_low]  # Extract actual values
   
   # Print results
   cat("\nDataset:", name, 
       "\nOutliers (>",upper_bound,") - Indices:", outlier_indices_max, 
       "\nOutliers (>",upper_bound,") - Values:", outlier_values_max, 
       "\nOutliers (<",lower_bound,") - Indices:", outlier_indices_low, 
       "\nOutliers (<",lower_bound,") - Values:", outlier_values_low, "\n")
   # Combine indices into a data frame
   outliers_df <- data.frame(
     dataset = name,
     outlier_indices = c(outlier_indices_max, outlier_indices_low),
     outlier_values = c(data$wbc_max[outlier_indices_max], data$wbc_max[outlier_indices_low])
   )
   
   # Store the data frame in the list
   outliers_list[[name]] <- outliers_df
 }


 # Combine all outlier data frames into one
 outliers_all <- do.call(rbind, outliers_list)
 cat("All outliers for variable wbc_max: ")
 print(outliers_all)  
 

 testdata[outliers_list$testdata$outlier_indices, "wbc_max"] <- NA
 traindata[outliers_list$traindata$outlier_indices, "wbc_max"] <- NA

 boxplot(traindata$wbc_max, main = "Boxplot of wbc_max", col = "blue")
 plot(traindata$wbc_max)
 
 md.pattern(testdata,plot = TRUE)
 md.pattern(traindata,plot = TRUE)


 cat("Unusual Registries of wbc_max are ",
     round((29/dim(alldata)[1])*100,3),"% of all data.")
 
 cat("We can set the unusual values of wbc_max to NA because they represent only 0.411% of the data, making them statistically insignificant. Given their rarity, these extreme values are likely outliers rather than meaningful trends, and removing them would minimize bias while preserving the integrity of the dataset.")


 
 ############# INSPECTION OF WBC_MIN
 
 #The normal white blood cell (WBC)
 
 
 #count in adults typically ranges from 4,500 to 11,000 cells per microliter 
 #(4.5 to 11.0 × 10⁹/L)  #ref
 
 #Hyperleukocytosis: Defined as a WBC count greater than 100,000 cells/µL, hyperleukocytosis is often associated with certain leukemias and other hematologic disorders.
 #ref
 
 
 
 boxplot(traindata$wbc_min, main = "Boxplot of wbc min", col = "blue")
 hist(traindata$wbc_min, main = "Histogram of wbc min", col = "blue")
 plot(traindata$wbc_min)
 
 
 #Finding the outliers or wrongly inserted data
 datasets <- list(traindata = traindata, testdata = testdata)  
 outliers_list <- list()  # Initialize a list to store outlier indices
 
 upper_bound <- 110
 lower_bound<- 0
 
 
 for (name in names(datasets)) {
   data <- datasets[[name]] #extract dataset
   
   # Find outlier indices and values
   outlier_indices_max <- which(data$wbc_min > upper_bound)  
   outlier_values_max <- data$wbc_min[outlier_indices_max]  # Extract actual values
   
   outlier_indices_low <- which(data$wbc_min < lower_bound)  
   outlier_values_low <- data$wbc_min[outlier_indices_low]  # Extract actual values
   
   # Print results
   cat("\nDataset:", name, 
       "\nOutliers (>",upper_bound,") - Indices:", outlier_indices_max, 
       "\nOutliers (>",upper_bound,") - Values:", outlier_values_max, 
       "\nOutliers (<",lower_bound,") - Indices:", outlier_indices_low, 
       "\nOutliers (<",lower_bound,") - Values:", outlier_values_low, "\n")
   # Combine indices into a data frame
   outliers_df <- data.frame(
     dataset = name,
     outlier_indices = c(outlier_indices_max, outlier_indices_low),
     outlier_values = c(data$wbc_min[outlier_indices_max], data$wbc_min[outlier_indices_low])
   )
   
   # Store the data frame in the list
   outliers_list[[name]] <- outliers_df
 }
 
 
 # Combine all outlier data frames into one
 outliers_all <- do.call(rbind, outliers_list)
 cat("All outliers for variable wbc_min: ")
 print(outliers_all)  
 
 
 testdata[outliers_list$testdata$outlier_indices, "wbc_min"] <- NA
 traindata[outliers_list$traindata$outlier_indices, "wbc_min"] <- NA
 
 boxplot(traindata$wbc_min, main = "Boxplot of wbc_min", col = "blue")
 plot(traindata$wbc_min)
 
 md.pattern(testdata,plot = TRUE)
 md.pattern(traindata,plot = TRUE)
 
 
 cat("Unusual Registries of wbc_min are ",
     round((30/dim(alldata)[1])*100,3),"% of all data.")
 
 cat("We can set the unusual values of wbc_min to NA because they represent only 0.411% of the data, making them statistically insignificant. Given their rarity, these extreme values are likely outliers rather than meaningful trends, and removing them would minimize bias while preserving the integrity of the dataset.")
 


 write.csv(traindata, "cleaned_traindata.csv", row.names = FALSE)
 write.csv(testdata, "cleaned_testdata.csv", row.names = FALSE)

























