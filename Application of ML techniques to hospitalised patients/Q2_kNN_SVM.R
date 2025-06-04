# Research Question 2 : Apply the k-Nearest Neighbors (kNN) classifier to assess the
# likelihood of mortality
# based on age, gender and peripheral oxygen saturation (spo2).

# Discuss the implications of precision, recall, and F1-score in a clinical setting. Could
# the SVM algorithm achieve better results?
#SVM NEEDS SCALING
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
packages <- c("car", "dplyr", "tidyverse", "summarytools","tidyr",
              "mice","ggplot2","MLmetrics","pROC","vtable","ggpubr",
              "outliers",
              "GGally",
              "class","e1071")
load_packages(packages)

#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)
#keep the data we need for the models only
names(traindata)
variable_needed <-c("age","gender","spo2","death")

traindata <- traindata[,variable_needed]
testdata <- testdata[,variable_needed]
alldata <- rbind(traindata,testdata)
sumtable(alldata[variable_needed],labels=labs,align = 'p{.3\\textwidth}ccccccc', fit.page = '\\textwidth', out = 'latex')


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
 
 define_keywords(title.dfSummary = "Data Frame Summary in PDF Format")
 dfSummary(alldata)

################## Create pairwise plots
ggpairs(traindata, aes(color = death, alpha = 0.5))
ggpairs(testdata,aes(color = death, alpha = 0.5))

#Surely there are many extreme outliers 
#from both medical view of the metrics that Sp02 can take Hypoxemia<60mm Hg Domain-Specific Thresholds
#https://www.mayoclinic.org/symptoms/hypoxemia/basics/definition/sym-20050930
#and for a statistical stand point IQR
grubbs.test(traindata$spo2)
grubbs.test(testdata$spo2)
# Calculate the IQR for 'spo2'
IQR(traindata$spo2)
IQR(testdata$spo2)

Q1 <- quantile(traindata$spo2, 0.25)
Q3 <- quantile(traindata$spo2, 0.75)
IQR_value <- IQR(traindata$spo2)

# Define the lower bound (anything below this is an outlier)
lower_bound <- Q1 - 4 * IQR_value 
lower_bound <- 30#reference
cat("Sp02 lower bound due to extreme values and domain details.Our lower bound is: ",lower_bound)
# Filter out rows where 'spo2' is below the lower bound
traindata_filtered <- traindata[traindata$spo2 >= lower_bound, ]
testdata_filtered <- testdata[testdata$spo2 >= lower_bound, ]

cat("Train Removing %",100-(dim(traindata_filtered)[1]/dim(traindata)[1])*100)
cat("From all data Removing %",100-((dim(testdata_filtered)[1]+dim(traindata_filtered)[1])/(dim(testdata)[1]+dim(traindata)[1]))*100)


# Rename columns for better readability
#colnames(traindata_filtered) <- c("Age", "Gender", "SpO2 Level", "Death Outcome")
#colnames(testdata_filtered) <- c("Age", "Gender", "SpO2 Level", "Death Outcome")
ggpairs(traindata_filtered, aes(color =`Death Outcome`, alpha = 0.5))
ggpairs(testdata_filtered,aes(color = `Death Outcome`, alpha = 0.5))

# View the filtered data
head(traindata_filtered)
plot(traindata_filtered$spo2)
grubbs.test(traindata_filtered$spo2)#disadvatages : assumes normality,only one outlier,sensitive to sample size,possible loss of important data

# The term "SpO2" refers to peripheral capillary oxygen saturation, which is a measure of the amount of oxygenated hemoglobin (oxygen-carrying molecules in the blood) in the blood. It is often measured using a device called a pulse oximeter.
# 
# An SpO2 value of 11 is extremely low and not typical in any healthy individual. The SpO2 value is usually expressed as a percentage, where a normal range is typically between 95% and 100% for a healthy person. If someone has an SpO2 value as low as 11%, it indicates severe hypoxemia, meaning their blood oxygen levels are dangerously low and they are at risk of organ damage or failure.
# 
# In medical practice, values below 90% are often considered critical, and levels as low as 80% or lower generally require immediate intervention. 11% is an emergency situation and would be life-threatening.
# 
# If this value appears in your data, it may either be an error, an outlier, or there may be a very specific and serious medical condition being referenced. For instance, in the context of medical data, it could be due to a measurement error or miscalculation, or it might represent the data from a very severe case.

traindata<-traindata_filtered 
testdata<-testdata_filtered 
#colnames(traindata) <- c("Age", "Gender", "SpO2 Level", "Death Outcome")
alldata = rbind(traindata,testdata)
colnames(alldata) <- c("Age", "Gender", "SpO2 Level", "Death Outcome")
#colnames(testdata_filtered) <- c("Age", "Gender", "SpO2 Level", "Death Outcome")
ggpairs(alldata, aes(color =`Death Outcome`, alpha = 0.5))
st(alldata)

variable_needed

xtrain = traindata[,c( "age"   , "gender" ,"spo2")]
ytrain = traindata[,"death"]
xtest = testdata[,c("age"    ,"gender" ,"spo2"  )]
ytest = testdata[,"death"]
#knn cannot take factor variables , we could make it numeric and scale/ normalise
# the other variables, so euclidean distance can make more sense
#Note: scaling works better if they follow the normal distribution
# Apply the model to the test data
xtrain$gender <- scale(as.numeric(as.factor(xtrain$gender)))
xtest$gender <- scale(as.numeric(as.factor(xtest$gender)))
xtrain[, c("age", "spo2")] <- scale(xtrain[, c("age", "spo2")])
xtest[, c("age", "spo2")] <- scale(xtest[, c("age", "spo2")])
#plot(xtrain, col = ytrain, pch = c("o","+")[ytrain])

pred = knn(xtrain, xtest, ytrain, k = 9)
ConfusionMatrix(pred, ytest)
Accuracy(ytest, pred)

Precision(ytest, pred, "YES")
Recall(ytest, pred, "YES")
F1_Score(ytest, pred, "YES")

Precision(ytest, pred, "NO")
Recall(ytest, pred, "NO")
F1_Score(ytest, pred, "NO")



# Initialize k and accuracy storage
k_values <- c()  # Vector to store k values
accuracies <- c()  # Vector to store corresponding accuracies
f1_scores <- c()  # Vector to store corresponding F1 scores
f1_scoresNO <-c() #f1 no
k = 1
while (k<20) {
 
  k=k+2
  pred = knn(xtrain, xtest, ytrain, k = k)
  
  
  acc<-Accuracy(pred,ytest)
  f1<-F1_Score(ytest, pred, "YES")
  f2<-F1_Score(ytest, pred, "NO")
  cat("Acc = ",acc,"F1 (yes)=",f1,"F1 (No)=",f2, " k = ",k,"\n")
 
  k_values <- c(k_values, k)
  accuracies <-c(accuracies,acc)
  f1_scores<-c(f1_scores,f1)
  f1_scoresNO<-c(f1_scoresNO,f2)

  
}

# Plot accuracy over different values of k
accuracy_plot <- ggplot(data = data.frame(k_values, accuracies), aes(x = k_values, y = accuracies)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Accuracy vs K Value in KNN", x = "K Value", y = "Accuracy") +
  theme_minimal()

# Display the plot
print(accuracy_plot)

# Plot F1 Score over different values of k
f1_plot <- ggplot(data = data.frame(k_values, f1_scores), aes(x = k_values, y = f1_scores)) +
  geom_line(color = "green") +
  geom_point(color = "orange") +
  labs(title = "F1 Score (Yes) vs K Value in KNN", x = "K Value", y = "F1 Score") +
  theme_minimal()

# Display the F1 Score plot
print(f1_plot)
# Combine accuracy and F1 score into one data frame
combined_data <- data.frame(
  k_values = rep(k_values, 3),  # Duplicate k_values for each metric
  value = c(accuracies, f1_scores,f1_scoresNO),  # Combine accuracies and f1_scores
  metric = rep(c("Accuracy", "F1 Score (Yes)", "F1 Score (No)"), each = length(k_values))  # Add a column for metric types
)

# Plot both accuracy and F1 score on the same plot with lines
combined_plot <- ggplot(combined_data, aes(x = k_values, y = value, color = metric, group = metric)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Points on the lines
  labs(title = "Accuracy and F1 Score vs K Value for k-NN", x = "K Value", y = "Value") +
  scale_color_manual(values = c("blue", "green","red")) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title

# Display the combined plot
print(combined_plot)
############################################ optimised code for knn grid search and documentation

library(patchwork)  # For combining plots

# Initialize parameter grid - odd k values from 3 to 21
k_grid <- seq(3, 21, by = 2)  

# Create results dataframe with all metrics
results <- data.frame(
  k = k_grid,
  Accuracy = numeric(length(k_grid)),
  Precision_YES = numeric(length(k_grid)),
  Recall_YES = numeric(length(k_grid)),
  F1_YES = numeric(length(k_grid)),
  Precision_NO = numeric(length(k_grid)),
  Recall_NO = numeric(length(k_grid)),
  F1_NO = numeric(length(k_grid))
)

# Evaluate k-NN for each k value
for (i in seq_along(k_grid)) {
  k <- k_grid[i]
  pred <- knn(train = xtrain, test = xtest, cl = ytrain, k = k, prob = TRUE)
  
  # Calculate all metrics
  results$Accuracy[i] <- Accuracy(pred, ytest)
  results$Precision_YES[i] <- Precision(ytest, pred, "YES")
  results$Recall_YES[i] <- Recall(ytest, pred, "YES")
  results$F1_YES[i] <- F1_Score(ytest, pred, "YES")
  results$Precision_NO[i] <- Precision(ytest, pred, "NO")
  results$Recall_NO[i] <- Recall(ytest, pred, "NO")
  results$F1_NO[i] <- F1_Score(ytest, pred, "NO")
  
  # Print progress
  cat(sprintf(
    "k = %2d | Acc = %.3f | Prec(YES) = %.3f | Rec(YES) = %.3f | F1(YES) = %.3f\n",
    k, results$Accuracy[i], results$Precision_YES[i], 
    results$Recall_YES[i], results$F1_YES[i]
  ))
}

# Reshape data for visualization
plot_data <- results %>%
  pivot_longer(cols = -k, names_to = "Metric", values_to = "Value") %>%
  mutate(
    Class = ifelse(grepl("_YES", Metric), "YES", "NO"),
    Metric_Type = gsub("_YES|_NO", "", Metric)
  )

# Create individual plots
accuracy_plot <- ggplot(filter(plot_data, Metric == "Accuracy"), 
                        aes(x = k, y = Value)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  geom_point(color = "#1f77b4", size = 3) +
  labs(title = "Accuracy", y = "Value") +
  theme_minimal()

precision_recall_plot <- ggplot(filter(plot_data, Metric_Type %in% c("Precision", "Recall")), 
                                aes(x = k, y = Value, color = Metric_Type, linetype = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Precision" = "#ff7f0e", "Recall" = "#2ca02c")) +
  labs(title = "Precision & Recall by Class", y = "Value", color = "Metric") +
  theme_minimal() +
  theme(legend.position = "top")

f1_plot <- ggplot(filter(plot_data, Metric == "F1_YES" | Metric == "F1_NO"), 
                  aes(x = k, y = Value, color = Class)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("YES" = "#d62728", "NO" = "#9467bd")) +
  labs(title = "F1 Scores by Class", y = "Value") +
  theme_minimal()

# Combine plots
combined_plot <- (accuracy_plot | (precision_recall_plot / f1_plot)) +
  plot_annotation(
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
                  
                  # Display combined plot
                  print(combined_plot)
                  
                  # Find optimal k based on composite score
                  optimal_k <- results %>%
                    mutate(Composite_Score = 0.4*Accuracy + 0.3*F1_YES + 0.2*Recall_YES + 0.1*Precision_YES) %>%
                    arrange(desc(Composite_Score)) %>%
                    slice(1)
                  
                  # Print optimal parameters
                  cat("\nOptimal Parameters:\n")
                  cat(sprintf("k = %d\nAccuracy: %.3f\nPrecision (YES): %.3f\nRecall (YES): %.3f\nF1 (YES): %.3f\nF1 (NO): %.3f",
                              optimal_k$k, optimal_k$Accuracy, optimal_k$Precision_YES,
                              optimal_k$Recall_YES, optimal_k$F1_YES, optimal_k$F1_NO))
#################################
# 
dev.off()
#optimal model KNN
pred = knn(xtrain, xtest, ytrain, k = 9)
ConfusionMatrix(pred, ytest)
Accuracy(ytest, pred)

Precision(ytest, pred, "YES")
Recall(ytest, pred, "YES")
F1_Score(ytest, pred, "YES")

Precision(ytest, pred, "NO")
Recall(ytest, pred, "NO")
F1_Score(ytest, pred, "NO")

# Initialize an empty data frame to store results
results <- data.frame(
  K = integer(),
  Accuracy = numeric(),
  Precision_Yes = numeric(),
  Recall_Yes = numeric(),
  F1_Yes = numeric(),
  Precision_No = numeric(),
  Recall_No = numeric(),
  F1_No = numeric()
)

# Loop over k values
for (k in c(3, 5, 7,9,11,13)) {
  # Apply KNN
  pred <- knn(xtrain, xtest, ytrain, k = k)
  
  # Compute performance metrics
  acc <- Accuracy(ytest, pred)
  prec_yes <- Precision(ytest, pred, "YES")
  recall_yes <- Recall(ytest, pred, "YES")
  f1_yes <- F1_Score(ytest, pred, "YES")
  
  prec_no <- Precision(ytest, pred, "NO")
  recall_no <- Recall(ytest, pred, "NO")
  f1_no <- F1_Score(ytest, pred, "NO")
  
  # Store results in the data frame
  results <- rbind(results, data.frame(
    K = k, 
    Accuracy = acc,
    Precision_Yes = prec_yes, Recall_Yes = recall_yes, F1_Yes = f1_yes,
    Precision_No = prec_no, Recall_No = recall_no, F1_No = f1_no
  ))
}



# Define custom theme with alternating row colors
custom_theme <- ttheme(
  colnames.style = list(fill = "lightgray", color = "black", face = "bold"),
  tbody.style = list(fill = c("white", "#f0f0f0"))  # Alternating rows
)
results <- as.data.frame(lapply(results, function(x) if(is.numeric(x)) round(x, 3) else x))

# Create the table with custom styling
colnames(results)<-c("K value","Accuracy","Precision (Yes)","Recall (Yes)","F1 Score (Yes)","Precision (No)","Recall (No)","F1 Score (No)")

table_plot <- ggtexttable(results, rows = NULL, theme = custom_theme)
# Display the table
print(table_plot)
#optimal k = 9


########### SVM
#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)
traindata=traindata[,c( "age"   , "gender" ,"spo2","death")]
testdata=testdata[,c( "age"   , "gender" ,"spo2","death")]
#having the modified scaled data
xtrain = traindata[,c( "age"   , "gender" ,"spo2")]
ytrain = traindata[,"death"]
xtest = testdata[,c("age"    ,"gender" ,"spo2"  )]
ytest = testdata[,"death"]

#xtrain$gender <- scale(as.numeric(as.factor(xtrain$gender)))
#xtest$gender <- scale(as.numeric(as.factor(xtest$gender)))
#xtrain[, c("age", "spo2")] <- scale(xtrain[, c("age", "spo2")])
#xtest[, c("age", "spo2")] <- scale(xtest[, c("age", "spo2")])







# Plot a contour for the decision function of the SVM with gamma = 1, gamma = 2σ^2
svm_model = svm(death~ age + gender + spo2,
                kernel="radial", 
                type="C-classification",
                data = traindata,
                gamma = 0.01,
                probability = TRUE)
svm_model


pred = predict(svm_model, xtest,probability = TRUE)

Accuracy(pred,ytest)

# Set of gamma values
gammavalues = c(0.01, 0.1, 1,10,100)

# Calculate the training error
training_error = c()
for (gamma in gammavalues) {
  svm_model = svm(death ~ age + gender + spo2, kernel="radial",
                  type="C-classification", data = traindata, gamma = gamma)
  pred = predict(svm_model, xtrain)
  training_error = c(training_error, 1 - Accuracy(pred, ytrain))
}

# Calculate the testing error
testing_error = c()
for (gamma in gammavalues) {
  svm_model = svm(death~ age + gender + spo2, kernel="radial", type="C-classification",
                  data = traindata, gamma = gamma)
  pred = predict(svm_model, xtest)
  testing_error = c(testing_error, 1 - Accuracy(pred, ytest))
}

# Plot the training error and the testing error
plot(training_error, type = "l", col="blue",
     ylim = c(0, 0.5), xlab = "Gamma", ylab = "Error", xaxt = "n")
axis(1, at = 1:length(gammavalues), labels = gammavalues)
lines(testing_error, col="red")
legend("bottomleft", c("Training Error", "Testing Error"), pch = c("-","-"),  col = c("blue", "red"))

names(traindata)
###########################
# Load required libraries
library(e1071)
library(MLmetrics)
library(ggplot2)

# Define gamma values on a logarithmic scale
gamma_values <- 10^seq(-2, 2, by = 1)  # 0.01, 0.1, 1, 10, 100

# Pre-allocate error vectors
train_errors <- numeric(length(gamma_values))
test_errors <- numeric(length(gamma_values))
f1_scores <- numeric(length(gamma_values))

# Evaluate models for each gamma
for (i in seq_along(gamma_values)) {
  gamma <- gamma_values[i]
  
  # Train model
  svm_model <- svm(
    death ~ age + gender + spo2,
    kernel = "radial",
    type = "C-classification",
    data = traindata,
    gamma = gamma,
    probability = TRUE
  )
  
  # Training performance
  train_pred <- predict(svm_model, xtrain)
  train_errors[i] <- 1 - Accuracy(train_pred, ytrain)
  
  # Testing performance
  test_pred <- predict(svm_model, xtest)
  test_errors[i] <- 1 - Accuracy(test_pred, ytest)
  f1_scores[i] <- F1_Score(ytest, test_pred, "YES")
  
  cat(sprintf("Gamma: %.2f | Train Error: %.3f | Test Error: %.3f | F1-Score: %.3f\n",
              gamma, train_errors[i], test_errors[i], f1_scores[i]))
}

# Create comprehensive results data frame
results <- data.frame(
  Gamma = gamma_values,
  Train_Error = train_errors,
  Test_Error = test_errors,
  F1_Score = f1_scores
)

# Find optimal gamma (minimum test error)
optimal_gamma <- gamma_values[which.min(test_errors)]

# Enhanced visualization
ggplot(results, aes(x = factor(Gamma))) +
  geom_line(aes(y = Train_Error, color = "Training Error", group = 1), linewidth = 1.2) +
  geom_point(aes(y = Train_Error, color = "Training Error"), linewidth = 3) +
  geom_line(aes(y = Test_Error, color = "Testing Error", group = 1), linewidth = 1.2) +
  geom_point(aes(y = Test_Error, color = "Testing Error"), linewidth = 3) +
  geom_line(aes(y = F1_Score/2, color = "F1-Score (scaled)", group = 1), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = F1_Score/2, color = "F1-Score (scaled)"), linewidth = 3) +
  scale_y_continuous(
    name = "Error Rate",
    sec.axis = sec_axis(~.*2, name = "F1-Score")
  ) +
  scale_color_manual(
    values = c("Training Error" = "blue", "Testing Error" = "red", "F1-Score (scaled)" = "darkgreen"),
    name = "Metric"
  ) +
  labs(
    title = "SVM Performance Across Gamma Values",
    x = "Gamma (RBF Kernel Parameter)",
    caption = sprintf("Optimal Gamma: %.2f (Test Error: %.3f, F1: %.3f)", 
                      optimal_gamma, min(test_errors), max(f1_scores))
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "darkgreen"),
    plot.caption = element_text(face = "bold")
  )


#########################
# Apply 10-fold cross validation to find the best value for gamma
k = 10
dim(alldata)
dim(traindata)
# Split in 10 folds
dsize = nrow(traindata)
set.seed(28); folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
for (gamma in gammavalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select 9 out of 10 folds for training and 1 for validation
    trainingset <- traindata[unlist(folds[-i]),]
    validationset <- traindata[unlist(folds[i]),]
    # Train and apply the model
    svm_model = svm(death~ age + gender + spo2, kernel="radial", 
                    type="C-classification", data = trainingset, gamma = gamma)
    pred = predict(svm_model, validationset[, c("age","gender" ,"spo2" )])
    # Save predictions and testsets
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(validationset[,"death"]))
  }
  # Calculate the new accuracy and add it to the previous ones
  accuracies = c(accuracies, Accuracy(predictions, testsets))
}
# Find the best gamma value
print(accuracies)
bestgamma = gammavalues[which.max(accuracies)]

# Plot the accuracy for each gamma value
plot(accuracies, type = "l", col="blue", ylim = c(0.5, 1.0), xlab = "Gamma", ylab = "Accuracy", xaxt = "n", main = "Cross-Validation Accuracy")
axis(1, at = 1:length(gammavalues), labels = gammavalues)



###########################
# library(foreach)  # For parallel processing
# library(doParallel) # For parallel backend
# 
# # Set up parallel processing
# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl)

# Define gamma values on a logarithmic scale
gamma_values <- 10^seq(-2, 2, by = 1)  # 0.01, 0.1, 1, 10, 100
k <- 10  # Number of folds
set.seed(28)  # For reproducibility

# Create balanced folds preserving class distribution
folds <- caret::createFolds(traindata$death, k = k, list = TRUE)

# Initialize results dataframe
cv_results <- data.frame(
  gamma = gamma_values,
  accuracy = numeric(length(gamma_values)),
  accuracy_sd = numeric(length(gamma_values)),
  f1_score = numeric(length(gamma_values)),
  f1_sd = numeric(length(gamma_values))
)

# Perform cross-validation
for (g in seq_along(gamma_values)) {
  gamma <- gamma_values[g]
  fold_acc <- numeric(k)
  fold_f1 <- numeric(k)
  
  for (i in 1:k) {
    # Create training and validation sets
    train_idx <- unlist(folds[-i])
    valid_idx <- unlist(folds[i])
    
    # Train SVM model
    model <- svm(
      death ~ age + gender + spo2,
      kernel = "radial",
      type = "C-classification",
      data = traindata[train_idx, ],
      gamma = gamma,
      probability = TRUE
    )
    
    # Make predictions
    pred <- predict(model, traindata[valid_idx, c("age", "gender", "spo2")])
    true <- traindata[valid_idx, "death"]
    
    # Store metrics
    fold_acc[i] <- Accuracy(pred, true)
    fold_f1[i] <- F1_Score(true, pred, "YES")
  }
  
  # Store results
  cv_results$accuracy[g] <- mean(fold_acc)
  cv_results$accuracy_sd[g] <- sd(fold_acc)
  cv_results$f1_score[g] <- mean(fold_f1)
  cv_results$f1_sd[g] <- sd(fold_f1)
  
  cat(sprintf("Gamma: %.2f | Accuracy: %.3f (±%.3f) | F1: %.3f (±%.3f)\n",
              gamma, cv_results$accuracy[g], cv_results$accuracy_sd[g],
              cv_results$f1_score[g], cv_results$f1_sd[g]))
}

# Find optimal gamma (maximizing both accuracy and F1)
optimal_gamma <- cv_results %>%
  mutate(combined_score = 0.6 * accuracy + 0.4 * f1_score) %>%
  arrange(desc(combined_score)) %>%
  slice(1)

# Enhanced visualization
ggplot(cv_results, aes(x = factor(gamma))) +
  geom_errorbar(aes(ymin = accuracy - accuracy_sd, 
                    ymax = accuracy + accuracy_sd, 
                    color = "Accuracy"), 
                width = 0.2, size = 0.8) +
  geom_line(aes(y = accuracy, group = 1, color = "Accuracy"), size = 1.2) +
  geom_point(aes(y = accuracy, color = "Accuracy"), size = 3) +
  geom_errorbar(aes(ymin = f1_score - f1_sd, 
                    ymax = f1_score + f1_sd, 
                    color = "F1 Score"), 
                width = 0.2, size = 0.8) +
  geom_line(aes(y = f1_score, group = 1, color = "F1 Score"), 
            size = 1.2, linetype = "dashed") +
  geom_point(aes(y = f1_score, color = "F1 Score"), size = 3) +
  scale_color_manual(values = c("Accuracy" = "#1f77b4", "F1 Score" = "#ff7f0e")) +
  labs(title = "10-Fold Cross-Validation Performance",
       subtitle = paste("Optimal gamma:", optimal_gamma$gamma, 
                        "(Accuracy:", round(optimal_gamma$accuracy, 3), 
                        "| F1:", round(optimal_gamma$f1_score, 3), ")"),
       x = "Gamma Value (RBF Kernel)",
       y = "Metric Value",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# Print results
print(cv_results)
cat("\nOptimal Gamma Value:", optimal_gamma$gamma, "\n")


# Plot a contour for the decision function of the SVM with gamma = 1, gamma = 2σ^2
svm_model = svm(death~ age + gender + spo2,
                kernel="radial", 
                type="C-classification",
                data = traindata,
                gamma = 10,
                probability = TRUE)
svm_model


pred = predict(svm_model, xtest,probability = TRUE)

Accuracy(pred,ytest)

ConfusionMatrix(pred, ytest)
Accuracy(ytest, pred)

Precision(ytest, pred, "YES")
Recall(ytest, pred, "YES")
F1_Score(ytest, pred, "YES")

Precision(ytest, pred, "NO")
Recall(ytest, pred, "NO")
F1_Score(ytest, pred, "NO")


############for latex table
#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)
xtrain = traindata[,c( "age"   , "gender" ,"spo2")]
ytrain = traindata[,"death"]
xtest = testdata[,c("age"    ,"gender" ,"spo2"  )]
ytest = testdata[,"death"]
xtrain$gender <- scale(as.numeric(as.factor(xtrain$gender)))
xtest$gender <- scale(as.numeric(as.factor(xtest$gender)))
xtrain[, c("age", "spo2")] <- scale(xtrain[, c("age", "spo2")])
xtest[, c("age", "spo2")] <- scale(xtest[, c("age", "spo2")])
predknn<-knn(xtrain, xtest, ytrain, k = 9)


traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)

traindata=traindata[,c( "age"   , "gender" ,"spo2","death")]
testdata=testdata[,c( "age"   , "gender" ,"spo2","death")]

xtrain = traindata[,c( "age"   , "gender" ,"spo2")]
ytrain = traindata[,"death"]
xtest = testdata[,c("age"    ,"gender" ,"spo2"  )]
ytest = testdata[,"death"]



# Define model names
SVM01 <- svm_model
#kNN9 <-
model_names <- c("kNN9", "SVM01")

# Initialize an empty list to store results
metrics_list <- list()

# Loop through each model
for (model_name in model_names) {
  
  # Apply the model
  if (model_name == "kNN9") {
    pred <- predknn
  } else {
    pred <- predict(get(model_name), xtest, type = "class")
  }
  
  # Compute metrics
  acc <- Accuracy(ytest, pred)
  pr_Y <- Precision(ytest, pred, "YES")
  recall_Y <- Recall(ytest, pred, "YES")
  F1_Y <- F1_Score(ytest, pred, "YES")
  pr_N <- Precision(ytest, pred, "NO")
  recall_N <- Recall(ytest, pred, "NO")
  F1_N <- F1_Score(ytest, pred, "NO")
  
  # Store results in a list
  metrics_list[[model_name]] <- c(
    Accuracy = acc,
    Precision_YES = pr_Y,
    Recall_YES = recall_Y,
    F1_YES = F1_Y,
    Precision_NO = pr_N,
    Recall_NO = recall_N,
    F1_NO = F1_N
  )
}

# Convert list to dataframe
metrics_df <- as.data.frame(metrics_list)

# Print the metrics dataframe
print(metrics_df)
# Round all numeric values to 3 decimal places
metrics_df <- as.data.frame(lapply(metrics_df, function(x) if(is.numeric(x)) round(x, 3) else x))
rownames(metrics_df) <- c("Accuracy ", "Precision (Yes)", "Recall (Yes)", "F1 Score (Yes)"
                          , "Precision (No)", "Recall (No)", "F1 Score (No)")  # Customize names
metrics_df <- cbind(Metrics = rownames(metrics_df), metrics_df)
rownames(metrics_df) <- NULL  # Remove row names

# Modify the column names
colnames(metrics_df) <- c("Metrics", "kNN (k = 9)", "SVM (gamma = 10)")  


# Define custom theme with alternating row colors
custom_theme <- ttheme(
  colnames.style = list(fill = "lightgray", color = "black", face = "bold"),
  tbody.style = list(fill = c("white", "#f0f0f0"))  # Alternating rows
)

# Create the table with custom styling
table_plot <- ggtexttable(metrics_df, rows = NULL, theme = custom_theme)

# Display the table
print(table_plot)

################## ROC CURVES

# Define models and names (assuming predknn and svm_model exist)
models <- list(
  "kNN (k=9)" = list(pred = predknn, prob = NULL),  # Add probability predictions if available
  "SVM (γ=10)" = list(
    pred = predict(svm_model, xtest, type = "class"),
    prob = predict(svm_model, xtest, probability = TRUE)
  )
)

# Calculate all metrics for each model
metrics <- lapply(models, function(m) {
  pred <- m$pred
  list(
    Accuracy = Accuracy(ytest, pred),
    Precision = c(
      "YES" = Precision(ytest, pred, "YES"),
      "NO" = Precision(ytest, pred, "NO")
    ),
    Recall = c(
      "YES" = Recall(ytest, pred, "YES"),
      "NO" = Recall(ytest, pred, "NO")
    ),
    F1 = c(
      "YES" = F1_Score(ytest, pred, "YES"),
      "NO" = F1_Score(ytest, pred, "NO")
    ),
    AUC = if(!is.null(m$prob)) {
      roc(ytest, attr(m$prob, "probabilities")[,"YES"])$auc
    } else NA
  )
})

# Create comprehensive visualization
plot_metrics <- function(metrics) {
  # Prepare data for plotting
  plot_data <- data.frame(
    Model = rep(names(metrics), each = 7),
    Metric = rep(c("Accuracy", "Precision (YES)", "Recall (YES)", "F1 (YES)", 
                   "Precision (NO)", "Recall (NO)", "F1 (NO)"), length(metrics)),
    Value = unlist(lapply(metrics, function(x) c(
      x$Accuracy, x$Precision["YES"], x$Recall["YES"], x$F1["YES"],
      x$Precision["NO"], x$Recall["NO"], x$F1["NO"]
    )))
  )
  
  # Add AUC separately if available
  auc_data <- data.frame(
    Model = names(metrics),
    AUC = sapply(metrics, function(x) ifelse(is.na(x$AUC), NA, x$AUC))
  )
  
  # Create metric comparison plot
  p1 <- ggplot(plot_data, aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(Value, 3)), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 3) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Model Performance Comparison",
         y = "Score", x = "") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Create ROC curve plot if AUC available
  if(any(!is.na(auc_data$AUC))) {
    roc_curves <- lapply(names(models), function(name) {
      if(!is.null(models[[name]]$prob)) {
        roc(ytest, attr(models[[name]]$prob, "probabilities")[,"YES"])
      } else NULL
    })
    names(roc_curves) <- names(models)
    
    roc_data <- do.call(rbind, lapply(names(roc_curves), function(name) {
      if(!is.null(roc_curves[[name]])) {
        data.frame(
          Model = name,
          FPR = 1 - roc_curves[[name]]$specificities,
          TPR = roc_curves[[name]]$sensitivities
        )
      }
    }))
    
    p2 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
      geom_line(size = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_color_brewer(palette = "Set1") +
      labs(title = "ROC Curves",
           x = "False Positive Rate",
           y = "True Positive Rate") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Combine plots
    ggarrange(p1, p2, ncol = 2, widths = c(1.5, 1))
  } else {
    p1  # Return just metrics plot if no ROC data
  }
}

# Generate metrics table
metrics_table <- do.call(rbind, lapply(names(metrics), function(name) {
  data.frame(
    Model = name,
    Accuracy = round(metrics[[name]]$Accuracy, 3),
    Precision_YES = round(metrics[[name]]$Precision["YES"], 3),
    Recall_YES = round(metrics[[name]]$Recall["YES"], 3),
    F1_YES = round(metrics[[name]]$F1["YES"], 3),
    Precision_NO = round(metrics[[name]]$Precision["NO"], 3),
    Recall_NO = round(metrics[[name]]$Recall["NO"], 3),
    F1_NO = round(metrics[[name]]$F1["NO"], 3),
    AUC = ifelse(is.na(metrics[[name]]$AUC), NA, round(metrics[[name]]$AUC, 3))
  )
}))

# Create publication-quality table
table_plot <- ggtextable(
  metrics_table,
  rows = NULL,
  theme = ttheme(
    colnames.style = list(
      fill = "#2C3E50", 
      color = "white", 
      face = "bold",
      size = 10
    ),
    tbody.style = list(
      fill = c("white", "#F7F7F7"),
      size = 9
    )
  )
) %>%
  tab_add_title(text = "Model Performance Metrics", face = "bold", size = 12)

# Display all results
print(plot_metrics(metrics))
print(table_plot)

# Identify best model (highest average of Accuracy and F1-YES)
metrics_table$Composite_Score <- 
  (metrics_table$Accuracy + metrics_table$F1_YES) / 2
best_model <- metrics_table[which.max(metrics_table$Composite_Score), ]

cat("\nBest Performing Model:\n")
cat("----------------------\n")
cat(sprintf("%s\n\n", best_model$Model))
cat(sprintf("Accuracy: %.3f\n", best_model$Accuracy))
cat(sprintf("F1 Score (YES): %.3f\n", best_model$F1_YES))
if(!is.na(best_model$AUC)) cat(sprintf("AUC: %.3f\n", best_model$AUC))





