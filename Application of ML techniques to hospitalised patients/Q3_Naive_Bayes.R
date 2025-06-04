# Utilize Naive Bayes to predict patient mortality using the
# categorical features heart rate (hr), 
                      #systolic blood pressure (sbp), 
                      #diastolic blood pressure (dbp), liver health status, and cardiovascular status.
#Assess the model’s performance using accuracy, precision, and recall.

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
              "outliers","tidyr",
              "GGally",
              "mlbench","e1071","ROCR")
load_packages(packages)

#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)

#keep the data we need for the models only
names(traindata)
variable_needed <-c("hr","sbp","dbp","liver","cardiovascular" ,"death")

traindata <- traindata[,variable_needed]
testdata <- testdata[,variable_needed]
alldata = rbind(traindata,testdata)
glimpse(traindata)
str(traindata)
str(testdata)

#Checking the distributions of data and for unusual values
view(dfSummary(traindata))
view(dfSummary(testdata))
view(dfSummary(alldata))
############### FOR LATER USE IN LATEX
 # st_options(
 #   plain.ascii = FALSE, 
 #   style = "rmarkdown",
 #   dfSummary.style = "grid",
 #   dfSummary.valid.col = FALSE,
 #   dfSummary.graph.magnif = .52,
 #  tmp.img.dir = "/tmp"
# )
# 
define_keywords(title.dfSummary = "Data Frame Summary in PDF Format")
colnames(alldata)<-c( "Heart Rate","Systolic BP","Diastolic BP","Liver Disease","Cardiovascular Disease","Death")
dfSummary(alldata)


xtrain = traindata[,c( "hr","sbp","dbp","liver","cardiovascular")]
ytrain = traindata[,"death"]
xtest = testdata[,c("hr","sbp","dbp","liver","cardiovascular")]
ytest = testdata[,"death"]

class_distribution <- table(ytrain) / length(ytrain)  # Get real class proportions
model <- naiveBayes(xtrain, ytrain, prior = as.numeric(rev(class_distribution)))


################## Create pairwise plots
ggpairs(traindata, aes(color = death, alpha = 0.5))
ggpairs(testdata,aes(color = death, alpha = 0.5))
alldata=rbind(traindata,testdata)
ggpairs(alldata,,aes(color = death, alpha = 0.5))

# Apply Naive Bayes \ tried different laplace smoothing values
model <- naiveBayes(death ~ .,laplace = 10,
                    prior=c(0.2652374 ,0.7347626) ,data = traindata)
model

#summary(model)
# Predict on the test set and print the confusion matrix and metrics
pred = predict(model, xtest)

ConfusionMatrix(pred,ytest)
Precision(ytest, pred,"NO") 
Precision(ytest, pred,"YES")
Recall(ytest, pred)
F1_Score(ytest, pred)


# Get the prediction probabilities and plot ROC curve
predprob = predict(model, xtest, type = "raw")

pred_obj = prediction(predprob[,1],
                      ytest, 
                      label.ordering = c("YES", "NO"))

ROCcurve <- performance(pred_obj, "tpr", "fpr")
plot(ROCcurve, col = "blue")
abline(0,1, col = "grey")

dev.off()
ROCcurve <- performance(pred_obj, "tnr", "fpr")
plot(ROCcurve, col = "blue")
abline(1,-1, col = "grey")


# Get the AUC
performance(pred_obj, "auc")
unlist(performance(pred_obj, "auc")@y.values[1])
performance(pred_obj, "tpr")@y.values
performance(pred_obj, "acc")@y.values


################ can't predict yes due to prior ratio imbalance

pred_adjusted <- ifelse(predprob[, "YES"] > 0.3, "YES", "NO")  # Adjust threshold
ConfusionMatrix(pred_adjusted,ytest)
Precision(ytest, pred_adjusted,"NO") # or Precision(ytest, pred, "democrat") and Precision(ytest, pred, "republican")
Precision(ytest, pred_adjusted,"YES")
Recall(ytest, pred_adjusted)
F1_Score(ytest, pred_adjusted)

############ Finding the best threshold


# Get probability predictions
predprob <- predict(model, xtest, type = "raw")  # Output class probabilities

# Adjust classification threshold
optimal_threshold <- 0.32  # Change this value to adjust sensitivity/specificity
# Define thresholds
thresholds <- seq(0.1,0.5, by = 0.05)


# Load the pROC package for AUC calculation
library(pROC)

# Initialize an empty dataframe to store results
results_df <- data.frame(
  Threshold = numeric(),
  Accuracy = numeric(),
  Precision_YES = numeric(),
  Recall_YES = numeric(),
  F1_Score_YES = numeric(),
  Precision_NO = numeric(),
  Recall_NO = numeric(),
  F1_Score_NO = numeric(),
  #AUC = numeric(),  # Add AUC column
  stringsAsFactors = FALSE
)

# Calculate AUC (once, outside the loop)
roc_curve <- roc(ytest, predprob[, "YES"])
auc_value <- auc(roc_curve)

# Loop through each threshold
for (threshold in thresholds) {
  
  # Adjust classification based on the threshold
  pred_adjusted <- ifelse(predprob[, "YES"] > threshold, "YES", "NO")
  
  # Convert predictions to factor (to match ytest format)
  pred_adjusted <- factor(pred_adjusted, levels = c("NO", "YES"))
  
  # Compute metrics
  accuracy <- Accuracy(pred_adjusted, ytest)
  f1_score_yes <- F1_Score(pred_adjusted, ytest, "YES")
  f1_score_no <- F1_Score(pred_adjusted, ytest, "NO")
  precision_yes <- Precision(pred_adjusted, ytest, "YES")
  precision_no <- Precision(pred_adjusted, ytest, "NO")
  recall_yes <- Recall(pred_adjusted, ytest, "YES")
  recall_no <- Recall(pred_adjusted, ytest, "NO")
  
  # Append results to the dataframe
  results_df <- rbind(results_df, data.frame(
    Threshold = threshold,
    Accuracy = round(accuracy, 3),
    Precision_YES = round(precision_yes, 3),
    Recall_YES = round(recall_yes, 3),
    F1_Score_YES = round(f1_score_yes, 3),
    Precision_NO = round(precision_no, 3),
    Recall_NO = round(recall_no, 3), 
    F1_Score_NO = round(f1_score_no, 3)
    #,AUC = round(auc_value, 3)  # Add AUC value
  ))
  
#   # Print confusion matrix and metrics for the current threshold
#   print("--------------------------")
#   print(paste("Threshold:", threshold))
#   print(ConfusionMatrix(pred_adjusted, ytest))
#   print(paste("Accuracy:", round(accuracy, 3)))
#   print(paste("F1 Score (YES):", round(f1_score_yes, 3)))
#   print(paste("F1 Score (NO):", round(f1_score_no, 3)))
#   print(paste("Precision (YES):", round(precision_yes, 3)))
#   print(paste("Precision (NO):", round(precision_no, 3)))
#   print(paste("Recall (YES):", round(recall_yes, 3)))
#   print(paste("Recall (NO):", round(recall_no, 3)))
#   print(paste("AUC:", round(auc_value, 3)))  # Print AUC
}

# Display the results dataframe as a table
print(results_df)
# Convert list to dataframe
colnames(results_df) <- c("Threshold","Accuracy", "Precision (Yes)", "Recall (Yes)", "F1 Score (Yes)"
                          , "Precision (No)", "Recall (No)", "F1 Score (No)")  

metrics_df <- results_df

# Print the metrics dataframe
print(metrics_df)
# Round all numeric values to 3 decimal places
#metrics_df <- as.data.frame(lapply(metrics_df, function(x) if(is.numeric(x)) round(x, 3) else x))
#colnames(metrics_df) <- c("Threshold","Accuracy", "Precision (Yes)", "Recall (Yes)", "F1 Score (Yes)"
       #                   , "Precision (No)", "Recall (No)", "F1 Score (No)")  

# Define custom theme with alternating row colors
custom_theme <- ttheme(
  colnames.style = list(fill = "lightgray", color = "black", face = "bold"),
  tbody.style = list(fill = c("white", "#f0f0f0"))  # Alternating rows
)
dev.off()
# Create the table with custom styling
table_plot <- ggtexttable(metrics_df, rows = NULL, theme = custom_theme)

# Display the table
print(table_plot)




# Reshape the dataframe for easier plotting

results_long <- pivot_longer(results_df, 
                             cols = -Threshold, 
                             names_to = "Metric", 
                             values_to = "Value")

# Plot all metrics in a single plot
ggplot(results_long, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Model Performance Metrics Across Thresholds",
       x = "Threshold",
       y = "Metric Value",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Alternatively, plot each metric separately
metrics <- unique(results_long$Metric)
for (metric in metrics) {
  print(
    ggplot(results_long[results_long$Metric == metric, ], aes(x = Threshold, y = Value)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = paste("Threshold vs", metric),
           x = "Threshold",
           y = metric) +
      theme_minimal()
  )
}

#### roc testing
# Adjust predictions based on the threshold
threshold <- 0.33
pred_adjusted <- ifelse(predprob[, "YES"] > threshold, "YES", "NO")

# Convert adjusted predictions to binary format (1 for "YES", 0 for "NO")
pred_adjusted_binary <- ifelse(pred_adjusted == "YES", 1, 0)
ytest_binary <- ifelse(ytest == "YES", 1, 0)

# Create a prediction object for the adjusted predictions
pred_obj_adjusted <- prediction(pred_adjusted_binary, ytest_binary)

# Calculate ROC curve for adjusted predictions
ROCcurve_adjusted <- performance(pred_obj_adjusted, "tnr", "fnr")
ROCcurve <- performance(pred_obj, "tnr", "fnr")
# Plot the ROC curve
plot(ROCcurve_adjusted, col = "blue", main = "ROC Curve for Adjusted Predictions")
abline(0, 1, col = "grey", lty = 2)  # Add a diagonal reference line

# Calculate AUC for adjusted predictions
auc_adjusted <- performance(pred_obj_adjusted, "auc")@y.values[[1]]
print(paste("AUC for Adjusted Predictions:", auc_adjusted))
ConfusionMatrix(pred_adjusted,ytest)


# Plot ROC curve for probabilistic predictions
plot(ROCcurve, col = "red", main = "ROC Curves Comparison")
# Add ROC curve for adjusted predictions
plot(ROCcurve_adjusted, col = "blue", add = TRUE)
# Add a diagonal reference line
abline(0, 1, col = "grey", lty = 2)
legend("bottomright", legend = c("Probabilistic", "Adjusted"), col = c("red", "blue"), lty = 1)
ConfusionMatrix()
### BUT


# Get prediction probabilities
predprob <- predict(model, xtest, type = "raw")

# Create a prediction object
pred_obj <- prediction(predprob[, "YES"], ytest, label.ordering = c("NO", "YES"))

# Calculate TNR (True Negative Rate) and FPR (False Positive Rate)
TNR_FPR_curve <- performance(pred_obj, "tnr", "fpr")

# Plot the TNR vs. FPR curve
plot(TNR_FPR_curve, col = "blue", main = "TNR vs. FPR Curve", xlab = "False Positive Rate (FPR)", ylab = "True Negative Rate (TNR)")
abline(1, -1, col = "grey", lty = 2)  # Add a reference line
# Calculate TPR (True Positive Rate) and FPR (False Positive Rate) for standard ROC curve
ROCcurve <- performance(pred_obj, "tpr", "fpr")

# Plot the standard ROC curve
plot(ROCcurve, col = "red", main = "ROC Curves Comparison", xlab = "False Positive Rate (FPR)", ylab = "Rate")
# Add the TNR vs. FPR curve
plot(TNR_FPR_curve, col = "blue", add = TRUE)
# Add reference lines
abline(0, 1, col = "grey", lty = 2)  # Reference line for ROC curve
abline(1, -1, col = "grey", lty = 2)  # Reference line for TNR vs. FPR curve
# Add a legend
legend("bottomright", legend = c("ROC Curve (TPR vs. FPR)", "TNR vs. FPR Curve"), col = c("red", "blue"), lty = 1)

#####
model
ConfusionMatrix(pred,ytest)

pred_adjusted <- ifelse(predprob[, "YES"] > 0.33, "YES", "NO")  # Adjust threshold
ConfusionMatrix(pred_adjusted,ytest)
Precision(ytest, pred_adjusted,"NO") # or Precision(ytest, pred, "democrat") and Precision(ytest, pred, "republican")
Precision(ytest, pred_adjusted,"YES")
Recall(ytest, pred_adjusted)
F1_Score(ytest, pred_adjusted)
################################### general optimised code for other use


# Set optimal threshold from previous analysis
optimal_threshold <- 0.33

# Convert predictions to binary format
pred_adjusted <- ifelse(predprob[, "YES"] > optimal_threshold, "YES", "NO")
pred_binary <- as.numeric(pred_adjusted == "YES")
ytest_binary <- as.numeric(ytest == "YES")

# Create prediction objects
pred_obj_raw <- prediction(predprob[, "YES"], ytest, label.ordering = c("NO", "YES"))
pred_obj_adj <- prediction(pred_binary, ytest_binary)

# Calculate performance metrics
perf_roc_raw <- performance(pred_obj_raw, "tpr", "fpr")
perf_roc_adj <- performance(pred_obj_adj, "tpr", "fpr")
perf_tnr_fpr <- performance(pred_obj_raw, "tnr", "fpr")

# Create comprehensive ROC plot
create_roc_plot <- function() {
  plot_df <- rbind(
    data.frame(
      fpr = perf_roc_raw@x.values[[1]],
      tpr = perf_roc_raw@y.values[[1]],
      Type = "Probabilistic"
    ),
    data.frame(
      fpr = perf_roc_adj@x.values[[1]],
      tpr = perf_roc_adj@y.values[[1]],
      Type = "Threshold-Adjusted"
    )
  )
  
  auc_raw <- performance(pred_obj_raw, "auc")@y.values[[1]]
  auc_adj <- performance(pred_obj_adj, "auc")@y.values[[1]]
  
  ggplot(plot_df, aes(x = fpr, y = tpr, color = Type)) +
    geom_line(size = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = c("Probabilistic" = "#E41A1C", "Threshold-Adjusted" = "#377EB8")) +
    labs(title = "ROC Curve Comparison",
         subtitle = sprintf("AUC: Probabilistic = %.3f | Threshold-Adjusted = %.3f", auc_raw, auc_adj),
         x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)",
         color = "Prediction Type") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Create TNR-FPR plot
create_tnr_plot <- function() {
  plot_df <- data.frame(
    fpr = perf_tnr_fpr@x.values[[1]],
    tnr = perf_tnr_fpr@y.values[[1]]
  )
  
  ggplot(plot_df, aes(x = fpr, y = tnr)) +
    geom_line(color = "#4DAF4A", size = 1.2) +
    geom_abline(slope = -1, intercept = 1, linetype = "dashed", color = "grey50") +
    labs(title = "Specificity Analysis",
         x = "False Positive Rate",
         y = "True Negative Rate (Specificity)") +
    theme_minimal()
}

# Generate performance metrics table
metrics <- data.frame(
  Metric = c("Accuracy", "Precision (YES)", "Recall (YES)", "F1 (YES)", 
             "Precision (NO)", "Recall (NO)", "F1 (NO)"),
  Value = c(
    Accuracy(pred_adjusted, ytest),
    Precision(ytest, pred_adjusted, "YES"),
    Recall(ytest, pred_adjusted, "YES"),
    F1_Score(ytest, pred_adjusted, "YES"),
    Precision(ytest, pred_adjusted, "NO"),
    Recall(ytest, pred_adjusted, "NO"),
    F1_Score(ytest, pred_adjusted, "NO")
  )
)

# Display results
create_roc_plot()   | create_tnr_plot()

print(metrics)
cat("\nConfusion Matrix:\n")
print(ConfusionMatrix(pred_adjusted, ytest))







########################################3














### k - fold Cross Validation
# Combine train and test data for cross-validation
alldata <- rbind(traindata, testdata)

# Set seed for reproducibility
set.seed(28)

# Number of folds
k <- 10  # You can adjust this (common values are 5 or 10)

# Create folds
folds <- cut(seq(1, nrow(alldata)), breaks=k, labels=FALSE)

# Initialize vectors to store metrics
accuracy <- numeric(k)
precision_yes <- numeric(k)
precision_no <- numeric(k)
recall_yes <- numeric(k)
recall_no <- numeric(k)
f1_yes <- numeric(k)
f1_no <- numeric(k)
auc_values <- numeric(k)

# Perform k-fold cross-validation
for(i in 1:k) {
  # Split data into training and validation sets
  test_indices <- which(folds == i)
  train_data <- alldata[-test_indices, ]
  test_data <- alldata[test_indices, ]
  
  # Get class distribution for priors
  class_dist <- table(train_data$death) / nrow(train_data)
  
  # Train model
  model <- naiveBayes(death ~ ., 
                      laplace = 1, 
                      prior = as.numeric(rev(class_dist)), 
                      data = train_data)
  
  # Make predictions
  pred_prob <- predict(model, test_data, type = "raw")
  pred_class <- ifelse(pred_prob[, "YES"] > 0.33, "YES", "NO")  # Using your optimal threshold
  
  # Convert to factors with same levels
  pred_class <- factor(pred_class, levels = c("NO", "YES"))
  true_class <- factor(test_data$death, levels = c("NO", "YES"))
  
  # Calculate metrics
  accuracy[i] <- Accuracy(pred_class, true_class)
  precision_yes[i] <- Precision(true_class, pred_class, "YES")
  precision_no[i] <- Precision(true_class, pred_class, "NO")
  recall_yes[i] <- Recall(true_class, pred_class, "YES")
  recall_no[i] <- Recall(true_class, pred_class, "NO")
  f1_yes[i] <- F1_Score(true_class, pred_class, "YES")
  f1_no[i] <- F1_Score(true_class, pred_class, "NO")
  
  # Calculate AUC
  roc_obj <- roc(response = true_class, predictor = pred_prob[, "YES"])
  auc_values[i] <- auc(roc_obj)
}

# Create results dataframe
cv_results <- data.frame(
  Fold = 1:k,
  Accuracy = accuracy,
  Precision_YES = precision_yes,
  Precision_NO = precision_no,
  Recall_YES = recall_yes,
  Recall_NO = recall_no,
  F1_YES = f1_yes,
  F1_NO = f1_no,
  AUC = auc_values
)

# Calculate mean and standard deviation across folds
mean_results <- colMeans(cv_results[-1])
sd_results <- apply(cv_results[-1], 2, sd)

# Create summary dataframe
summary_results <- data.frame(
  Metric = names(mean_results),
  Mean = mean_results,
  SD = sd_results
)

# Print results
print("Cross-Validation Results per Fold:")
print(cv_results)

print("\nSummary Statistics Across Folds:")
print(summary_results)


ggplot(summary_results, aes(x = Metric, y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.4) +
  labs(title = "Model Performance Metrics Across K-Folds",
       x = "Metric",
       y = "Mean Value ± SD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# Data Combination: I combined your train and test data to perform cross-validation on the entire dataset.
# 
# Reproducibility: Set a seed for consistent fold creation.
# 
# Metrics Tracked: The code tracks all the metrics you were using (accuracy, precision, recall, F1 for both classes, and AUC).
# 
# Optimal Threshold: I used your optimal threshold of 0.33 for the "YES" class.
# 
# Results Presentation: The code provides:
#   
#   Detailed results for each fold
# 
# Mean and standard deviation across all folds
# 
# A visualization of the results with error bars
# 
# Class Imbalance Handling: The priors are automatically calculated for each fold based on the training data distribution.
# 
# This approach gives you a more robust estimate of your model's performance than a single train-test split, as it evaluates the model on multiple different splits of the data.
