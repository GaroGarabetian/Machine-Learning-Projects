# source("cleaning_data.R")

# Research Question 1 :
#How can decision trees be utilized to 
#  predict patient mortality
# using age, respiration rate (rr_min, rr_max), and SOFA score as predictors?

#Evaluate
# the decision tree's effectiveness using accuracy, precision, and recall


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
             "rpart",
              "rpart.plot",
              "randomForest","caret")
load_packages(packages)

#Importing clean data
traindata = read.csv("cleaned_traindata.csv",stringsAsFactors = TRUE)
testdata = read.csv("cleaned_testdata.csv",stringsAsFactors = TRUE)
alldata = rbind(traindata,testdata)
cat("Percentage of training data ",dim(traindata)[1]*100/dim(alldata)[1],"%")
str(traindata)
str(testdata)
#Checking the distributions of data and for unusual values
view(dfSummary(traindata[-1]))
view(dfSummary(testdata[-1]))
#keep the data we need for the models only
names(traindata)
variable_needed <-c("age","rr_min","rr_max","SOFA" ,"death")

traindata <- traindata[,variable_needed]
testdata <- testdata[,variable_needed]

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

names(traindata)
nrow(traindata)
# 
# method = "class"	Since death is categorical, we use "class" for classification (for regression, it would be "anova").
# minsplit = 1	The minimum number of observations required to split a node. 
                #Setting it to 1 means every node can split as long as there's at least 1 data point, 
                #leading to a very deep tree (overfitting risk).
# minbucket = 1	The minimum number of observations per terminal node (leaf).
                #Setting it to 1 allows very small leaves, increasing the risk of overfitting.

# cp = -1	Complexity parameter: Controls pruning. 
          #A negative cp (like -1) prevents pruning, making the tree grow fully. 
          #Normally, a positive cp helps avoid overfitting by pruning unnecessary splits.

view(dfSummary(traindata[c("age","rr_min","rr_max","SOFA")]))
library(magrittr)
traindata[c("age","rr_min","rr_max","SOFA")] %>%
  descr(stats = "common") %>%
  tb() %>%
  knitr::kable()

library(magrittr)
library(ggpubr)

#rownames(traindata[c("age", "rr_min", "rr_max", "SOFA")])<-c("Age","Resp.Rate Min","Resp.Rate Max","SOFA")
# Compute descriptive statistics
summary_stats <- traindata[c("age", "rr_min", "rr_max", "SOFA")] %>%
  descr(stats = "common") %>%
  as.data.frame()

# Round to 2 decimal places
summary_stats <- round(summary_stats, 2)

# Remove last row (if necessary)
summary_stats <- summary_stats[-nrow(summary_stats), ]

# Transpose the data frame (swap rows and columns)
summary_stats_transposed <- t(summary_stats)

# Rename columns (new row names)
colnames(summary_stats_transposed) <- c("Mean", "St Dev", "Min", "Median", "Max", "Total")

# Convert the row names to a column for better formatting
rownames(summary_stats_transposed) <- c("Age","Resp.Rate Min","Resp.Rate Max","SOFA")

summary_stats_transposed <- cbind(Variable = rownames(summary_stats_transposed), summary_stats_transposed)
#rownames(summary_stats_transposed) <- NULL

# Define a custom theme with alternating row colors
custom_theme <- ttheme(
  colnames.style = list(fill = "lightgray", color = "black", face = "bold"),
  tbody.style = list(fill = c("white", "#f0f0f0"))  # Alternating row colors
)

# Create the table plot
table_plot <- ggtexttable(summary_stats_transposed, rows = NULL, theme = custom_theme)

# Display the table
print(table_plot)

########### summary statistics


labs <- c('Age',
          'Resp. Rate Min',
          'Resp. Rate Max',
          'SOFA Score')
variable_needed
sumtable(traindata[c("age", "rr_min", "rr_max", "SOFA")],labels=labs,col.align ='left')
sumtable(traindata[c("age", "rr_min", "rr_max", "SOFA")],labels=labs,align = 'p{.3\\textwidth}ccccccc', fit.page = '\\textwidth', out = 'latex')
sumtable(alldata[c("age", "rr_min", "rr_max", "SOFA")],labels=labs,align = 'p{.3\\textwidth}ccccccc', fit.page = '\\textwidth', out = 'latex')

# #for r markdown
 dfSummary(traindata[c("age","rr_min","rr_max","SOFA")], 
           plain.ascii  = FALSE, 
           style        = "grid", 
           graph.magnif = 0.82, 
           varnumbers   = FALSE,
           valid.col    = FALSE,
           tmp.img.dir  = "/tmp")


#  Create complete (unpruned) tree
model <- rpart(death ~ age + rr_min + rr_max + SOFA
               , method = "class", data = traindata,
                minsplit=5,minbucket=20)
rpart.plot(model, extra = 101, nn = TRUE)
rpart.plot(model, extra = 104, nn = TRUE)


# Create full tree with initial minsplit
model1 <- rpart(death ~ age + rr_min + rr_max + SOFA,
               method = "class",
               data = traindata,
               minsplit = 50, minbucket = 5,
               )


rpart.plot(model1,extra = 104,nn=TRUE)

# Print complexity parameter table
printcp(model1)
#Aim to minimise xerror and pick the right complexity parameter                      

# Plot cross-validation results
plotcp(model1)


# Get best cp (smallest x-error)
best_cp <- model1$cptable[which.min(model1$cptable[, "xerror"]), "CP"]

# Prune the tree
pruned_model <- prune(model1, cp = best_cp)

# Visualize the pruned tree
rpart.plot(pruned_model, extra = 104, nn = TRUE)




# Apply the model to the test data
xtest = testdata[,c( "age" , "rr_min" , "rr_max" ,"SOFA")]
ytest = testdata[,"death"]
#comment/ uncomment for the preferred  metrics
pred = predict(model1, xtest, type="class")
pred = predict(pruned_model, xtest, type="class")

# Create confusion matrix and compute metrics
cm = as.matrix(table(Actual = ytest, Predicted = pred))
cm[2,2]

ConfusionMatrix(pred, ytest)
Accuracy(ytest, pred)

Precision(ytest, pred, "YES")
Recall(ytest, pred, "YES")
F1_Score(ytest, pred, "YES")

Precision(ytest, pred, "NO")
Recall(ytest, pred, "NO")
F1_Score(ytest, pred, "NO")

#########################
# Define model names
model_names <- c("model1", "pruned_model")

# Initialize an empty list to store results
metrics_list <- list()

# Loop through each model
for (model_name in model_names) {
  
  # Apply the model
  pred <- predict(get(model_name), xtest, type = "class")
  
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
colnames(metrics_df) <- c("Metrics", "Model 1", "Pruned Model 1")  # Example column names


# Define custom theme with alternating row colors
custom_theme <- ttheme(
  colnames.style = list(fill = "lightgray", color = "black", face = "bold"),
  tbody.style = list(fill = c("white", "#f0f0f0"))  # Alternating rows
)

# Create the table with custom styling
table_plot <- ggtexttable(metrics_df, rows = NULL, theme = custom_theme)

# Display the table
print(table_plot)



#####################Creating Grid for better search of model
# f


# Apply the model to the test data
xtest = testdata[,c( "age" , "rr_min" , "rr_max" ,"SOFA")]
ytest = testdata[,"death"]

# Define values for grid search
#Ensure minsplit >= 2 * minbucket.

minsplit_values <- c( 10, 20, 30,40, 50)
minbucket_values <- c(5,7, 10)
xtest = testdata[,c( "age" , "rr_min" , "rr_max" ,"SOFA")]
ytest = testdata[,"death"]
# Create an empty dataframe to store results
results <- data.frame(minsplit = numeric(),
                      minbucket = numeric(),
                      tree_size = numeric(),
                      accuracy = numeric())

# Perform grid search
for (minsplit_value in minsplit_values) {
  for (minbucket_value in minbucket_values) {
    
    # Train the model
    model <- rpart(death ~ age + rr_min + rr_max + SOFA, 
                   method = "class", 
                   data = traindata, 
                   minsplit = minsplit_value, 
                   minbucket = minbucket_value
                )
    
    # Make predictions
    
    pred <- predict(model, xtest, type = "class")
    
    # Compute accuracy
    acc <- Accuracy(ytest, pred)
    
    # Store results
    results <- rbind(results, data.frame(
      minsplit = minsplit_value,
      minbucket = minbucket_value,
      tree_size = length(model$frame$var),
      accuracy = acc
    ))
  }
}

# Print results
print(results)
rpart.plot(model)

# Define grid search values
best_cp

#grid parameters
minsplit_values <- c( 10,20, 30,40, 50)
minbucket_values <- c(5,7, 10)
cp_values <- c(0.001, 0.01, best_cp, 0.05, 0.1)  # Complexity parameter values
xtest = testdata[,c( "age" , "rr_min" , "rr_max" ,"SOFA")]
ytest = testdata[,"death"]
# Create an empty dataframe to store results
results <- data.frame(
  minsplit = numeric(),
  minbucket = numeric(),
  cp = numeric(),
  tree_size = numeric(),
  accuracy = numeric(),
  precision_YES = numeric(),
  recall_YES = numeric(),
  F1_YES = numeric(),
  precision_NO = numeric(),
  recall_NO = numeric(),
  F1_NO = numeric()
)

# Nested loop for grid search
for (minsplit_value in minsplit_values) {
  for (minbucket_value in minbucket_values) {
    for (cp_value in cp_values) {
      
      # Train the model
      model <- rpart(death ~ age + rr_min + rr_max + SOFA, 
                     method = "class", 
                     data = traindata, 
                     minsplit = minsplit_value, 
                     minbucket = minbucket_value, 
                     cp = cp_value)
      
      # Make predictions
      pred <- predict(model, xtest, type = "class")
      
      # Compute evaluation metrics
      acc <- Accuracy(ytest, pred)
      pr_Y <- Precision(ytest, pred, "YES")
      recall_Y <- Recall(ytest, pred, "YES")
      F1_Y <- F1_Score(ytest, pred, "YES")
      pr_N <- Precision(ytest, pred, "NO")
      recall_N <- Recall(ytest, pred, "NO")
      F1_N <- F1_Score(ytest, pred, "NO")
      
      # Store results
      results <- rbind(results, data.frame(
        minsplit = minsplit_value,
        minbucket = minbucket_value,
        cp = round(cp_value, 4),
        tree_size = length(model$frame$var),
        accuracy = acc,
        precision_YES = pr_Y,
        recall_YES = recall_Y,
        F1_YES = F1_Y,
        precision_NO = pr_N,
        recall_NO = recall_N,
        F1_NO = F1_N
      ))
    }
  }
}

# Remove rows with NA values
results <- na.omit(results)

# Print cleaned results
print(results)




# 
# 1. Tree Complexity
# Tree complexity refers to how deep or complicated the tree is, meaning the number of nodes and branches. This complexity is influenced by parameters like:
#   minsplit: The minimum number of observations required to split a node. Lower values allow the tree to grow deeper (more complex), while higher values prevent the tree from growing too deep.
# maxdepth: The maximum depth of the tree (optional).
# minbucket: The minimum number of observations in a leaf node.
# A more complex tree will tend to overfit the training data, meaning it might be too specific to the training set and not generalize well to new, unseen data.
# 2. Generalization Performance
# Generalization refers to how well a model can predict outcomes on new, unseen data. In the case of decision trees:
#   A model that overfits (due to high complexity) will have very low error on the training data but poor performance on test data because it has "memorized" the training set rather than learning the underlying patterns.
# A model that underfits (due to excessive pruning or constraints like high minsplit) will be too simplistic, not capturing enough detail to accurately make predictions, leading to poor performance on both training and test data.
# The Trade-Off: Complexity vs. Generalization
# Too Complex (Overfitting): If you allow the tree to grow too deep by setting very low minsplit, minbucket, or using a very small cp (complexity parameter), you might end up with a very detailed tree that perfectly classifies the training data. However, this tree might not generalize well to unseen data (i.e., the test set) because it has memorized the training set noise or minor details. This is called overfitting.
# 
# Too Simple (Underfitting): On the other hand, if you set values for minsplit and minbucket too high, the tree will be too simplified and might not capture the important patterns in the data. This leads to underfitting, where the tree doesn’t perform well even on the training data and will likely perform poorly on the test data.
# 
# How to Find the Best Balance?
#   To find the best trade-off between complexity and generalization:
#   
#   Cross-Validation: This is where cross-validation comes in. By evaluating your model on different subsets of your data (e.g., using k-fold cross-validation), you can assess how well the tree generalizes beyond the training set. Cross-validation helps you find the cp value and minsplit that balances complexity with good predictive performance.
# 
# In the complexity parameter table (printcp(model)), you will see different cp values and the corresponding cross-validation error. Lower cp values generally result in a more complex tree, while higher cp values prune the tree and reduce complexity.
# Plotting cp vs. Cross-Validation Error (xerror):
#   
#   When you plot the complexity parameter (plotcp(model)), we’ll notice that the x-error (cross-validation error) initially decreases as you make the tree more complex. But after a certain point, making the tree more complex starts to increase the x-error, indicating overfitting.
# Prune the Tree: By pruning the tree at the optimal cp (where the x-error is minimized), we avoid overfitting and underfitting. The pruning process removes unnecessary branches that do not add value, making the model simpler and more generalized while retaining its predictive power.
# 
# In Practice: How to Identify the Best Tree
# Train the unpruned model: First, build an unpruned tree with default or chosen minsplit and minbucket values.
# Use printcp() and plotcp() to identify the best cp that minimizes cross-validation error.
# Prune the tree: Once you’ve found the optimal cp, prune the tree to remove unnecessary splits.
# Evaluate the model performance: You can then evaluate your pruned tree on a test set to ensure that it generalizes well.
 #

###heatmap


#  grid search values
minsplit_values <- c(10, 20, 30, 40, 50)
minbucket_values <- c(5, 7, 10,15)
cp_values <- c(0.001, 0.01, 0.02, 0.04,0.05, 0.1)  # Including your best_cp 

# Prepare test data
xtest <- testdata[, c("age", "rr_min", "rr_max", "SOFA")]
ytest <- testdata[, "death"]

# Create results dataframe
results <- expand.grid(
  minsplit = minsplit_values,
  minbucket = minbucket_values,
  cp = cp_values
) %>%
  mutate(
    tree_size = NA_real_,
    accuracy = NA_real_,
    precision_YES = NA_real_,
    recall_YES = NA_real_,
    F1_YES = NA_real_,
    precision_NO = NA_real_,
    recall_NO = NA_real_,
    F1_NO = NA_real_
  )

# Perform grid search (this may take some time)
for (i in 1:nrow(results)) {
  params <- results[i, ]
  
  # Train model with current parameters
  model <- rpart(death ~ age + rr_min + rr_max + SOFA, 
                 method = "class", 
                 data = traindata, 
                 minsplit = params$minsplit,
                 minbucket = params$minbucket,
                 cp = params$cp)
  
  # Make predictions
  pred <- predict(model, xtest, type = "class")
  
  # Store metrics
  results$tree_size[i] <- length(model$frame$var)
  results$accuracy[i] <- Accuracy(ytest, pred)
  results$precision_YES[i] <- Precision(ytest, pred, "YES")
  results$recall_YES[i] <- Recall(ytest, pred, "YES")
  results$F1_YES[i] <- F1_Score(ytest, pred, "YES")
  results$precision_NO[i] <- Precision(ytest, pred, "NO")
  results$recall_NO[i] <- Recall(ytest, pred, "NO")
  results$F1_NO[i] <- F1_Score(ytest, pred, "NO")
}

# Remove rows with NA values (if any failed)
results <- na.omit(results)

# Find best parameters based on F1 score for YES class (you can change this metric)
best_params <- results %>%
  arrange(desc(F1_YES)) %>%
  slice(1)

print("Best Parameters:")
print(best_params) #expected the one with cp=0.001 deep size

# Create heatmap for accuracy
heatmap_accuracy <- results %>%
  group_by(minsplit, minbucket) %>%
  summarise(mean_accuracy = mean(accuracy)) %>%
  ggplot(aes(x = factor(minsplit), y = factor(minbucket), fill = mean_accuracy)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Mean Accuracy by Minimum Split and Minimum Node Size",
       x = "Minimum Split",
       y = "Minimum Node Size",
       fill = "Accuracy") +
  theme_minimal()

print(heatmap_accuracy)

# Create heatmap for F1 score (YES class)
heatmap_f1 <- results %>%
  group_by(minsplit, minbucket) %>%
  summarise(mean_f1 = mean(F1_YES)) %>%
  ggplot(aes(x = factor(minsplit), y = factor(minbucket), fill = mean_f1)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Mean F1 Score (YES class) by Minimum Split and Minimum Node Size",
       x = "Minimum Split",
       y = "Minimum Node Size",
       fill = "F1 Score") +
  theme_minimal()

print(heatmap_f1)

# Create heatmap for tree size
heatmap_size <- results %>%
  group_by(minsplit, minbucket) %>%
  summarise(mean_size = mean(tree_size)) %>%
  ggplot(aes(x = factor(minsplit), y = factor(minbucket), fill = mean_size)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Mean Tree Size  by Minimum Split and Minimum Node Size",
       x = "Minimum Split",
       y = "Minimum Node Size",
       fill = "Tree Size") +
  theme_minimal()

print(heatmap_size)

# Combined plot showing relationship between cp and all metrics
combined_cp_plot <- results %>%
  group_by(cp) %>%
  summarise(
    F1 = mean(F1_YES),
    Accuracy = mean(accuracy),
    Size = mean(tree_size)
  ) %>%
  pivot_longer(cols = c(F1, Accuracy, Size), names_to = "Metric") %>%
  ggplot(aes(x = cp, y = value, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Metric, scales = "free_y", ncol = 1) +
  labs(title = "Impact of Complexity Parameter on Model Performance",
       x = "Complexity Parameter (cp)",
       y = "Metric Value") +
  theme_minimal() +
  scale_x_continuous(trans = 'log10') +  # Log scale for cp
  annotation_logticks(sides = "b")

print(combined_cp_plot)

# Visualize trade-off between complexity and performance
ggplot(results, aes(x = tree_size, y = F1_YES, color = factor(cp))) +
  geom_point(size = 4) +
  labs(title = "Model Performance vs Complexity",
       x = "Tree Size (Number of Nodes)",
       y = "F1 Score (YES class)",
       color = "Complexity\nParameter (cp)") +
  theme_minimal()

# Train final model with best parameters
final_model <- rpart(death ~ age + rr_min + rr_max + SOFA, 
                     method = "class", 
                     data = traindata, 
                     minsplit = best_params$minsplit,
                     minbucket = best_params$minbucket,
                     cp = best_params$cp)

# Visualize final model
rpart.plot(final_model, main = "Overfitted Decision Tree on Training Data (CP = 0.001)")


# Print complexity parameter table
printcp(final_model)
#Aim to minimise xerror and pick the right complexity parameter                      

# Plot cross-validation results
plotcp(final_model)


# Get best cp (smallest x-error)
best_cp <- final_model$cptable[which.min(final_model$cptable[, "xerror"]), "CP"]

# Prune the tree
pruned_model <- prune(final_model, cp = best_cp)
rpart.plot(pruned_model, main = "Pruned Decision Tree", extra = 104, nn = TRUE)

# Print and plot CP table
dev.off()
printcp(final_model)
plotcp(final_model)


# Get optimal CP values
best_cp <- final_model$cptable[which.min(final_model$cptable[, "xerror"]), "CP"]
best_cp_1se <- final_model$cptable[
  final_model$cptable[, "xerror"] <= min(final_model$cptable[, "xerror"]) + 
    final_model$cptable[which.min(final_model$cptable[, "xerror"]), "xstd"],
  "CP"
] %>% max()

# Prune trees
min_error_tree <- prune(final_model, cp = best_cp)
robust_tree <- prune(final_model, cp = best_cp_1se)

# Compare sizes
cat("Full tree size:", length(final_model$frame$var), "\n")
cat("Minimum error tree size:", length(min_error_tree$frame$var), "\n")
cat("1-SE rule tree size:", length(robust_tree$frame$var), "\n")

# Visual comparison
par(mfrow = c(1, 3))
rpart.plot(final_model, main = "Full Tree")
rpart.plot(min_error_tree, main = "Minimum Error Tree")
rpart.plot(robust_tree, main = "1-SE Rule Tree")

par(mfrow = c(1, 1))
rpart.plot(robust_tree, main = "1-SE Rule Tree",extra = 104,nn = TRUE)



####### table creation for metrics

test_features <- testdata[, c("age", "rr_min", "rr_max", "SOFA")]
test_target <- testdata$death 
# Enhanced evaluation function with error handling
evaluate_model <- function(model, features, target) {
  # Ensure factor levels match for later use in other RQs
  pred <- predict(model, features, type = "class")
  pred <- factor(pred, levels = levels(target))
  
  # Calculate probabilities for AUC
  pred_prob <- predict(model, features, type = "prob")[,"YES"]
  
  # Compute metrics with tryCatch for robustness
  metrics <- tryCatch({
    list(
      Accuracy = MLmetrics::Accuracy(target, pred),
      Precision_YES = MLmetrics::Precision(target, pred, "YES"),
      Recall_YES = MLmetrics::Recall(target, pred, "YES"),
      F1_YES = MLmetrics::F1_Score(target, pred, "YES"),
      Precision_NO = MLmetrics::Precision(target, pred, "NO"),
      Recall_NO = MLmetrics::Recall(target, pred, "NO"),
      F1_NO = MLmetrics::F1_Score(target, pred, "NO"),
      AUC = as.numeric(pROC::roc(response = target, predictor = pred_prob)$auc),
      Tree_Size = length(model$frame$var),
      CP = ifelse(is.null(model$cptable), NA, 
                  model$cptable[which.min(model$cptable[,"xerror"]), "CP"])
    )
  }, error = function(e) {
    warning(paste("Error in evaluation:", e$message))
    return(rep(NA, 10))
  })
  
  return(metrics)
}

# Evaluate all three models using the proper test set
full_metrics <- evaluate_model(final_model, test_features, test_target)
min_error_metrics <- evaluate_model(min_error_tree, test_features, test_target) 
robust_metrics <- evaluate_model(robust_tree, test_features, test_target)

# Create publication-ready table
library(gt)

metrics_table <- data.frame(
  Metric = c("Accuracy", "Precision (YES)", "Recall (YES)", "F1 (YES)",
             "Precision (NO)", "Recall (NO)", "F1 (NO)", "AUC",
             "Tree Size", "Optimal CP"),
  Full_Tree = unlist(full_metrics),
  Min_Error = unlist(min_error_metrics),
  Robust_Tree = unlist(robust_metrics)
) %>%
  gt() %>%
  fmt_number(columns = 2:4, decimals = 3) %>%
  tab_header(
    title = "Decision Tree Model Performance Comparison",
    subtitle = "Evaluation on Independent Test Set"
  ) %>%
  cols_label(
    Full_Tree = "Full Tree",
    Min_Error = "Min Error Tree", 
    Robust_Tree = "1-SE Robust Tree"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.width = pct(95),
    table.align = "center"
  )

# Display the table
metrics_table

# Visual comparison plot

metrics_long <- data.frame(
  Model = rep(c("Full Tree", "Min Error", "1-SE Robust"), each = 10),
  Metric = rep(c("Accuracy", "Precision_YES", "Recall_YES", "F1_YES",
                 "Precision_NO", "Recall_NO", "F1_NO", "AUC",
                 "Tree_Size", "CP"), 3),
  Value = c(unlist(full_metrics), unlist(min_error_metrics), unlist(robust_metrics))
)

ggplot(metrics_long %>% filter(Metric %in% c("Accuracy", "F1_YES", "AUC")), 
       aes(x = Model, y = Value, fill = Model)) +
  geom_col(position = "dodge") +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Key Performance Metrics Comparison for the 3 decision trees",
       y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

















##############Random forest



# Create a Random Forest model
rf_model <- randomForest(death ~ age + rr_min + rr_max + SOFA, 
                         data = traindata, 
                         method = "class", 
                         ntree = 100,   # Number of trees
                         mtry = 3,      # Number of features to consider at each split (default is sqrt of number of features)
                         nodesize = 10, # Minimum size of terminal nodes
                         importance = TRUE) # Display feature importance

# View the model summary
print(rf_model)
# Basic error vs. number of trees plot

plot(rf_model, main = "Error Rate vs. Number of Trees")
legend("topright", 
       legend = colnames(rf_model$err.rate),
       col = 1:ncol(rf_model$err.rate),
       lty = 1)

# Variable Importance Plot
var_imp <- importance(rf_model, type = 1) # Mean Decrease in Accuracy
var_imp <- data.frame(Variable = rownames(var_imp), Importance = var_imp[,1])

# Create ggplot
library(ggplot2)
ggplot(var_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance",
       x = "Predictor Variable",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()

# For a specific variable (e.g., age)
partialPlot(rf_model, 
            pred.data = traindata, 
            x.var = "age",
            which.class = "YES",
            main = "Partial Dependence on Age")

# Enhanced importance plot
varImpPlot(rf_model,
           type = 1, # Mean decrease in accuracy
           scale = TRUE,
           main = "Variable Importance",
           col = "blue",
           pch = 19)

 
# # View tree structure as text
# tree_data <- randomForest::getTree(rf_model, k = 1, labelVar = TRUE)
# head(tree_data, 10)  # Show splitting rules for first 10 nodes
# 

 # Shows decision rules
# library(DiagrammeR)
# library(data.tree)
# 
# # Convert to data.tree format
# tree_df <- as.data.frame(tree_data)
# tree_df$pathString <- paste("root", tree_df$`left daughter`, tree_df$`right daughter`, sep = "/")
# tree_structure <- as.Node(tree_df)
# 
# # Plot interactive diagram
# plot(tree_structure, 
#      direction = "TB",  # Top-to-bottom layout
#      output = "graph",  # Interactive HTML widget
#      width = "100%", 
#      height = "800px")


# Predict on test data
rf_predictions <- predict(rf_model, newdata = testdata)



# Apply the model to the test data
xtest = testdata[,c( "age" , "rr_min" , "rr_max" ,"SOFA")]
ytest = testdata[,"death"]
pred = predict(rf_model, xtest, type="class")


ConfusionMatrix(pred, ytest)
Accuracy(pred, ytest) #################fix accuracy elsewhere like this

Precision(ytest, pred, "YES")
Recall(ytest, pred, "YES")
F1_Score(ytest, pred, "YES")

Precision(ytest, pred, "NO")
Recall(ytest, pred, "NO")
F1_Score(ytest, pred, "NO")


# Predicted probabilities
pred_probs <- predict(rf_model, xtest, type = "prob")

# ROC curve
library(pROC)

par(mfrow = c(1, 2))
roc_curve <- roc(ytest, pred_probs[,2])

plot(roc_curve, main = "Random Forest ROC Curve", col = "blue", lwd = 2)

# Add a legend to the plot, including the AUC value
legend("bottomright", 
       legend = paste("AUC =", round(auc(roc_curve), 3)), 
       col = "blue", 
       lwd = 2, 
       bty = "n")  # 'bty = "n"' removes the box around the legend



# models <-list(rf_model,robust_tree,final_model, pruned_model)
# 
# 
# # Assuming you have a list of models and your test data
# library(pROC)
# par(mfrow = c(1, 1))
# # Set up a blank plot
# plot(1, type = "n", xlab = "False Positive Rate", ylab = "True Positive Rate", xlim = c(0, 1), ylim = c(0, 1))
# 
# # Loop through each model and add its ROC curve to the plot
# for (i in 1:length(models)) {
#   # Get predicted probabilities for the "YES" class (adjust the class name accordingly)
#   pred_probs <- predict(models[[i]], xtest, type = "prob")
#   
#   # Compute the ROC curve for the "YES" class (class 2 for binary classification)
#   roc_curve <- roc(ytest, pred_probs[, 2])
#   
#   # Add the ROC curve to the plot
#  # lines.roc(ytest, pred_probs[,"YES"],percent=TRUE,col=i,print.auc = TRUE,ci=TRUE,print.thres="best",asp = NA)
#   
#   lines(roc_curve$specificities, roc_curve$sensitivities, col = i, lwd = 2)
# }
# 
# # Add a legend to identify each curve
# legend("bottomright", legend = paste("Model", 1:length(models)), col = 1:length(models), lwd = 2)


dev.off()
models <-list(rf_model,robust_tree,final_model, pruned_model)

# Define your model names (in the same order as your models list)
model_names <- c("Random Forest", "1-SE Robust Tree", "Full Decision Tree", "Pruned Decision Tree")

# Set up color palette
model_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")

# Create empty plot
plot(1, type = "n", 
     xlab = "False Positive Rate (1 - Specificity)", 
     ylab = "True Positive Rate (Sensitivity)",
     xlim = c(1, 0), ylim = c(0, 1),  # Note xlim reversed for ROC convention
     main = "ROC Curve Comparison")

# Add diagonal reference line
abline(a = 0, b = 1, col = "gray", lty = 2)

# Initialize AUC storage
auc_values <- numeric(length(models))

# Plot each ROC curve
for (i in 1:length(models)) {
  pred_probs <- predict(models[[i]], xtest, type = "prob")
  roc_obj <- roc(ytest, pred_probs[, "YES"])
  
  # Plot ROC curve
  lines(1 - roc_obj$specificities, roc_obj$sensitivities, 
        col = model_colors[i], lwd = 2.5)
  
  # Store AUC value
  auc_values[i] <- auc(roc_obj)
  
  # Add AUC text at 40% x-position, spaced vertically
  # text(x = 0.8, y = 0.3 - (i-1)*0.1,
  #      labels = sprintf("%s (AUC = %.3f)", model_names[i], auc_values[i]),
  #      col = model_colors[i], adj = 0)
}

# Alternative legend version
legend("bottomleft", 
       legend = sprintf("%s (AUC = %.3f)", model_names, auc_values),
       col = model_colors,
       lwd = 2.5,
       cex = 0.8,
       bty = "n")


# ###############
# # Make predictions
# pred1 <- predict(robust_tree, xtest, type="prob")
# pred_prune <- predict(pruned_model, xtest, type = "prob")
# pred_rf <- predict(rf_model, xtest, type="prob")
# # Compute ROC curves
# roc_rf <- roc(ytest, pred_rf[,"YES"])
# roc_dt <- roc(ytest, pred1[,"YES"])
# roc_pruned <- roc(ytest, pred_prune[,"YES"])
# 
# roc5 <- plot.roc(ytest, pred_rf[,"YES"],percent=TRUE,col="red",print.auc = FALSE,ci=TRUE,print.thres="best",asp = NA)
# roc3 <- lines.roc(ytest, pred1[,"YES"],main="Statistical comparison", percent=TRUE, col="#1c61b6",print.auc = TRUE,ci=TRUE,print.thres="best",asp = NA)
# roc4 <- lines.roc(ytest, pred_prune[,"YES"],percent=TRUE,col="#008600",print.auc = TRUE,ci=TRUE,print.thres="best",asp = NA)
# # Extract AUC values
# auc_rf <- round(auc(roc_rf), 3)
# auc_dt <- round(auc(roc_dt), 3)
# auc_pruned <- round(auc(roc_pruned), 3)
# 
# # Add a legend with AUC values
# legend("bottomright",
#        legend = c(paste("Random Forest Model (AUC =", auc_rf, ")"),
#                   paste("Robust Model Tree (AUC =", auc_dt, ")"),
#                   paste("Pruned Model Tree (AUC =", auc_pruned, ")")),
#        col = c("red", "orange", "#008600"), 
#        lwd = 2)
# 
# 
# # Perform the statistical comparison using the Bootstrap method
# comparison <- roc.test(roc3, roc5, method="bootstrap")
# p_value <- comparison$p.value  # Extract the p-value
# 
# # Add the legend with the p-value
# 
# legend("topleft",
#        legend=paste("ROC test (Bootstrap) p-value <0.001"))

#### optimised code for later use

# Define models with descriptive names
model_list <- list(
  "Random Forest" = rf_model,
  "Robust Tree (1-SE)" = robust_tree,
  "Pruned Tree" = pruned_model
)

# Generate predictions and ROC curves
roc_results <- lapply(model_list, function(model) {
  pred_prob <- predict(model, xtest, type = "prob")[,"YES"]
  roc_obj <- roc(ytest, pred_prob)
  return(roc_obj)
})

# Create enhanced ROC plot
ggroc(roc_results, size = 1.2) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c("blue", "orange", "red"),
    labels = paste0(names(roc_results), 
                    " (AUC = ", 
                    sapply(roc_results, function(x) round(auc(x), 3)), 
                    ")")
  ) +
  labs(
    title = "ROC Curve Comparison with Statistical Testing",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Models"
  ) +
  theme_minimal() +
  theme(
    legend.position.inside = c(0.7, 0.3),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) -> roc_plot

# Perform pairwise statistical comparisons
comparisons <- combn(names(roc_results), 2, simplify = FALSE)
test_results <- lapply(comparisons, function(pair) {
  test <- roc.test(roc_results[[pair[1]]], roc_results[[pair[2]]], method = "bootstrap")
  data.frame(
    Model1 = pair[1],
    Model2 = pair[2],
    AUC1 = round(auc(roc_results[[pair[1]]]), 3),
    AUC2 = round(auc(roc_results[[pair[2]]]), 3),
    Difference = round(test$estimate[1] - test$estimate[2], 3),
    P_value = format.pval(test$p.value, eps = 0.001, digits = 3)
  )
})

# Create comparison table
comparison_table <- do.call(rbind, test_results)

# Add significance stars
comparison_table$Significance <- ifelse(comparison_table$P_value < 0.001, "***",
                                        ifelse(comparison_table$P_value < 0.01, "**",
                                               ifelse(comparison_table$P_value < 0.05, "*", "ns")))

# Print the plot and table
print(roc_plot)
library(grid)
library(gridExtra)
grid.arrange(
  tableGrob(comparison_table, 
            theme = ttheme_minimal(
              core = list(bg_params = list(fill = c("#F7F7F7", "#FFFFFF"), col = NA),
                          fg_params = list(hjust = 0, x = 0.1)),
              colhead = list(fg_params = list(col = "white", fontface = "bold"),
                             bg_params = list(fill = "#2C3E50"))
            )),
  ncol = 1,
  top = textGrob("Pairwise ROC Curve Comparisons (Bootstrap Test)", 
                 gp = gpar(fontsize = 12, fontface = "bold"))
)


# View feature importance
importance(rf_model)
dev.off()
# Plot feature importance
varImpPlot(rf_model)


# Print session info to console
sessionInfo()

# Save to a text file
sink("session_info.txt")
sessionInfo()
sink()




