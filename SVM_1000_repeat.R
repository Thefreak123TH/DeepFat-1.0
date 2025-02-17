rm(list=ls())
library(caret)
library(kernlab)
library(e1071)

df <- read.csv(".../xxx.csv", header=TRUE)
df <- na.omit(df)  

df$cadrads1 <- factor(df$cadrads1)
levels(df$cadrads1) <- make.names(levels(df$cadrads1), unique = TRUE)

num_iterations <- 10
num_folds <- 5  

# Initialize vectors to store overall metrics
overall_sensitivity <- numeric(num_iterations)
overall_specificity <- numeric(num_iterations)
overall_accuracy <- numeric(num_iterations)
overall_auc <- numeric(num_iterations)

set.seed(123)  # For reproducibility

for(iter in 1:num_iterations) {
  # Define the training control for 5-fold cross-validation
  trainControl <- trainControl(
    method = "cv",
    number = num_folds,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,  # Needed for ROC/AUC calculation
    savePredictions = "final"
  )
  
  # Train the SVM model using grid search (if needed, specify your tuneGrid)
  svmModel <- train(
    cadrads1 ~ ., 
    data = df,
    method = "svmRadial",  
    trControl = trainControl,
    metric = "ROC",
    preProcess = c("center", "scale")  # Pre-processing: centering and scaling
  )
  
  # Extract and store metrics from the trained model
  overall_sensitivity[iter] <- svmModel$results$Sens[svmModel$results$ROC == max(svmModel$results$ROC)]
  overall_specificity[iter] <- svmModel$results$Spec[svmModel$results$ROC == max(svmModel$results$ROC)]
  overall_accuracy[iter] <- (overall_sensitivity[iter] + overall_specificity[iter]) / 2  
  overall_auc[iter] <- max(svmModel$results$ROC)
  
  # Print the best tuning parameters for the current iteration
  best_params <- svmModel$bestTune
  cat("Iteration:", iter, "\n")
  cat("Best C:", best_params$C, "\n")
  cat("Best sigma:", best_params$sigma, "\n")
  
}

# Calculate the mean and standard deviation for each metric
mean_sensitivity <- mean(overall_sensitivity)
mean_specificity <- mean(overall_specificity)
mean_accuracy <- mean(overall_accuracy)
mean_auc <- mean(overall_auc)

stdv_sensitivity <- sd(overall_sensitivity)
stdv_specificity <- sd(overall_specificity)
stdv_accuracy <- sd(overall_accuracy)
stdv_auc <- sd(overall_auc)

# Print the overall results
cat("Mean Sensitivity:", mean_sensitivity, "Stdv:", stdv_sensitivity, "\n")
cat("Mean Specificity:", mean_specificity, "Stdv:", stdv_specificity, "\n")
cat("Mean Accuracy:", mean_accuracy, "Stdv:", stdv_accuracy, "\n")
cat("Mean AUC:", mean_auc, "Stdv:", stdv_auc, "\n")


