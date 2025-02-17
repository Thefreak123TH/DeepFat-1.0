rm(list=ls())
library(randomForest)
library(caret)
library(pROC)

df <- read.csv(".../xxx.csv", header=TRUE)

df <- na.omit(df)
df$cadrads1 <- as.factor(df$cadrads1)  # Ensure the target variable is treated as a factor

num_iterations <- 10

# Initialize matrices to store metrics for each iteration
all_sensitivity <- all_specificity <- all_accuracy <- all_auc <- list()
all_prec <- all_f1 <- list()
all_predictions <- list()

set.seed(123)  

for(iter in 1:num_iterations) {
  folds <- createFolds(df$cadrads1, k = 5, list = TRUE, returnTrain = TRUE)
  
  sensitivity <- specificity <- accuracy <- auc <- numeric(length(folds))
  prec <- f1_score <- numeric(length(folds))
  
  fold_predictions <- list()
  
  for(i in seq_along(folds)) {
    train_indices <- folds[[i]]
    valid_indices <- setdiff(seq_len(nrow(df)), train_indices)
    
    train_data <- df[train_indices, ]
    valid_data <- df[valid_indices, ]
    
    model <- randomForest(cadrads1 ~ ., data = train_data[, c("cadrads1", names(df)[1:4])], ntree = 500)
    predictions <- predict(model, newdata = valid_data[, c("cadrads1", names(df)[1:4])], type = "prob")[,2]
    
    pred_labels <- ifelse(predictions > 0.5, 1, 0)
    actual_labels <- valid_data$cadrads1
    fold_predictions[[i]] <- data.frame(Actual = actual_labels, Prediction = pred_labels)
    
    tbl <- table(Predicted = pred_labels, Actual = actual_labels)
    
    TP <- tbl[2, 2]
    TN <- tbl[1, 1]
    FP <- tbl[2, 1]
    FN <- tbl[1, 2]
    
    sensitivity[i] <- if ((TP + FN) == 0) NA else TP / (TP + FN)
    
    specificity[i] <- if (sum(tbl[1,]) == 0) NA else tbl[1, 1] / sum(tbl[1,])
    accuracy[i] <- sum(diag(tbl)) / sum(tbl)
    
    # Calculate precision
    prec[i] <- if ((TP + FP) == 0) NA else TP / (TP + FP)
    
    # Calculate F1 score
    if (is.na(prec[i]) || is.na(sensitivity[i]) || (prec[i] + sensitivity[i]) == 0) {
      f1_score[i] <- NA
    } else {
      f1_score[i] <- 2 * ((prec[i] * sensitivity[i]) / (prec[i] + sensitivity[i]))
    }
    
    roc_result <- roc(response = actual_labels, predictor = as.numeric(predictions))
    auc[i] <- auc(roc_result)
  }
  
  # Store metrics from the current iteration
  all_sensitivity[[iter]] <- sensitivity
  all_specificity[[iter]] <- specificity
  all_accuracy[[iter]] <- accuracy
  all_prec[[iter]] <- prec
  all_f1[[iter]] <- f1_score
  all_auc[[iter]] <- auc
  all_predictions[[iter]] <- fold_predictions
}

# Calculate the mean and standard deviation across all iterations
mean_sensitivity <- mean(unlist(all_sensitivity), na.rm = TRUE)
mean_specificity <- mean(unlist(all_specificity))
mean_accuracy <- mean(unlist(all_accuracy))
mean_prec <- mean(unlist(all_prec), na.rm = TRUE)
mean_f1 <- mean(unlist(all_f1), na.rm = TRUE)
mean_auc <- mean(unlist(all_auc))

stdv_sensitivity <- sd(unlist(all_sensitivity), na.rm = TRUE)
stdv_specificity <- sd(unlist(all_specificity))
stdv_accuracy <- sd(unlist(all_accuracy))
stdv_prec <- sd(unlist(all_prec), na.rm = TRUE)
stdv_f1 <- sd(unlist(all_f1), na.rm = TRUE)
stdv_auc <- sd(unlist(all_auc))

# Print the overall results
cat("Overall Mean Precision:", mean_prec, "Stdv:", stdv_prec, "\n")
cat("Overall Mean Sensitivity:", mean_sensitivity, "Stdv:", stdv_sensitivity, "\n")
cat("Overall Mean Specificity:", mean_specificity, "Stdv:", stdv_specificity, "\n")
cat("Overall Mean Accuracy:", mean_accuracy, "Stdv:", stdv_accuracy, "\n")
cat("Overall Mean F1 Score:", mean_f1, "Stdv:", stdv_f1, "\n")
cat("Overall Mean AUC:", mean_auc, "Stdv:", stdv_auc, "\n")



