rm(list=ls())
library(catboost)
library(caret)
library(pROC)
library(ggplot2)

df <- read.csv(".../xxx.csv", header=TRUE)

#df <- data.frame(lapply(df, function(x) if(is.numeric(x)) x / max(x, na.rm = TRUE) else x))
df <- data.frame(lapply(df, as.numeric))

target_variable <- "PR"

# normalization
#numeric_vars <- sapply(df, is.numeric)
#numeric_vars[(length(numeric_vars)-4):length(numeric_vars)] <- FALSE  
#df[numeric_vars] <- scale(df[numeric_vars]) 

# Set parameters for CatBoost
params <- list(
  iterations = 300, # 50, 100, 1000
  learning_rate = 0.01, # 0.01 or 0.1
  depth = 6, # 3 or 6
  l2_leaf_reg = 5, # 3 or 5
  rsm = 0.75, # 0.5 or 0.75
  border_count = 64, # 32 or 64
  loss_function = 'Logloss', 
  eval_metric = 'Logloss', # Logloss, AUC, F1 / RMSE, MAE
  auto_class_weights = 'Balanced',
  use_best_model = TRUE,
  bootstrap_type = 'Bernoulli',
  subsample = 0.6,
  thread_count = 10
)

num_iterations <- 10

# Initialize lists to store metrics for all iterations
all_sensitivity <- all_specificity <- all_accuracy <- all_auc <- list()
all_prec <- all_f1 <- list()
all_predictions <- list()

for(iter in 1:num_iterations) {
#  folds <- createFolds(df$cadrads1, k = 5, list = TRUE, returnTrain = TRUE)
  folds <- createFolds(df[[target_variable]], k = 5, list = TRUE, returnTrain = TRUE)

  # Initialize vectors to store metrics for each fold of the current iteration
  sensitivity <- specificity <- accuracy <- auc <- numeric(length(folds))
  prec <- f1_score <- numeric(length(folds))
  
  fold_predictions <- list()
  
  for(i in seq_along(folds)) {
    train_indices <- folds[[i]]
    valid_indices <- setdiff(seq_len(nrow(df)), train_indices)
    
    # Prepare the data for CatBoost
    train_data <- df[train_indices, 1:14] 
    train_label <- df[[target_variable]][train_indices]

    valid_data <- df[valid_indices, 1:14]
    valid_label <- df[[target_variable]][valid_indices]

    # Create the pool objects
    train_pool <- catboost.load_pool(data = train_data, label = train_label)
    valid_pool <- catboost.load_pool(data = valid_data, label = valid_label)
    
    # Train the model
    model <- catboost.train(learn_pool = train_pool, params = params)
    
    # Make predictions
    predictions <- catboost.predict(model, valid_pool, prediction_type = "Probability")
    pred_labels <- ifelse(predictions > 0.5, 1, 0) # Probability 
    
    actual_labels <- df[[target_variable]][valid_indices]

    fold_predictions[[i]] <- data.frame(Actual = actual_labels, Prediction = pred_labels)

    # Create a full contingency table with all combinations to avoid dimension issues
    tbl <- table(factor(pred_labels, levels = c(0, 1)), factor(actual_labels, levels = c(0, 1)))
    
    TP <- tbl[2, 2]
    TN <- tbl[1, 1]
    FP <- tbl[2, 1]
    FN <- tbl[1, 2]
    
    # Update sensitivity calculation with conditional check
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


