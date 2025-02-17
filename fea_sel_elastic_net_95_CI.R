rm(list = ls())
library(glmnet)
library(ggplot2)
library(boot)

# Load and prepare data
df <- read.csv(".../xxx.csv", header=TRUE)
df <- na.omit(df)

categorical_vars <- c("gender","bmiband","ageband","diabetesmellitusr","atrialfibrillationr","hypertension",
                      "chdfamilyhistory","chestpaindiagnosis","hyperlipidemia")
df[categorical_vars] <- lapply(df[categorical_vars], factor)

X <- model.matrix(~ . - PR - LAP - NRS - SC, data=df)
y <- df$PR
print(nrow(X))
print(length(y))

# Fit the model with cross-validation to find the optimal lambda
set.seed(123) # Ensure reproducibility
cv_fit <- cv.glmnet(X, y, type.measure="auc", alpha = 0.5, family = "binomial", nfolds=5) 
optimal_lambda <- cv_fit$lambda.min

# Function to extract coefficients from glmnet
glmnet_coef <- function(data, indices) {
  df <- data[indices, ]
  X <- model.matrix(~ . - PR - LAP - NRS - SC, data=df)
  y <- df$PR
  
  fit <- glmnet(X, y, alpha = 0.5, family = "binomial")
  as.vector(coef(fit, s = optimal_lambda)[-1])
}

# Bootstrap to estimate confidence intervals
set.seed(123)
boot_results <- boot(data = df, statistic = glmnet_coef, R = 1000)

# Calculate confidence intervals
ci_lower <- apply(boot_results$t, 2, quantile, 0.025)
ci_upper <- apply(boot_results$t, 2, quantile, 0.975)

# Extract coefficients and confidence intervals
optimal_coefs <- coef(cv_fit, s = "lambda.min")
coef_df <- data.frame(
  Feature = row.names(optimal_coefs)[-1],  
  Coefficient = optimal_coefs[-1, 1],  
  AbsoluteCoefficient = abs(optimal_coefs[-1, 1]),
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# Clip confidence intervals to avoid extreme values
coef_df$CI_Upper <- pmin(coef_df$CI_Upper, 2) 
coef_df$CI_Lower <- pmax(coef_df$CI_Lower, -3) 

# Select top 15 features
ordered_coef_df <- coef_df[order(-coef_df$AbsoluteCoefficient), ]
top_n_features <- head(ordered_coef_df, 13)
print(top_n_features)

# Plot with confidence intervals
top_n_features$Color <- ifelse(top_n_features$Coefficient > 0, "#2E86C1", "#E74C3C")
ggplot(top_n_features, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Color)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  coord_flip() +
  labs(title = "Top 13 Features from Elastic Net with 95% CI", x = "Feature", y = "Coefficient Value") +
  scale_fill_manual(values = c("#2E86C1" = "#2E86C1", "#E74C3C" = "#E74C3C")) +
  theme_minimal() +
  guides(fill = FALSE) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(-3, 2), breaks = seq(-3, 2, 1))



