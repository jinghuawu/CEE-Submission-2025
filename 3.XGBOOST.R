######### Multi-class Classification ######### XGBoost

# Load required libraries
library(xgboost)  
library(ggplot2)  
library(shapviz)  
library(caret)    
library(readxl)   
library(dummies)
library(multiROC)
library(pROC)
library(reshape2)
library(dplyr)
library(grf)

# Set working directory
setwd(dir = 'X:/XX')
getwd()

# Load dataset
mydata <- read_excel("CEE_imputed_results_modified.xlsx", col_names = TRUE) # Modify with actual file path
mydata <- subset(mydata, !(Group == "Fertile" & ZrHf > 25))  # Remove specific data based on condition
mydata <- mydata %>% select(-"Sample ID", -"ZrHf")  # Drop unnecessary columns
mydata <- as.data.frame(mydata)
mydata$Group <- as.factor(mydata$Group)  # Convert the target variable into a factor

# XGBoost model

# Data split: 70% for training, 30% for testing, using stratified sampling (based on class distribution)
set.seed(123) 
train_index <- createDataPartition(mydata$Group, p = 0.7, list = FALSE)

X_train <- mydata[train_index, ]  # Training data
train_data <- X_train[, -1]      # Features for training
train_label <- X_train$Group     # Labels for training

X_test <- mydata[-train_index, ]  # Test data
test_data <- X_test[, -1]         # Features for testing
test_label <- X_test$Group        # Labels for testing

dim(X_train)  # Summary of training data dimensions
dim(X_test)   # Summary of testing data dimensions

# Use trainControl to define cross-validation method
ctrl <- trainControl(method = "cv",      # Cross-validation method
                     number = 5)         # 5-fold cross-validation

# Define parameter grid
param_grid <- expand.grid(
  nrounds = c(100, 200, 300),            # Number of boosting rounds
  max_depth = c(3, 5, 7),                # Maximum tree depth
  eta = c(0.01, 0.1, 0.3),               # Learning rate
  gamma = c(0, 0.1, 0.2),                # Gamma parameter in the penalty term
  colsample_bytree = c(0.5, 0.7, 0.9),   # Proportion of features used by each tree
  min_child_weight = c(1, 5, 9),         # Minimum sum of child weights
  subsample = c(0.5, 0.7, 0.9)           # Proportion of samples for each tree
)

# Train the model and tune parameters using the train function
model <- train(Group ~ .,
               data = X_train,
               method = "xgbTree",                     # Use xgboost method
               trControl = ctrl,                       # Specify cross-validation method
               nthread = parallel::detectCores() - x,  # Leave x of CPU cores
               tuneGrid = param_grid                   # Specify parameter grid
)   

# Cross-validation results
cv_results <- model$resample
print(cv_results)

# Calculate cross-validation mean accuracy
cv_mean <- mean(cv_results$Accuracy)
print(paste("Cross-validation mean: ", round(cv_mean, 4)))

# Output best model and parameters
model

# Define custom model parameters
custom_params <- list(
  objective = "multi:softprob",  # Multi-class classification
  num_class = 2)                 # Number of classes

# Train using the best parameters
dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train_data), label = as.integer(train_label) - 1)
fit <- xgboost::xgb.train(data = dtrain, 
                          nrounds = 200,          # Number of boosting rounds
                          nthread = 8,            # Number of threads to use
                          max_depth = 5,          # Maximum tree depth
                          eta = 0.1,              # Learning rate
                          gamma = 0.2,            # Minimum loss reduction for tree splitting
                          colsample_bytree = 0.9, # Proportion of features for each tree
                          min_child_weight = 1,   # Minimum sum of child weights
                          subsample = 0.7,        # Proportion of samples
                          params = custom_params)   

# SHAP evaluation
x <- shapviz(fit, X_pred = as.matrix(X_train[,-1]), X = X_train)

sv_importance(x, kind = "bee", max_display = 20, alpha = 0.1) +
  coord_cartesian(xlim = c(-1.5, 1.5))  # Modify X-axis range

# Extract SHAP values for class 2
shap_class2 <- as.data.frame(x[[2]]$S)
colnames(shap_class2) <- colnames(train_data)             # Ensure column names match features
mean_shap_values <- colMeans(abs(shap_class2))            # Calculate average SHAP values for each feature

# Create data frame for SHAP summary
shap_summary <- data.frame(
  Feature = colnames(train_data),
  Mean_SHAP = mean_shap_values
)
shap_summary <- shap_summary %>% arrange(desc(Mean_SHAP)) # Sort by importance
print(shap_summary)                                       # Display top features

# Calculate SHAP contribution for class 2 features
shap_class2 <- as.data.frame(x[[2]]$S)
colnames(shap_class2) <- colnames(train_data)
mean_shap_values <- colMeans(abs(shap_class2))            # Average SHAP values
total_shap <- sum(mean_shap_values)                       # Total SHAP value (sum of contributions)
shap_summary <- data.frame(
  Feature = colnames(train_data),
  Mean_SHAP = mean_shap_values,
  SHAP_Contribution = mean_shap_values / total_shap       # Proportion of SHAP contribution
)

shap_summary <- shap_summary %>% arrange(desc(SHAP_Contribution)) # Sort by contribution
print(shap_summary)                                           # Display top features

# Add cumulative SHAP contribution
shap_summary <- shap_summary %>%
  mutate(Cumulative_Contribution = cumsum(SHAP_Contribution))  # Calculate cumulative SHAP contribution

print(head(shap_summary, 60))                                  # Display top features with cumulative contribution

############# Confusion Matrix #############
# Predict classes for test data
dtest <- xgboost::xgb.DMatrix(data = as.matrix(test_data), label = as.integer(test_label) - 1)
xgb.pred <- predict(fit, newdata = dtest, reshape = T)  # Get class probabilities
xgb.pred = as.data.frame(xgb.pred)
lv_group <- mydata$Group
colnames(xgb.pred) = levels(lv_group)
xgb.pred$prediction = apply(xgb.pred, 1, function(x) colnames(xgb.pred)[which.max(x)])

# Convert predictions to class labels
predicted_classes <- xgb.pred$prediction

# Convert to factor
predicted_classes <- factor(predicted_classes, levels = levels(X_test$Group))

# Create confusion matrix
conf_matrix <- confusionMatrix(predicted_classes, X_test$Group)

# Output confusion matrix results
print(conf_matrix)

# Extract Accuracy, Precision, Recall, F1 score
accuracy <- conf_matrix$overall['Accuracy']         # Accuracy
precision <- conf_matrix$byClass['Pos Pred Value']  # Precision for positive class
recall <- conf_matrix$byClass['Sensitivity']        # Recall
f1_score <- conf_matrix$byClass['F1']               # F1 score

# Print these metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Beautify confusion matrix visualization using ggplot2
conf_matrix_df <- as.data.frame(conf_matrix$table)
conf_matrix_melted <- melt(conf_matrix_df)

ggplot(conf_matrix_melted, 
       aes(x = Reference, y = Prediction, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue", n.breaks = 5) +
  labs(title = "Confusion Matrix", x = "Reference", y = "Prediction",
       fill = "Count") + theme_minimal()

############# ROC and PR Curve Plotting #############
# Merge data for ROC and PR curves
X_test <- as.data.frame(X_test)  # Convert to data frame
true_label <- dummies::dummy(X_test$Group, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
colnames(xgb.pred) <- paste(colnames(xgb.pred), "_pred_RF")
final_df <- cbind(true_label, xgb.pred)
final_df

# Generate ROC curve data
roc_res <- multi_roc(final_df, force_diag = T)
plot_roc_df <- plot_roc_data