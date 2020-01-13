#############################
#### Run analyses quickly ###
#############################

# Purpose: If need to run the notebook quickly to get to final model results
# just run this script. Efficient for making plots.

# Notes:
# Collapse — Alt+L
# Expand — Shift+Alt+L

# Libraries
library(data.table)
library(ggplot2)
library(xgboost)
library(cowplot)
library(ICEbox)
library(dplyr)
library(keras)
library(caret)
library(e1071)
library(dplyr)
library(corrr)
library(ROCR)
library(pdp)

### Model 1 - predicting health in 1 years time ###

### 1. Get the data ready for analyses ###

# Load UKHLS
source("./load_var_names.R") # Loads variable names by domain
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model1_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_1yr)] # Subset data

# Split data into train and test samples
set.seed(250388)
#train_split <- createDataPartition(model1_data$health_1yr, p = 0.75, list = FALSE, times = 1) # Split 80/20
#saveRDS(train_split, file = "./Data split files/train_split_health1yr.rds") # Save
train_split <- readRDS(file = "./Data split files/train_split_health1yr_model1.rds") # Load
train <- model1_data[train_split,] # Training data
test <- model1_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_1yr"] # Training data labels
test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_1yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model1_data[train_split,id]
test_data$id <- model1_data[-train_split,id]

# Load genetic data
source("./genetic_data_analysis_model1.R")

# Load variables to keep following feature reduction
source("./load_feature_selected_model1.R")


### 2. Neural Nets ###


# Define all variables to include
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)

# Define training control (i.e. define k-fold cross validation to be used below)
train_control <- trainControl(method = "cv", number = 4, savePredictions = TRUE) # 4 is same as used in neural nets

# Train the model (logistic regression using caret) 
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

temp <- cbind(train_labels, train_all[,lasso_all_vars,with=F]) # Subset variables needed
logit_train_all <- train(as.factor(health_1yr) ~ ., # Formula (the '.' means all columns in the data not stated in the formula)
                         data = temp, 
                         trControl = train_control, method = "glm", family = binomial(link = "logit")) 

# Evaluate model performance
test.probs <- predict(logit_train_all, test_all[,lasso_all_vars,with=F], type = "raw") # Predicted binary outcome based on logistic regression model on test data

# Model fit statistics
cm <- confusionMatrix(as.factor(test.probs), as.factor(test_labels$health_1yr), positive = "1") 

# Convert data to expected TensorFlow format
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_1yr # labels

# Because we will need to instantiate the same model multiple times, we use a function to construct it.
build_model <- function() {
  
  # Define network structure
  model1_all <- keras_model_sequential() %>% # State is sequential model (i.e. no reinforced learning)
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% # First layer. We include l1 regularisation to minimise potential for overfitting, although its affect is small. L1 regularisation was selected merely because it is the same as the LASSO regression.
    layer_dense(units = 1, activation = "sigmoid") # Output layer - generate a predicted prob
  
  model1_all %>% compile(
    optimizer = "rmsprop", # Optimiser - how the network will update itself during the training phase - in our case we will use Gradient Descent since has been shown to be most effective and efficient (this is one algorithm for it)
    loss = "binary_crossentropy", # Best choice for dealing with probabilities and binary labels. Crossentropy loss function measures the difference between the probability and label values.
    metrics = c("precision") # Evaluate model based on precision 
  )
  
}

# k-fold validation (k=4)
k <- 4 # Define k
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) # Identify how to split data into k folds

num_epochs <- 20 # Number of epochs
all_scores <- c()
for (i in 1:k) {
  
  cat("processing fold #", i, "\n") # print progress
  
  # Prepare the validation data from partition #k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_inputs <- dm_train[val_indices,]
  val_labels <- train_y[val_indices]
  
  # Prepare the training data using data from all other partitions
  partial_train_inputs <- dm_train[-val_indices,]
  partial_train_labels <- train_y[-val_indices]
  
  # Build the Keras model (already compiled previously)
  model1_all <- build_model()
  
  # Train the model (in silent mode: verbose=0)
  model1_all %>% fit(partial_train_inputs, partial_train_labels,
                     epochs = num_epochs, batch_size = 128, verbose = 0)
  
  # Evaluate the model on the validation data
  results <- model1_all %>% evaluate(val_inputs, val_labels, verbose = 0)
  
  # Save each epoch score/information
  all_scores <- c(all_scores, results$precision)
  
} 

dm_test <- as.matrix(test_all[,lasso_all_vars,with=F])
test_y <- test_labels$health_1yr 
metrics <- model1_all %>% evaluate(dm_test, test_y, batch_size = 128, verbose = 0)
metrics

# Predict outcomes
pred_y <- model1_all %>% predict(dm_test, batch_size=128, verbose=0)
pred.nn <- rep(0,length(pred_y))
pred.nn[pred_y>=0.5] <- 1
cm2 <- confusionMatrix(as.factor(pred.nn), as.factor(test_y), positive = "1")  # Calculate model fit measures


### 3. Create plots for neural nets ###


# Create plots
pred_y <- model1_all %>% predict(dm_train, batch_size=128, verbose=0) # Predicted y's
corrr_analysis <- train_all[,lasso_all_vars,with=F] %>% # Test data
  mutate(outcome = pred_y) %>% # Get predicted outcome
  correlate() %>% # Calcuate correlations
  focus(outcome) %>% # For rest of lines just edit this variable
  rename(feature = rowname) %>% # Rename
  arrange(abs(outcome))

# We have a lot of variables so subset only those with correlations +- 0.2
hold <- as.data.table(corrr_analysis)
hold2 <- hold[hold$outcome >= 0.2 | hold$outcome <= -0.2]

# Plot
cor_plot_model1 <- ggplot(hold2, aes(x = outcome, y = reorder(feature, desc(outcome)))) +
  geom_point() +
  # Positive Correlations - Contribute to LLTI (i.e. predict 1)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Negative Correlations - Prevent LLTI (i.e. predict 0)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Aesthetics
  labs(y = "Feature", x = "Correlation")
cor_plot_model1

# Save data for later
dm_train_m1 <- dm_train

### Model 2 - predicting health in 1 years time with no ill health at baseline ###

### 1. Get the data ready for analyses ###

# Load UKHLS
source("./load_var_names.R") # Loads variable names by domain
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model2_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_1yr) & ukhls$health_0yr == 0] # Subset data

# Split data into train and test samples
set.seed(250388)
#train_split <- createDataPartition(model2_data$health_1yr, p = 0.75, list = FALSE, times = 1) # Split 80/20
#saveRDS(train_split, file = "./Data split files/train_split_health1yr.rds") # Save
train_split <- readRDS(file = "./Data split files/train_split_health1yr_model2.rds") # Load
train <- model2_data[train_split,] # Training data
test <- model2_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_1yr"] # Training data labels
test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_1yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model2_data[train_split,id]
test_data$id <- model2_data[-train_split,id]

# Load genetic data
source("./genetic_data_analysis_model2.R")

# Load variables to keep following feature reduction
source("./load_feature_selected_model2.R")


### 2. Neural Nets ###


# Define all variables to include
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)

# Define training control (i.e. define k-fold cross validation to be used below)
train_control <- trainControl(method = "cv", number = 4, savePredictions = TRUE) # 4 is same as used in neural nets

# Train the model (logistic regression using caret) 
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

temp <- cbind(train_labels, train_all[,lasso_all_vars,with=F]) # Subset variables needed
logit_train_all <- train(as.factor(health_1yr) ~ ., # Formula (the '.' means all columns in the data not stated in the formula)
                         data = temp, 
                         trControl = train_control, method = "glm", family = binomial(link = "logit")) 

# Evaluate model performance
test.probs <- predict(logit_train_all, test_all[,lasso_all_vars,with=F], type = "raw") # Predicted binary outcome based on logistic regression model on test data

# Model fit statistics
cm <- confusionMatrix(as.factor(test.probs), as.factor(test_labels$health_1yr), positive = "1") 

# Convert data to expected TensorFlow format
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_1yr # labels

# Because we will need to instantiate the same model multiple times, we use a function to construct it.
build_model <- function() {
  
  # Define network structure
  model2_all <- keras_model_sequential() %>% # State is sequential model (i.e. no reinforced learning)
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% # First layer. We include l1 regularisation to minimise potential for overfitting, although its affect is small. L1 regularisation was selected merely because it is the same as the LASSO regression.
    layer_dense(units = 1, activation = "sigmoid") # Output layer - generate a predicted prob
  
  model2_all %>% compile(
    optimizer = "rmsprop", # Optimiser - how the network will update itself during the training phase - in our case we will use Gradient Descent since has been shown to be most effective and efficient (this is one algorithm for it)
    loss = "binary_crossentropy", # Best choice for dealing with probabilities and binary labels. Crossentropy loss function measures the difference between the probability and label values.
    metrics = c("precision") # Evaluate model based on precision 
  )
  
}

# k-fold validation (k=4)
k <- 4 # Define k
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) # Identify how to split data into k folds

num_epochs <- 20 # Number of epochs
all_scores <- c()
for (i in 1:k) {
  
  cat("processing fold #", i, "\n") # print progress
  
  # Prepare the validation data from partition #k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_inputs <- dm_train[val_indices,]
  val_labels <- train_y[val_indices]
  
  # Prepare the training data using data from all other partitions
  partial_train_inputs <- dm_train[-val_indices,]
  partial_train_labels <- train_y[-val_indices]
  
  # Build the Keras model (already compiled previously)
  model2_all <- build_model()
  
  # Train the model (in silent mode: verbose=0)
  model2_all %>% fit(partial_train_inputs, partial_train_labels,
                     epochs = num_epochs, batch_size = 128, verbose = 0)
  
  # Evaluate the model on the validation data
  results <- model2_all %>% evaluate(val_inputs, val_labels, verbose = 0)
  
  # Save each epoch score/information
  all_scores <- c(all_scores, results$precision)
  
} 

dm_test <- as.matrix(test_all[,lasso_all_vars,with=F])
test_y <- test_labels$health_1yr 
metrics <- model2_all %>% evaluate(dm_test, test_y, batch_size = 128, verbose = 0)
metrics

# Predict outcomes
pred_y <- model2_all %>% predict(dm_test, batch_size=128, verbose=0)
pred.nn <- rep(0,length(pred_y))
pred.nn[pred_y>=0.5] <- 1
cm2 <- confusionMatrix(as.factor(pred.nn), as.factor(test_y), positive = "1")  # Calculate model fit measures


### 3. Create plots for neural nets ###


# Create plots
pred_y <- model2_all %>% predict(dm_train, batch_size=128, verbose=0) # Predicted y's
corrr_analysis <- train_all[,lasso_all_vars,with=F] %>% # Test data
  mutate(outcome = pred_y) %>% # Get predicted outcome
  correlate() %>% # Calcuate correlations
  focus(outcome) %>% # For rest of lines just edit this variable
  rename(feature = rowname) %>% # Rename
  arrange(abs(outcome))

# We have a lot of variables so subset only those with correlations +- 0.2
hold <- as.data.table(corrr_analysis)
hold2 <- hold[hold$outcome >= 0.2 | hold$outcome <= -0.2]

# Plot
cor_plot_model2 <- ggplot(hold2, aes(x = outcome, y = reorder(feature, desc(outcome)))) +
  geom_point() +
  # Positive Correlations - Contribute to LLTI (i.e. predict 1)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Negative Correlations - Prevent LLTI (i.e. predict 0)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Aesthetics
  labs(y = "Feature", x = "Correlation")
cor_plot_model2

# Save data for later
dm_train_m2 <- dm_train

### Model 3 - predicting health in 5 years time ###

### 1. Get the data ready for analyses ###

# Load UKHLS
source("./load_var_names.R") # Loads variable names by domain
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model3_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_5yr)] # Subset data

# Split data into train and test samples
set.seed(250388)
#train_split <- createDataPartition(model3_data$health_5yr, p = 0.75, list = FALSE, times = 1) # Split 80/20
#saveRDS(train_split, file = "./Data split files/train_split_health1yr.rds") # Save
train_split <- readRDS(file = "./Data split files/train_split_health5yr_model3.rds") # Load
train <- model3_data[train_split,] # Training data
test <- model3_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_5yr"] # Training data labels
test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_5yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model3_data[train_split,id]
test_data$id <- model3_data[-train_split,id]

# Load genetic data
source("./genetic_data_analysis_model3.R")

# Load variables to keep following feature reduction
source("./load_feature_selected_model3.R")


### 2. Neural Nets ###


# Define all variables to include
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)

# Define training control (i.e. define k-fold cross validation to be used below)
train_control <- trainControl(method = "cv", number = 4, savePredictions = TRUE) # 4 is same as used in neural nets

# Train the model (logistic regression using caret) 
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

temp <- cbind(train_labels, train_all[,lasso_all_vars,with=F]) # Subset variables needed
logit_train_all <- train(as.factor(health_5yr) ~ ., # Formula (the '.' means all columns in the data not stated in the formula)
                         data = temp, 
                         trControl = train_control, method = "glm", family = binomial(link = "logit")) 

# Evaluate model performance
test.probs <- predict(logit_train_all, test_all[,lasso_all_vars,with=F], type = "raw") # Predicted binary outcome based on logistic regression model on test data

# Model fit statistics
cm <- confusionMatrix(as.factor(test.probs), as.factor(test_labels$health_5yr), positive = "1") 

# Convert data to expected TensorFlow format
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_5yr # labels

# Because we will need to instantiate the same model multiple times, we use a function to construct it.
build_model <- function() {
  
  # Define network structure
  model3_all <- keras_model_sequential() %>% # State is sequential model (i.e. no reinforced learning)
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% # First layer. We include l1 regularisation to minimise potential for overfitting, although its affect is small. L1 regularisation was selected merely because it is the same as the LASSO regression.
    layer_dense(units = 1, activation = "sigmoid") # Output layer - generate a predicted prob
  
  model3_all %>% compile(
    optimizer = "rmsprop", # Optimiser - how the network will update itself during the training phase - in our case we will use Gradient Descent since has been shown to be most effective and efficient (this is one algorithm for it)
    loss = "binary_crossentropy", # Best choice for dealing with probabilities and binary labels. Crossentropy loss function measures the difference between the probability and label values.
    metrics = c("precision") # Evaluate model based on precision 
  )
  
}

# k-fold validation (k=4)
k <- 4 # Define k
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) # Identify how to split data into k folds

num_epochs <- 20 # Number of epochs
all_scores <- c()
for (i in 1:k) {
  
  cat("processing fold #", i, "\n") # print progress
  
  # Prepare the validation data from partition #k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_inputs <- dm_train[val_indices,]
  val_labels <- train_y[val_indices]
  
  # Prepare the training data using data from all other partitions
  partial_train_inputs <- dm_train[-val_indices,]
  partial_train_labels <- train_y[-val_indices]
  
  # Build the Keras model (already compiled previously)
  model3_all <- build_model()
  
  # Train the model (in silent mode: verbose=0)
  model3_all %>% fit(partial_train_inputs, partial_train_labels,
                     epochs = num_epochs, batch_size = 128, verbose = 0)
  
  # Evaluate the model on the validation data
  results <- model3_all %>% evaluate(val_inputs, val_labels, verbose = 0)
  
  # Save each epoch score/information
  all_scores <- c(all_scores, results$precision)
  
} 

dm_test <- as.matrix(test_all[,lasso_all_vars,with=F])
test_y <- test_labels$health_5yr 
metrics <- model3_all %>% evaluate(dm_test, test_y, batch_size = 128, verbose = 0)
metrics

# Predict outcomes
pred_y <- model3_all %>% predict(dm_test, batch_size=128, verbose=0)
pred.nn <- rep(0,length(pred_y))
pred.nn[pred_y>=0.5] <- 1
cm2 <- confusionMatrix(as.factor(pred.nn), as.factor(test_y), positive = "1")  # Calculate model fit measures


### 3. Create plots for neural nets ###


# Create plots
pred_y <- model3_all %>% predict(dm_train, batch_size=128, verbose=0) # Predicted y's
corrr_analysis <- train_all[,lasso_all_vars,with=F] %>% # Test data
  mutate(outcome = pred_y) %>% # Get predicted outcome
  correlate() %>% # Calcuate correlations
  focus(outcome) %>% # For rest of lines just edit this variable
  rename(feature = rowname) %>% # Rename
  arrange(abs(outcome))

# We have a lot of variables so subset only those with correlations +- 0.2
hold <- as.data.table(corrr_analysis)
hold2 <- hold[hold$outcome >= 0.2 | hold$outcome <= -0.2]

# Plot
cor_plot_model3 <- ggplot(hold2, aes(x = outcome, y = reorder(feature, desc(outcome)))) +
  geom_point() +
  # Positive Correlations - Contribute to LLTI (i.e. predict 1)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Negative Correlations - Prevent LLTI (i.e. predict 0)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Aesthetics
  labs(y = "Feature", x = "Correlation")
cor_plot_model3

# Save data for later
dm_train_m3 <- dm_train

### Model 4 - predicting health in 5 years time with no ill health at baseline ###

### 1. Get the data ready for analyses ###

# Load UKHLS
source("./load_var_names.R") # Loads variable names by domain
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model4_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_5yr) & ukhls$health_0yr == 0]  # Subset data

# Split data into train and test samples
set.seed(250388)
#train_split <- createDataPartition(model4_data$health_5yr, p = 0.75, list = FALSE, times = 1) # Split 80/20
#saveRDS(train_split, file = "./Data split files/train_split_health1yr.rds") # Save
train_split <- readRDS(file = "./Data split files/train_split_health5yr_model4.rds") # Load
train <- model4_data[train_split,] # Training data
test <- model4_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_5yr"] # Training data labels
test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_5yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model4_data[train_split,id]
test_data$id <- model4_data[-train_split,id]

# Load genetic data
source("./genetic_data_analysis_model4.R")

# Load variables to keep following feature reduction
source("./load_feature_selected_model4.R")


### 2. Neural Nets ###


# Define all variables to include
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)

# Define training control (i.e. define k-fold cross validation to be used below)
train_control <- trainControl(method = "cv", number = 4, savePredictions = TRUE) # 4 is same as used in neural nets

# Train the model (logistic regression using caret) 
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

temp <- cbind(train_labels, train_all[,lasso_all_vars,with=F]) # Subset variables needed
logit_train_all <- train(as.factor(health_5yr) ~ ., # Formula (the '.' means all columns in the data not stated in the formula)
                         data = temp, 
                         trControl = train_control, method = "glm", family = binomial(link = "logit")) 

# Evaluate model performance
test.probs <- predict(logit_train_all, test_all[,lasso_all_vars,with=F], type = "raw") # Predicted binary outcome based on logistic regression model on test data

# Model fit statistics
cm <- confusionMatrix(as.factor(test.probs), as.factor(test_labels$health_5yr), positive = "1") 

# Convert data to expected TensorFlow format
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_5yr # labels

# Because we will need to instantiate the same model multiple times, we use a function to construct it.
build_model <- function() {
  
  # Define network structure
  model4_all <- keras_model_sequential() %>% # State is sequential model (i.e. no reinforced learning)
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% # First layer. We include l1 regularisation to minimise potential for overfitting, although its affect is small. L1 regularisation was selected merely because it is the same as the LASSO regression.
    layer_dense(units = 1, activation = "sigmoid") # Output layer - generate a predicted prob
  
  model4_all %>% compile(
    optimizer = "rmsprop", # Optimiser - how the network will update itself during the training phase - in our case we will use Gradient Descent since has been shown to be most effective and efficient (this is one algorithm for it)
    loss = "binary_crossentropy", # Best choice for dealing with probabilities and binary labels. Crossentropy loss function measures the difference between the probability and label values.
    metrics = c("precision") # Evaluate model based on precision 
  )
  
}

# k-fold validation (k=4)
k <- 4 # Define k
indices <- sample(1:nrow(train_data))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) # Identify how to split data into k folds

num_epochs <- 20 # Number of epochs
all_scores <- c()
for (i in 1:k) {
  
  cat("processing fold #", i, "\n") # print progress
  
  # Prepare the validation data from partition #k
  val_indices <- which(folds == i, arr.ind = TRUE) 
  val_inputs <- dm_train[val_indices,]
  val_labels <- train_y[val_indices]
  
  # Prepare the training data using data from all other partitions
  partial_train_inputs <- dm_train[-val_indices,]
  partial_train_labels <- train_y[-val_indices]
  
  # Build the Keras model (already compiled previously)
  model4_all <- build_model()
  
  # Train the model (in silent mode: verbose=0)
  model4_all %>% fit(partial_train_inputs, partial_train_labels,
                     epochs = num_epochs, batch_size = 128, verbose = 0)
  
  # Evaluate the model on the validation data
  results <- model4_all %>% evaluate(val_inputs, val_labels, verbose = 0)
  
  # Save each epoch score/information
  all_scores <- c(all_scores, results$precision)
  
} 

dm_test <- as.matrix(test_all[,lasso_all_vars,with=F])
test_y <- test_labels$health_5yr 
metrics <- model4_all %>% evaluate(dm_test, test_y, batch_size = 128, verbose = 0)
metrics

# Predict outcomes
pred_y <- model4_all %>% predict(dm_test, batch_size=128, verbose=0)
pred.nn <- rep(0,length(pred_y))
pred.nn[pred_y>=0.5] <- 1
cm2 <- confusionMatrix(as.factor(pred.nn), as.factor(test_y), positive = "1")  # Calculate model fit measures


### 3. Create plots for neural nets ###


# Create plots
pred_y <- model4_all %>% predict(dm_train, batch_size=128, verbose=0) # Predicted y's
corrr_analysis <- train_all[,lasso_all_vars,with=F] %>% # Test data
  mutate(outcome = pred_y) %>% # Get predicted outcome
  correlate() %>% # Calcuate correlations
  focus(outcome) %>% # For rest of lines just edit this variable
  rename(feature = rowname) %>% # Rename
  arrange(abs(outcome))

# We have a lot of variables so subset only those with correlations +- 0.2
hold <- as.data.table(corrr_analysis)
hold2 <- hold[hold$outcome >= 0.2 | hold$outcome <= -0.2]

# Plot
cor_plot_model4 <- ggplot(hold2, aes(x = outcome, y = reorder(feature, desc(outcome)))) +
  geom_point() +
  # Positive Correlations - Contribute to LLTI (i.e. predict 1)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Negative Correlations - Prevent LLTI (i.e. predict 0)
  geom_segment(aes(xend = 0, yend = feature), data = hold2) +
  geom_point(data = hold2) +
  # Aesthetics
  labs(y = "Feature", x = "Correlation")
cor_plot_model4


# Save data for later
dm_train_m4 <- dm_train

### Making publishable plots ###

# Figure 1 #

# Create draft plot for review
theme_set(theme_grey())
plot_grid(cor_plot_model1, cor_plot_model2, cor_plot_model3, cor_plot_model4, labels = c("1", "2", "3", "4"), ncol = 2,
          align = "h", label_size = 12)

# Change the y-axis labels and tidy up to same dimensions

# Model 1
labels1 <- c(# Positive
             "Age", "Glycated haemoglobin", "Marital status", "Receives no benefits", "Receives child benefit", "Walked 10m", "Clauss fibrinogen",
             "Times married", "Has mobile phone", "Age quit smoking", "Waist circumference", 
             # Negative
             "Lung capacity ratio", "Can afford a holiday", "Mother's education", "Arthritis", "Alcohol spend", "Food spend", "Grip Strength (non-dom.)", "Grip Strength (dom.)",
             "Problems paying for house", "Works overtime", "Receives housing benefit", "Lung capacity peak", "Education", "Eating out spend", "Lung capacity FVC",
             "Number of cars", "Depressed about job", "Insulin-like-growth factor 1", "Receives incapacity benefits", "Worried about job", "Moderate intensity sport",
             "Lung capacity FEV", "Household size", "Hours work", "Walk pace", "Dihydroepiandrosterone sulphate", "Gross pay", "Physicality of work", "Receives state pension",
             "Job security",  "Job permanent")
cor_plot_model1 <- cor_plot_model1 + scale_y_discrete(labels = labels1) + xlim(-0.6,0.6) + theme(axis.text.y = element_text(size=8))

# Model 2
labels2 <- c("Age", "Marital status", "Has mobile phone", "Moderate intensity sport",  
             "Insulin-like-growth factor 1", "Automony over job tasks", "Household size", "Lung capacity FEV", "Job permanent", 
             "Physicality of work", "Dihydroepiandrosterone sulphate", "Receives state pension")
cor_plot_model2 <- cor_plot_model2 + scale_y_discrete(labels = labels2) + xlim(-0.6,0.6) + theme(axis.text.y = element_text(size=8))

# Model 3
labels3 <- c(# Positive
             "Age", "Glycated haemoglobin", "Walked 10m", "Clauss fibrinogen", "Waist circumference", "Marital status",
             "Receives child benefit", "C-reactive protein", "Receives no benefits",
             # Negative
             "Problems paying for house", "Mother's education", "Depressed about job", "Works overtime", "Eating out spend", "Education", 
             "Grip Strength (non-dom.)", "Grip Strength (dom.)", "Lung capacity FVC", "Household size", "Gloomy about job", "Job satisfaction", 
             "Arthritis", "Managerial duties", "Lung capacity FEV", "Insulin-like-growth factor 1", "Occupation", "Works weekends",
             "Moderate intensity sport", "Gross pay", "Dihydroepiandrosterone sulphate", "Job security", "Physicality of work",
             "Receives incapacity benefits",  "Job permanent", "Walk pace")
cor_plot_model3 <- cor_plot_model3 + scale_y_discrete(labels = labels3) + xlim(-0.6,0.6) + theme(axis.text.y = element_text(size=8))

# Model 4
labels4 <- c("Age", "Receives child benefit", "Times married", "Lung capacity FEV", "Education", "Number of cars", "Moderate intensity sport", 
             "Mother's education", "Automony over job tasks", "Problems paying for house", "Job security", "Household size", 
             "Job permanent", "Occupation", "Physicality of work", "Insulin-like-growth factor 1", "Dihydroepiandrosterone sulphate")
cor_plot_model4 <- cor_plot_model4 + scale_y_discrete(labels = labels4) + xlim(-0.6,0.6) + theme(axis.text.y = element_text(size=8))

# Final plot
fig1 <- plot_grid(cor_plot_model1, cor_plot_model3, cor_plot_model2, cor_plot_model4, labels = c("1", "2", "3", "4"), ncol = 2,
                  align = "h", label_size = 12)
fig1
ggsave("./Plots/figure1.tiff", fig1, dpi = 300)


# Appendices

# Model 1 #

# Job permanent
ice_jbperm_model1 <- ice(object = model1_all, X = dm_train_m1, predictor = "jbperm", frac_to_build = 1)  # Build ICE curves
tiff("./Plots/appendix_c1a.tiff") # Save plot
plot(ice_jbperm_model1, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
              plot_pdp = T, main = "Job permanent", centered = T, x_quantile = F, prop_range_y = FALSE)
# frac_to_build - % randomly selected to build plot - can take a smaller proportion to save time or larger for greater precision (1 max)
dev.off()

# Job security
ice_b_jbsec_model1 <- ice(object = model1_all, X = dm_train_m1, predictor = "b_jbsec", frac_to_build = 1) 
tiff("./Plots/appendix_c1b.tiff")
plot(ice_b_jbsec_model1, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
              plot_pdp = T, main = "Job security", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Age
ice_age_model1 <- ice(object = model1_all, X = dm_train_m1, predictor = "age", frac_to_build = 1) 
tiff("./Plots/appendix_c1c.tiff")
plot(ice_age_model1, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Age", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Hba1c
ice_hba1c_model1 <- ice(object = model1_all, X = dm_train_m1, predictor = "hba1c", frac_to_build = 1)
tiff("./Plots/appendix_c1d.tiff")
plot(ice_hba1c_model1, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial prediction", plot_orig_pts_preds = T,
              plot_pdp = T, main = "Glycated haemoglobin", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()


# Model 2 #

# Pension
ice_ben4_model2 <- ice(object = model2_all, X = dm_train_m2, predictor = "benefit4", frac_to_build = 1) 
tiff("./Plots/appendix_c2a.tiff")
plot(ice_ben4_model2, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Receives state pension", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# DHeas
ice_wkphys_model2 <- ice(object = model2_all, X = dm_train_m2, predictor = "b_wkphys", frac_to_build = 1) 
tiff("./Plots/appendix_c2b.tiff")
plot(ice_wkphys_model2, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Physicality of work", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Age
ice_age_model2 <- ice(object = model2_all, X = dm_train_m2, predictor = "age", frac_to_build = 1) 
tiff("./Plots/appendix_c2c.tiff")
plot(ice_age_model2, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Age", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Marital status
ice_marstat_model2 <- ice(object = model2_all, X = dm_train_m2, predictor = "marstat", frac_to_build = 1)
tiff("./Plots/appendix_c2d.tiff")
plot(ice_marstat_model2, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Marital status", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()


# Model 3 #

# Walk pace 
ice_wlkpace_model3 <- ice(object = model3_all, X = dm_train_m3, predictor = "b_walkpace", frac_to_build = 1) 
tiff("./Plots/appendix_c3a.tiff")
plot(ice_wlkpace_model3, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Walk pace", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Job permanent
ice_jbperm_model3 <- ice(object = model3_all, X = dm_train_m3, predictor = "jbperm", frac_to_build = 1) 
tiff("./Plots/appendix_c3b.tiff")
plot(ice_jbperm_model3, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Job permanent", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Age
ice_age_model3 <- ice(object = model3_all, X = dm_train_m3, predictor = "age", frac_to_build = 1) 
tiff("./Plots/appendix_c3c.tiff")
plot(ice_age_model3, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Age", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# hba1c
ice_hb1ac_model3 <- ice(object = model3_all, X = dm_train_m3, predictor = "hba1c", frac_to_build = 1)
tiff("./Plots/appendix_c3d.tiff")
plot(ice_hb1ac_model3, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Glycated haemoglobin", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()


# Model 4 #

# Dheas
ice_aheas_model4 <- ice(object = model4_all, X = dm_train_m4, predictor = "dheas", frac_to_build = 1) 
tiff("./Plots/appendix_c4a.tiff")
plot(ice_aheas_model4, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Dihydroepiandrosterone sulphate", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# IFG1
ice_igfi_model4 <- ice(object = model4_all, X = dm_train_m4, predictor = "igfi", frac_to_build = 1) 
tiff("./Plots/appendix_c4b.tiff")
plot(ice_igfi_model4, frac_to_plot = 0.05, xlab = "Standardised value", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Insulin-like-growth factor 1", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Age
ice_age_model4 <- ice(object = model4_all, X = dm_train_m4, predictor = "age", frac_to_build = 1) 
tiff("./Plots/appendix_c4c.tiff")
plot(ice_age_model4, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial Prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Age", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

# Child benefits
ice_benefit5_model4 <- ice(object = model4_all, X = dm_train_m4, predictor = "benefit5", frac_to_build = 1)
tiff("./Plots/appendix_c4d.tiff")
plot(ice_benefit5_model4, frac_to_plot = 0.05, xlab = "Standardised values", ylab = "Partial prediction", plot_orig_pts_preds = T,
     plot_pdp = T, main = "Receives child benefit", centered = T, x_quantile = F, prop_range_y = FALSE)
dev.off()

### XGBoost Variable Importance Plots ###

# Load variable importance data
xgb_m1 <- read.csv("./Model fit statistics/xgb_featureimport_model1.csv")
xgb_m2 <- read.csv("./Model fit statistics/xgb_featureimport_model2.csv")
xgb_m3 <- read.csv("./Model fit statistics/xgb_featureimport_model3.csv")
xgb_m4 <- read.csv("./Model fit statistics/xgb_featureimport_model4.csv")

# Variables include
# Frequency: The number of times a feature is used to split the data across all trees.
# Cover: The number of times a feature is used to split the data across all trees weighted by the number of training data points that go through those splits.
# Gain; The average training loss reduction gained when using a feature for splitting/improvement in accuracy brought by a feature to the branches it is on

# Model 1
labels1 <- c("Diabetes", "Asthma", "Job permanent", "Physicality of work", # Define labels for plot
             "Dihydroepiandrosterone sulphate", "Arthritis", "Glycated haemoglobin", "Walk pace", "Receives incapacity benefits", "Age")
xgb_p1 <- xgb_m1 %>%
            top_n(10, Gain) %>% # Select top 10 gain values
            ggplot(aes(x = reorder(Feature, Gain), y = Gain)) + # Plot (order by value of Gain)
            geom_bar(stat="identity") +  
            coord_flip() +
            xlab("Feature") +
            scale_x_discrete(labels = labels1) +
            ylim(0,0.25)

# Model 2
labels2 <- c("Walked 30m", "Urea", "Lung capacity FEV", "Arthritis", 
             "Body Mass Index", "Haemoglobin", "Triglycerides", "Glycated haemoglobin", "Dihydroepiandrosterone sulphate", "Age")
xgb_p2 <- xgb_m2 %>%
  top_n(10, Gain) %>% 
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) + 
  geom_bar(stat="identity") +  
  coord_flip() +
  xlab("Feature") +
  scale_x_discrete(labels = labels2) +
  ylim(0,0.25)

# Model 3
labels3 <- c("Lung capacity FEV", "Waist circumference", "Diabetes", "Dihydroepiandrosterone sulphate", "Receives incapacity benefits",
             "Glycated haemoglobin", "Depressed about job", "Walk pace", "Age", "Arthritis")
xgb_p3 <- xgb_m3 %>%
  top_n(10, Gain) %>% 
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) + 
  geom_bar(stat="identity") +  
  coord_flip() +
  xlab("Feature") +
  scale_x_discrete(labels = labels3) +
  ylim(0,0.25)

# Model 4
labels4 <- c("Moderate intensity sport", "Dihydroepiandrosterone sulphate", "Grip Strength (dom.)", 
             "Alkaline phosphatase", "Ferritin", "Cholesertol", "Lung capacity ratio", "Creatinine", "Waist circumference", "Age")
xgb_p4 <- xgb_m4 %>%
  top_n(10, Gain) %>% 
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat="identity") +  
  coord_flip() +
  xlab("Feature") +
  scale_x_discrete(labels = labels4) +
  ylim(0,0.25)


# Create single plot
theme_set(theme_classic())
fig2 <- plot_grid(xgb_p1, xgb_p3, xgb_p2, xgb_p4, labels = c("1", "2", "3", "4"), ncol = 2,
                  align = "h", label_size = 12)
fig2
ggsave("./Plots/figure2.tiff", fig2, dpi = 300)


### XGBoost ICE Plots ###

# Model 1 #

# Load data
source("./load_var_names.R")
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model1_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_1yr)] 
set.seed(250388)
train_split <- readRDS(file = "./Data split files/train_split_health1yr_model1.rds") # Load
train <- model1_data[train_split,] # Training data
test <- model1_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_1yr"] # Training data labels

test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_1yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model1_data[train_split,id]
test_data$id <- model1_data[-train_split,id]

source("./load_feature_selected_model1.R")
source("./genetic_data_analysis_model1.R")
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

# Get training data ready
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_1yr # labels
dtrain <- xgb.DMatrix(data = dm_train, label = train_y) #  Change to XGBoost format

# Get test data ready
dm_test <- as.matrix(test_all[,lasso_all_vars,with=F]) # inputs
test_y <- test_labels$health_1yr # labels
dtest <- xgb.DMatrix(data = dm_test, label = test_y) # Change to XGBoost format

set.seed(250388) # set every time run model

# Watchlist to check for overfitting
watchlist <- list(train=dtrain, test=dtest) # Print train and test error through model iterations
# XGBoost has regularisation built in to prevent overfitting

# List paramters
# eta 0.05 rounds 5000 - low learning but longer period to learn
params <- list(
  booster = "gbtree", # Which booster to use - we use the tree one
  objective = "binary:logistic", # Outcome is binary - give predicted probability
  eta = 0.05, # Learning rate - prevent overfitting by adding penalty for additional trees added to a model (i.e. shrinks weights with each additional trees)
  gamma = 0, # Minimum loss reduction required to make a further partition on a leaf node of the tree
  max_depth = 6, # How deep a tree can be (i.e. 6 would be the extent of interactions); higher values may lead to overfitting due to greater likelihood of finding specific relationships linked to the training data
  min_child_weight = 1, # Minimum sum of weight - for additonal leaf node, needs to add so much; controls overfitting - higher values will prevent a model becoming too specific to the training data (too low and it will underfit)
  max_delta_step = 0, # Contraint of tree's weight - 0 is none, but positive valuses useful for imbalanced data (i.e. more 0s than 1s)
  subsample = 1, # If 0.5, then collect half the data to sample from to be computationally efficient
  colsample_bytree = 1, # As above but for columns (variables)
  alpha = 0.001 # L1 regularisation as used in the neural nets
)


# Train final model
xgb1 <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100, 
  maximize = F , 
  eval_metric = "map" # Binary classification error rate. It is calculated as #(wrong cases)/#(all cases). Could also use "auc" (area under curve) or "map" (mean average precision)
)

# Save training data
dm_train_xgb_m1 <- dm_train

# Model 2 #

# Load data
source("./load_var_names.R")
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model2_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_1yr) & ukhls$health_0yr == 0]
set.seed(250388)
train_split <- readRDS(file = "./Data split files/train_split_health1yr_model2.rds") # Load
train <- model2_data[train_split,] # Training data
test <- model2_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_1yr"] # Training data labels

test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_1yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model2_data[train_split,id]
test_data$id <- model2_data[-train_split,id]

source("./load_feature_selected_model2.R")
source("./genetic_data_analysis_model2.R")
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

# Get training data ready
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_1yr # labels
dtrain <- xgb.DMatrix(data = dm_train, label = train_y) #  Change to XGBoost format

# Get test data ready
dm_test <- as.matrix(test_all[,lasso_all_vars,with=F]) # inputs
test_y <- test_labels$health_1yr # labels
dtest <- xgb.DMatrix(data = dm_test, label = test_y) # Change to XGBoost format

set.seed(250388) # set every time run model

# Watchlist to check for overfitting
watchlist <- list(train=dtrain, test=dtest) # Print train and test error through model iterations
# XGBoost has regularisation built in to prevent overfitting

# List paramters
# eta 0.05 rounds 5000 - low learning but longer period to learn
params <- list(
  booster = "gbtree", # Which booster to use - we use the tree one
  objective = "binary:logistic", # Outcome is binary - give predicted probability
  eta = 0.05, # Learning rate - prevent overfitting by adding penalty for additional trees added to a model (i.e. shrinks weights with each additional trees)
  gamma = 0, # Minimum loss reduction required to make a further partition on a leaf node of the tree
  max_depth = 6, # How deep a tree can be (i.e. 6 would be the extent of interactions); higher values may lead to overfitting due to greater likelihood of finding specific relationships linked to the training data
  min_child_weight = 1, # Minimum sum of weight - for additonal leaf node, needs to add so much; controls overfitting - higher values will prevent a model becoming too specific to the training data (too low and it will underfit)
  max_delta_step = 0, # Contraint of tree's weight - 0 is none, but positive valuses useful for imbalanced data (i.e. more 0s than 1s)
  subsample = 1, # If 0.5, then collect half the data to sample from to be computationally efficient
  colsample_bytree = 1, # As above but for columns (variables)
  alpha = 0.001 # L1 regularisation as used in the neural nets
)


# Train final model
xgb2 <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100, 
  maximize = F , 
  eval_metric = "map" # Binary classification error rate. It is calculated as #(wrong cases)/#(all cases). Could also use "auc" (area under curve) or "map" (mean average precision)
)

# Save training data
dm_train_xgb_m2 <- dm_train

# Model 3 #

# Load data
source("./load_var_names.R")
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model3_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_5yr)] 
set.seed(250388)
train_split <- readRDS(file = "./Data split files/train_split_health5yr_model3.rds") # Load
train <- model3_data[train_split,] # Training data
test <- model3_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_5yr"] # Training data labels

test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_5yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model3_data[train_split,id]
test_data$id <- model3_data[-train_split,id]

source("./load_feature_selected_model3.R")
source("./genetic_data_analysis_model3.R")
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

# Get training data ready
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_5yr # labels
dtrain <- xgb.DMatrix(data = dm_train, label = train_y) #  Change to XGBoost format

# Get test data ready
dm_test <- as.matrix(test_all[,lasso_all_vars,with=F]) # inputs
test_y <- test_labels$health_5yr # labels
dtest <- xgb.DMatrix(data = dm_test, label = test_y) # Change to XGBoost format

set.seed(250388) # set every time run model

# Watchlist to check for overfitting
watchlist <- list(train=dtrain, test=dtest) # Print train and test error through model iterations
# XGBoost has regularisation built in to prevent overfitting

# List paramters
# eta 0.05 rounds 5000 - low learning but longer period to learn
params <- list(
  booster = "gbtree", # Which booster to use - we use the tree one
  objective = "binary:logistic", # Outcome is binary - give predicted probability
  eta = 0.05, # Learning rate - prevent overfitting by adding penalty for additional trees added to a model (i.e. shrinks weights with each additional trees)
  gamma = 0, # Minimum loss reduction required to make a further partition on a leaf node of the tree
  max_depth = 6, # How deep a tree can be (i.e. 6 would be the extent of interactions); higher values may lead to overfitting due to greater likelihood of finding specific relationships linked to the training data
  min_child_weight = 1, # Minimum sum of weight - for additonal leaf node, needs to add so much; controls overfitting - higher values will prevent a model becoming too specific to the training data (too low and it will underfit)
  max_delta_step = 0, # Contraint of tree's weight - 0 is none, but positive valuses useful for imbalanced data (i.e. more 0s than 1s)
  subsample = 1, # If 0.5, then collect half the data to sample from to be computationally efficient
  colsample_bytree = 1, # As above but for columns (variables)
  alpha = 0.001 # L1 regularisation as used in the neural nets
)


# Train final model
xgb3 <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100, 
  maximize = F , 
  eval_metric = "map" # Binary classification error rate. It is calculated as #(wrong cases)/#(all cases). Could also use "auc" (area under curve) or "map" (mean average precision)
)

# Save training data
dm_train_xgb_m3 <- dm_train

# Model 4 #

# Load data
source("./load_var_names.R")
ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
model4_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_5yr) & ukhls$health_0yr == 0] 
set.seed(250388)
train_split <- readRDS(file = "./Data split files/train_split_health5yr_model4.rds") # Load
train <- model4_data[train_split,] # Training data
test <- model4_data[-train_split,] # Test data

# Split data into inputs (explanatory variables) and labels (outputs/outcomes)
train_inputs <- train[, all_vars, with = FALSE] # Training data inputs
train_labels <- train[,"health_5yr"] # Training data labels

test_inputs <- test[, all_vars, with = FALSE] # Test data inputs
test_labels <- test[,"health_5yr"] # Test data labels
rm(train, test)

# Normalise the data
mean <- apply(train_inputs, 2, mean) # Calcuate the mean of each feature
std <- apply(train_inputs, 2, sd) # Calculate the standard deviation of each feature
train_data <- as.data.table(scale(train_inputs, center = mean, scale = std)) # Scale training data
test_data <- as.data.table(scale(test_inputs, center = mean, scale = std)) # Scale test data
rm(train_inputs, test_inputs)

# Join back on IDs
train_data$id <- model4_data[train_split,id]
test_data$id <- model4_data[-train_split,id]

source("./load_feature_selected_model4.R")
source("./genetic_data_analysis_model4.R")
lasso_gen_vars <- names(train_chr[,-1])
lasso_all_vars <- c(lasso_personal_vars, lasso_ses_vars, lasso_health_vars, lasso_biom_vars, lasso_gen_vars)
train_all <- cbind(train_data, train_chr[,-1,with=F]) # Create single object containing all data
test_all <- cbind(test_data, test_chr[,-1,with=F])

# Get training data ready
dm_train <- as.matrix(train_all[,lasso_all_vars,with=F]) # inputs
train_y <- train_labels$health_5yr # labels
dtrain <- xgb.DMatrix(data = dm_train, label = train_y) #  Change to XGBoost format

# Get test data ready
dm_test <- as.matrix(test_all[,lasso_all_vars,with=F]) # inputs
test_y <- test_labels$health_5yr # labels
dtest <- xgb.DMatrix(data = dm_test, label = test_y) # Change to XGBoost format

set.seed(250388) # set every time run model

# Watchlist to check for overfitting
watchlist <- list(train=dtrain, test=dtest) # Print train and test error through model iterations
# XGBoost has regularisation built in to prevent overfitting

# List paramters
# eta 0.05 rounds 5000 - low learning but longer period to learn
params <- list(
  booster = "gbtree", # Which booster to use - we use the tree one
  objective = "binary:logistic", # Outcome is binary - give predicted probability
  eta = 0.05, # Learning rate - prevent overfitting by adding penalty for additional trees added to a model (i.e. shrinks weights with each additional trees)
  gamma = 0, # Minimum loss reduction required to make a further partition on a leaf node of the tree
  max_depth = 6, # How deep a tree can be (i.e. 6 would be the extent of interactions); higher values may lead to overfitting due to greater likelihood of finding specific relationships linked to the training data
  min_child_weight = 1, # Minimum sum of weight - for additonal leaf node, needs to add so much; controls overfitting - higher values will prevent a model becoming too specific to the training data (too low and it will underfit)
  max_delta_step = 0, # Contraint of tree's weight - 0 is none, but positive valuses useful for imbalanced data (i.e. more 0s than 1s)
  subsample = 1, # If 0.5, then collect half the data to sample from to be computationally efficient
  colsample_bytree = 1, # As above but for columns (variables)
  alpha = 0.001 # L1 regularisation as used in the neural nets
)


# Train final model
xgb4 <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100, 
  maximize = F , 
  eval_metric = "map" # Binary classification error rate. It is calculated as #(wrong cases)/#(all cases). Could also use "auc" (area under curve) or "map" (mean average precision)
)

# Save training data
dm_train_xgb_m4 <- dm_train


# PDP plots #

# Model 1
p1 <- xgb1 %>%  # the %>% operator is read as "and then"
        partial(pred.var = "age", train = dm_train_xgb_m1, center = T, ICE = T) %>%
        autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
                    main = "Age", center = T, rug = F) + ylim(-0.6,0.6) # Alternatively coord_cartesian(ylim=c(-0.4,0.4), xlim=c(-2,2))
p2 <- xgb1 %>%  
        partial(pred.var = "benefit3", train = dm_train_xgb_m1, center = T, ICE = T) %>%
        autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
                    main = "Receives incapacity benefits", center = T, rug = F) + ylim(-0.6,0.6)
p3 <- xgb1 %>%  
        partial(pred.var = "b_walkpace", train = dm_train_xgb_m1, center = T, ICE = T) %>%
        autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
                    main = "Walk pace", center = T, rug = F) + ylim(-0.6,0.6)
p4 <- xgb1 %>% 
        partial(pred.var = "hba1c", train = dm_train_xgb_m1, center = T, ICE = T) %>%
        autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
                    main = "Glycated haemoglobin", center = T, rug = F) + ylim(-0.6,0.6)

figd1 <- plot_grid(p1, p2, p3, p4, ncol = 2, align = "h") # Join together into single plot
figd1 # Print

ggsave(file="./Plots/figure_d1.jpeg", figd1) # Save

# Model 2
p1 <- xgb3 %>%  
  partial(pred.var = "age", train = dm_train_xgb_m3, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Age", center = T, rug = F) + ylim(-0.4,0.2) 
p2 <- xgb3 %>%  
  partial(pred.var = "hcond2", train = dm_train_xgb_m3, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Arthritis", center = T, rug = F) + ylim(-0.4,0.2) 
p3 <- xgb3 %>%  
  partial(pred.var = "b_walkpace", train = dm_train_xgb_m3, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Walk pace", center = T, rug = F) + ylim(-0.4,0.2) 
p4 <- xgb3 %>% 
  partial(pred.var = "b_depenth4", train = dm_train_xgb_m3, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Depressed about job", center = T, rug = F) + ylim(-0.4,0.2) 

figd2 <- plot_grid(p2, p1, p3, p4, ncol = 2, align = "h")
figd2

ggsave(file="./Plots/figure_d2.jpeg", figd2)

# Model 3
p1 <- xgb2 %>%  
  partial(pred.var = "age", train = dm_train_xgb_m2, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Age", center = T, rug = F) + ylim(-1.2,-0.6) 
p2 <- xgb2 %>%  
  partial(pred.var = "dheas", train = dm_train_xgb_m2, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Dihydroepiandrosterone sulphate", center = T, rug = F) + ylim(-1.2,-0.6)  
p3 <- xgb2 %>%  
  partial(pred.var = "hba1c", train = dm_train_xgb_m2, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Glycated haemoglobin", center = T, rug = F) + ylim(-1.2,-0.6) 
p4 <- xgb2 %>% 
  partial(pred.var = "trig", train = dm_train_xgb_m2, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Triglycerides", center = T, rug = F) + ylim(-1.2,-0.6) 

figd3 <- plot_grid(p2, p1, p3, p4, ncol = 2, align = "h")
figd3

ggsave(file="./Plots/figure_d3.jpeg", figd3)

# Model 4
p1 <- xgb4 %>%  
  partial(pred.var = "age", train = dm_train_xgb_m4, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Age", center = T, rug = F) + ylim(-1,-0.4) 
p2 <- xgb4 %>%  
  partial(pred.var = "wstval", train = dm_train_xgb_m4, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Waist circumference", center = T, rug = F) + ylim(-1,-0.4) 
p3 <- xgb4 %>%  
  partial(pred.var = "ecre", train = dm_train_xgb_m4, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Creatinine", center = T, rug = F) + ylim(-1,-0.4) 
p4 <- xgb4 %>% 
  partial(pred.var = "lung_cap_ratio", train = dm_train_xgb_m4, center = T, ICE = F) %>%
  autoplot(smooth = F, lwd = 1.5, ylab = "Partial prediction", xlab = "Standardised value",
           main = "Lung capacity ratio", center = T, rug = F) + ylim(-1,-0.4) 

figd4 <- plot_grid(p2, p1, p3, p4, ncol = 2, align = "h")
figd4

ggsave(file="./Plots/figure_d4.jpeg", figd4)
