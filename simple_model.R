#################################
######## simple DL model ########
#################################

# Convert data to expected TensorFlow format
dm_train <- as.matrix(train_data[,lasso_all_vars,with=F]) # inputs
y <- train_labels$health_1yr # labels

# Define network structure
require(keras)
model <- keras_model_sequential() %>% # State is sequential model (i.e. no reinforced learning)
  layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% # First layer. We include l1 regularisation to minimise potential for overfitting, although its affect is small. L1 regularisation was selected merely because it is the same as the LASSO regression.
  layer_dense(units = 64, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% 
  layer_dense(units = 32, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>% 
  layer_dense(units = 1, activation = "sigmoid") # Output layer - generate a predicted prob
  
model %>% compile(
  optimizer = "rmsprop", # Optimiser - how the network will update itself during the training phase - in our case we will use Gradient Descent since has been shown to be most effective and efficient (this is one algorithm for it)
  loss = "binary_crossentropy", # Best choice for dealing with probabilities and binary labels. Crossentropy loss function measures the difference between the probability and label values.
  metrics = c("accuracy") # Evaluate model based on accuracy (% of correctly classified labels)
)

model %>% fit(dm_train, y, epochs = 10, batch_size = 128)

dm_test <- as.matrix(test_data[,lasso_all_vars,with=F])
test_y <- test_labels$health_1yr 
metrics <- model %>% evaluate(dm_test, test_y, batch_size = 128, verbose = 0)
metrics

# Predict outcomes
pred_y <- model %>% predict(dm_test, batch_size=128, verbose=0)
pred.nn <- rep(0,length(pred_y))
pred.nn[pred_y>=0.5] <- 1
require(caret)
cm2 <- confusionMatrix(as.factor(test_y), as.factor(pred.nn)) # Calculate model fit measures

# Store results in clean format
nnfit_test <- as.data.frame(as.matrix(cm2, what = "overall")) # Accuracy
nnfit_test <- rbind(nnfit_test, as.data.frame(as.matrix(cm2, what = "classes"))) # Other metrics
colnames(nnfit_test) <- c("Values") # Rename column 

# Calcuate AUC
library(ROCR)
pr <- prediction(pred_y, test_y) # calcuate predictions again!
auc <- performance(pr, measure = "auc") # Calculate AUC
auc <- auc@y.values[[1]] # Extract only AUC value 
nnfit_test[19,1] <- auc # Join onto table of results
rownames(nnfit_test)[19] <- "AUC" # Edit row name
nnfit_test <- round(nnfit_test, digits=4) # Round numbers off for presentation
nnfit_test # Print

