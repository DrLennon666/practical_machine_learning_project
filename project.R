library(caret)
library(ggplot2)
library(randomForest)
library(e1071)
set.seed(666)

train <- read.csv("C:/Users/Craig/Documents/R Class/machine_learning/pml_training.csv")

test_set <- read.csv("C:/Users/Craig/Documents/R Class/machine_learning/pml_testing.csv")

head(train)
names <- colnames(train)

# remove rows that don't pertain to a predictor or a response
small_names <- names[8:160]
small_train <- train[, small_names]
str(small_train) # There are quite a few columns with NAs

# need to remove NAs and #Div/0!
not_NA <- c()
for(i in 1:length(small_names)){

  if(sum(is.na(small_train[,i]))==0){
    
    not_NA <- c(not_NA,i)
  }
}

clean_small_train <- small_train[,not_NA]
str(clean_small_train) 

# some predictors are listed as factors and not numeric. "#DIV/0!"
# Remove predictors with DIV/0
has_div_0 <-c()
for(i in 1:length(colnames(clean_small_train))){
  
  if(is.element('#DIV/0!', clean_small_train[,i])==TRUE){
    has_div_0 <- c(has_div_0,i)
  }
}

clean_small_train <- clean_small_train[,-has_div_0]
str(clean_small_train)

# Check for zero variance predictors
zero_var_pred <- nearZeroVar(clean_small_train, saveMetrics = TRUE)
summary(zero_var_pred[3]) #no near zero variance predictors
summary(zero_var_pred[4]) #no zero variance predictors

# Prepare test data
small_test <- test_set[, 8:159]
str(small_test) # There are quite a few columns with NAs


# need to remove NAs
not_NA <- c()
for(i in 1:152){
  
  if(sum(is.na(small_test[,i]))==0){
    
    not_NA <- c(not_NA,i)
  }
}

clean_small_test <- small_test[,not_NA]
str(clean_small_test) 


# Check for zero variance predictors
zero_var_pred_test <- nearZeroVar(clean_small_test, saveMetrics = TRUE)
summary(zero_var_pred_test[3]) #no near zero variance predictors
summary(zero_var_pred_test[4]) #no zero variance predictors END TEST



# Train test split, not really needed for random forest classifiers
# Out of bag testing takes care of this.
#inX_train <- createDataPartition(y = clean_small_train$classe, p = 0.8, list = FALSE)

#X_train <- clean_small_train[inX_train,]
#X_test <- clean_small_train[-inX_train,]
#dim(X_train)
#dim(X_test)

rf <-randomForest(classe ~ ., data = clean_small_train, importance = TRUE, ntrees = 10, do.trace = TRUE)

rf_test <- predict(rf, clean_small_test) #X-test
answers <- as.vector(rf_test)

pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)

print(confusionMatrix(rf_test, X_test$classe))

