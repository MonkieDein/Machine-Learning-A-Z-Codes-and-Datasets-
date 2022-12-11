# XGBoost
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 10 - Model Selection and Boosting/Section 49 - XGBoost/R")
# Import the dataset
df = read.csv('Churn_Modelling.csv',stringsAsFactors = TRUE)
df = as.data.frame(lapply(df, as.numeric))

V = names(df)[!names(df) %in% c("RowNumber", "CustomerId" ,"Surname")]
df = df[V]
y = "Exited"
IV = V[V!=y]
# Splitting the dataset into (Training set) and (Test set)
set.seed(123)
split = sample.split(df[[y]],SplitRatio = 0.8)
train_df = subset(df,split == TRUE)
test_df = subset(df,split == FALSE)

# Fitting XGboost to the Training Set
library(xgboost)
classifier = xgboost(data = as.matrix(train_df[IV]), label = train_df[[y]],nround=10)

# Evaluating ML method in for our training data frame
# Applying k-fold Cross Validation
folds = createFolds(train_df[[y]],k=10)
cv = sapply(folds,function(f) {
  train_fold = train_df[-f,]
  test_fold = train_df[f,]
  classifier = xgboost(data = as.matrix(train_fold[IV]), label = train_fold[[y]],nround=10)
  y_pred = predict(classifier,newdata = as.matrix(test_fold[IV])) >= 0.5
  cm = table(test_fold[[y]],y_pred)
  accuracy = sum(diag(cm))/sum(cm)
  return(accuracy)
})
accuracy = mean(cv)
