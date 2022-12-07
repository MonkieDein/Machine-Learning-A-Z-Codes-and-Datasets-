# Logistic Regression
#
library(caTools) # For splitting test and train
library(latticeExtra) 
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 3 - Classification/Section 14 - Logistic Regression/R")
# Importing the dataset where string variable is treated as factor
df = read.csv('Social_Network_Ads.csv',stringsAsFactors = TRUE)
df = df[3:5] # remove user ID and gender
# define dependent variable (y) and independent variable (IV)
y = 'Purchased'
IV = colnames(df)[colnames(df)!=y]
# Set seed 
set.seed(123)

# Splitting the dataset into (Training set) and (Test set)
split = sample.split(df[[y]],SplitRatio = 0.75)
train_df = subset(df,split == TRUE)
test_df = subset(df,split == FALSE)

train_m = colMeans(train_df[IV])
train_sd = sapply(train_df[IV],sd)
unscale = function(df,m,sd){
  n = ncol(df)
  unscale_df = sapply(1:n,function(c) df[c] * sd[c] + m[c])
  return(data.frame(unscale_df))
}
myscale = function(df,m,sd){
  n = ncol(df)
  scale_df = sapply(1:n,function(c) (df[c] - m[c])/sd[c])
  return(data.frame(scale_df))
}
# Feature Scaling (beneficial for visualization and normalization)
train_df[IV] = myscale(train_df[IV],train_m,train_sd)
test_df[IV] =  myscale(test_df[IV],train_m,train_sd)

# Building Classifier
classifier = glm(formula = Purchased ~ .,family = binomial,data = train_df)

# Making prediction on the test set
prob_pred = predict(classifier,type='response',newdata = test_df[IV])
y_pred = 1*(prob_pred > 0.5)

# Confusion Matrix
cm = table(test_df[[y]],y_pred)

# Visualizaiton
library(ElemStatLearn)
set = train_df
X1 = seq(min(set[,1])-0.1,max(set[,1]) + 0.1,by=0.02)
X2 = seq(min(set[,2])-0.1,max(set[,2]) + 0.1,by=0.02)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = IV
prob_set = predict(classifier,type='response',newdata = grid_set)
y_grid = 1*(prob_set > 0.5)
plot(grid_set,pch= '.',cex=3,col = ifelse(y_grid==1,'springgreen3','tomato' ),
     main = 'Logistic Regression (Train)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(set,pch= 21,bg = ifelse(set[[y]]==1,'green4','red3' ))

set = test_df
X1 = seq(min(set[,1])-0.1,max(set[,1]) + 0.1,by=0.02)
X2 = seq(min(set[,2])-0.1,max(set[,2]) + 0.1,by=0.02)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = IV
prob_set = predict(classifier,type='response',newdata = grid_set)
y_grid = 1*(prob_set > 0.5)
plot(grid_set,pch= '.',cex=3,col = ifelse(y_grid==1,'springgreen3','tomato' ),
     main = 'Logistic Regression (Test)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(set,pch= 21,bg = ifelse(set[[y]]==1,'green4','red3' ))


