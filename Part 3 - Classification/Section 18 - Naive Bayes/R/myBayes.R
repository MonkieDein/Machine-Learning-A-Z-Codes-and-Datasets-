# Naive Bayes
#
# Use Bayes theorem to calculate posterior and use for classification
#
library(caTools) # For splitting test and train
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 3 - Classification/Section 16 - Support Vector Machine (SVM)/R")
# Importing the dataset where string variable is treated as factor
df = read.csv('Social_Network_Ads.csv',stringsAsFactors = TRUE)
df = df[!(names(df) %in% c("User.ID","Gender"))] # remove user ID and gender
# define dependent variable (y) and independent variable (IV)
y = 'Purchased'
df[[y]] = factor(df[[y]]) # set DP as factor variable
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
library(e1071)
classifier = naiveBayes(y = train_df[[y]],x = train_df[IV])

# Making prediction on the test set
y_pred = predict(classifier,newdata = test_df[IV]) 

# Confusion Matrix
cm = table(test_df[[y]],y_pred)

# Visualizaiton
library(ElemStatLearn)
set = train_df
X1 = seq(min(set[,1])-0.1,max(set[,1]) + 0.1,by=0.05)
X2 = seq(min(set[,2])-0.1,max(set[,2]) + 0.1,by=0.05)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = IV
y_grid = predict(classifier,newdata = grid_set)
plot(grid_set,pch= '.',cex=3,col = ifelse(y_grid==1,'springgreen3','tomato' ),
     main = 'Classfier (Train)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(set,pch= 21,bg = ifelse(set[[y]]==1,'green4','red3' ))

plot(grid_set,pch= '.',cex=3,col = ifelse(y_grid==1,'springgreen3','tomato' ),
     main = 'Classfier (Test)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(test_df,pch= 21,bg = ifelse(test_df[[y]]==1,'green4','red3' ))





