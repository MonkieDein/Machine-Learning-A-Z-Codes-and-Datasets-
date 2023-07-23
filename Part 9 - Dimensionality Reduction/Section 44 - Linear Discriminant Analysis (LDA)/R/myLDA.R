# Feature Extraction (Dimensionality Reduction)
# Linear Discriminant Analysis
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 9 - Dimensionality Reduction/Section 44 - Linear Discriminant Analysis (LDA)/R")
library(caTools) # For splitting test and train
df = read.csv('Wine.csv',stringsAsFactors = TRUE)
# define dependent variable (y) and independent variable (IV)
y = 'Customer_Segment'
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
# # Feature Scaling (beneficial for visualization and normalization)
train_df[IV] = myscale(train_df[IV],train_m,train_sd)
test_df[IV] =  myscale(test_df[IV],train_m,train_sd)

# Applying LDA
library(MASS)
lda = lda(formula = Customer_Segment ~ .,data = train_df)
train_lda = data.frame(predict(lda,train_df))
names(train_lda)[1] = y
IV_lda = c("x.LD1","x.LD2")
test_lda = data.frame(predict(lda,test_df))
names(test_lda)[1] = y
# Building Classifier
classifier = svm(formula = Customer_Segment ~ x.LD1 + x.LD2,data = train_lda,
                 type='C-classification',kernel='linear')

# Making prediction on the test set
y_pred = predict(classifier,newdata = test_lda[IV_lda]) 

# Confusion Matrix
cm = table(test_lda[[y]],y_pred)

# Visualizaiton
bgcol = c("deepskyblue","springgreen3","tomato")
ptcol = c("blue2","green4","red3")
library(ElemStatLearn)

set = train_lda
X1 = seq(min(set['x.LD1']),max(set['x.LD1']),length.out = 150)
X2 = seq(min(set['x.LD2']),max(set['x.LD2']),length.out = 150)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = IV_lda
y_grid = predict(classifier,newdata = grid_set)
plot(grid_set,pch= '.',cex=3,col = sapply(y_grid,function(z) bgcol[z]),
     main = 'SVM Classifier (Train)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(set[IV_lda],pch= 21,bg = sapply(set[[y]],function(z) ptcol[z]))

plot(grid_set,pch= '.',cex=3,col = sapply(y_grid,function(z) bgcol[z]),
     main = 'SVM Classifier (Test)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(test_lda[IV_lda],pch= 21,bg = sapply(test_lda[[y]],function(z) ptcol[z]))





