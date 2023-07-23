# Kernel PCA
# Feature Extraction (Dimensionality Reduction)
# Principal Component Analysis
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 9 - Dimensionality Reduction/Section 45 - Kernel PCA/R")
library(caTools) # For splitting test and train
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
# # Feature Scaling (beneficial for visualization and normalization)
train_df[IV] = myscale(train_df[IV],train_m,train_sd)
test_df[IV] =  myscale(test_df[IV],train_m,train_sd)

# Applying Kernal PCA
library(kernlab)
kpca = kpca(~., data=train_df[IV],kernel = 'rbfdot',features = 2)
train_pca = cbind(data.frame(predict(kpca,train_df)),train_df[y])
IV_pca = c("X1","X2")
test_pca = cbind(data.frame(predict(kpca,test_df)),test_df[y])

# Building Classifier
classifier = glm(formula = Purchased ~ . ,family = binomial, data = train_pca)

# Making prediction on the test set
y_pred = 1*(predict(classifier,newdata = test_pca[IV_pca]) > 0.5)

# Confusion Matrix
cm = table(test_pca[[y]],y_pred)

# Visualizaiton
library(ElemStatLearn)

set = train_pca
X1 = seq(min(set['X1']),max(set['X1']),length.out = 150)
X2 = seq(min(set['X2']),max(set['X2']),length.out = 150)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = IV_pca
y_grid = 1*(predict(classifier,newdata = grid_set) > 0.5) 
plot(grid_set,pch= '.',cex=3,col = ifelse(y_grid==1,'springgreen3','tomato' ),
     main = 'Decission Tree Classifier (Train)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(set[IV_pca],pch= 21,bg = ifelse(set[[y]]==1,'green4','red3' ))

plot(grid_set,pch= '.',cex=3,col = ifelse(y_grid==1,'springgreen3','tomato' ),
     main = 'Decission Tree Classifier (Test)',xlab = 'Age',
     ylab = 'Estimated Salary',xlim = range(X1),ylim=range(X2))
points(test_pca[IV_pca],pch= 21,bg = ifelse(test_df[[y]]==1,'green4','red3' ))
