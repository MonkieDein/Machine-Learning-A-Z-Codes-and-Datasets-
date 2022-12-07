# All regressions methods compare
#
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression")# Importing the dataset where string variable is treated as factor
df = read.csv('Data.csv',stringsAsFactors = TRUE)

# Import library
library(caTools) # For splitting test and train
library(randomForest) # For random forest regression
library(e1071) # For SVM regression

# define dependent variable (y) and independent variable (IV)
y = 'PE'
IV = colnames(df)[colnames(df)!=y]
# Set seed 
set.seed(1234)

# Splitting the dataset into (Training set) and (Test set)
split = sample.split(df[[y]],SplitRatio = 0.8)
train_df = subset(df,split == TRUE)
test_df = subset(df,split == FALSE)

# Create Regression
Rforest_reg = randomForest(x = train_df[IV],y = train_df[[y]],ntree = 100)
svr_reg = svm(formula = eval(parse( text= paste(y,"~",paste(IV,collapse=" + ")) )),
              data = train_df,type = "eps-regression")
mlr_reg = lm(formula = eval(parse( text= paste(y,"~polym(",paste(IV,collapse=","),",degree=2,raw=TRUE)") )),
             data = train_df)

# model Selection with summary
rSqr = function(reg,data,y,k){
  TSS = sum((data[[y]] - mean(data[[y]]))^2)
  yHat = predict(reg,newdata = data)
  RSS = sum((data[[y]]-yHat)^2)
  r2 = (1 - RSS/TSS)
  n = nrow(data)
  return( list(r2=r2,r2A = 1-(1-r2)*(n-1)/(n-k-1) ))
}

rSqr(Rforest_reg,train_df,y,length(IV))
rSqr(svr_reg,train_df,y,length(IV))
rSqr(mlr_reg,train_df,y,length(IV)^2)


