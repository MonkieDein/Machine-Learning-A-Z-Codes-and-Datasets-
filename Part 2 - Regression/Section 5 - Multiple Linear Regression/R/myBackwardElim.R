library(gsubfn)
# Backward Elimination (remove highest P-value < 0.05)
backwardElimination = function(df,p,y,IV){
  # Fitting Simple Linear Regression to the Training Set
  regressor = lm(formula = eval(parse( text= paste(y,"~",paste(IV,collapse=" + ")) )), data = df)
  # Summarize the regression output
  Pvalue = summary(regressor)$coefficients[-1,4]
  while (max(Pvalue) > p){
    IV = IV[IV!=names(Pvalue)[which.max(Pvalue)]]
    regressor = lm(formula = eval(parse( text= paste(y,"~",paste(IV,collapse=" + ")) )), data = df)
    Pvalue = summary(regressor)$coefficients[-1,4]
  }
  return(list(IV=IV,regressor=regressor))
}
#
# Multiple Linear regression, given X predict Y as Yhat.
# For non linear prediction one could use feature transformation on X or Y
# Goal : Minimize sum of square residual = sum( (Y_i - Yhat_i)^2 )
#
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 5 - Multiple Linear Regression/R")# Importing the dataset where string variable is treated as factor
df_pre = read.csv('50_Startups.csv',stringsAsFactors = TRUE)
# Preprocess categorical variable for backward Elimination
df = df_pre[which(colnames(df_pre)!="State")]
lvl = levels(df_pre$State)[-3]
for (l in lvl){
  df[[paste0("State",l)]] = (df_pre$State == l)*1
}

# Splitting the dataset into (Training set) and (Test set)
library(caTools)
set.seed(123)
split = sample.split(df$Profit,SplitRatio = 0.8)
train_df = subset(df,split == TRUE)
test_df = subset(df,split == FALSE)

y = 'Profit'
IV = colnames(df)[colnames(df)!=y]

list[IV,regressor] = backwardElimination(train_df,0.05,y,IV)
summary(regressor)

