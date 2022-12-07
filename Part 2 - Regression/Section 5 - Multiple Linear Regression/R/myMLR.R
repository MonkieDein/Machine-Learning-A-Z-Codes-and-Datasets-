# Assumption or Checks for LR output
# 1) Linearity
# 2) Homoscedasticity
# 3) Multivariate Normality
# 4) Independence
# 5) Lack of Multicollinearity
# 6) Outlier Check
#
# Feature Selection
# 1) ALL-IN (Overfitting)
# 2) Backward Elimination (remove highest P-value < 0.05)
# 3) Multivariate Normality (Adding the smallest P-value variable < 0.05)
# 4) Bidirectional (Alternate 2 and 3)
# 5) Score Comparison (eg: adjusted R-square)
#
# Multiple Linear regression, given X predict Y as Yhat.
# For non linear prediction one could use feature transformation on X or Y
# Goal : Minimize sum of square residual = sum( (Y_i - Yhat_i)^2 )
#
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 5 - Multiple Linear Regression/R")# Importing the dataset where string variable is treated as factor
df = read.csv('50_Startups.csv',stringsAsFactors = TRUE)

# Splitting the dataset into (Training set) and (Test set)
library(caTools)
set.seed(123)
split = sample.split(df$Profit,SplitRatio = 0.8)
train_df = subset(df,split == TRUE)
test_df = subset(df,split == FALSE)

# Fitting Simple Linear Regression to the Training Set
regressor = lm(formula = Profit ~ ., data = train_df)
# Summarize the regression output
summary(regressor)

# Filter P-value > 0.05 of the independent variable
# re-fit with only the relevant variable
regressor = lm(formula = Profit ~ R.D.Spend, data = train_df)
summary(regressor)

# Predicting the test results
y_pred = predict(regressor,newdata = test_df)

# Visualizing the Training Set Result
library(ggplot2)
ggplot() + geom_point(aes(x = train_df$R.D.Spend,y = train_df$Profit),color = 'red') +
  geom_line(aes(x=train_df$R.D.Spend,y = predict(regressor,newdata = train_df)),color = 'blue') +
  ggtitle('Profit vs Experience (Training Set)') + xlab('R.D.Spend') + ylab('Profit')
# Visualizing the Test Set Result
ggplot() + geom_point(aes(x = test_df$R.D.Spend,y = test_df$Profit),color = 'red') +
  geom_line(aes(x=test_df$R.D.Spend,y = predict(regressor,newdata = test_df)),color = 'blue') +
  ggtitle('Profit vs Experience (Test Set)') + xlab('R.D.Spend') + ylab('Profit')






