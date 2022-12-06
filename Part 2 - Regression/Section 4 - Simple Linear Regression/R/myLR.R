#
# Ordinary Least Square
# Simple Linear regression, given X predict Y as Yhat.
# Goal : Minimize sum of square residual = sum( (Y_i - Yhat_i)^2 )
#
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 4 - Simple Linear Regression/R")

# Importing the dataset where string variable is treated as factor
df = read.csv('Salary_Data.csv',stringsAsFactors = TRUE)

# Splitting the dataset into (Training set) and (Test set)
library(caTools)
set.seed(123)
split = sample.split(df$Salary,SplitRatio = 2/3)
train_df = subset(df,split == TRUE)
test_df = subset(df,split == FALSE)

# Fitting Simple Linear Regression to the Training Set
regressor = lm(formula = Salary ~ YearsExperience, 
               data = train_df)
# Summarize the regression output
summary(regressor)

# Predicting the test results
y_pred = predict(regressor,newdata = test_df)

# Visualizing the Training Set Result
library(ggplot2)
ggplot() + geom_point(aes(x = train_df$YearsExperience,y = train_df$Salary),color = 'red') +
  geom_line(aes(x=train_df$YearsExperience,y = predict(regressor,newdata = train_df)),color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') + xlab('Years of Experience') + ylab('Salary')
# Visualizing the Test Set Result
ggplot() + geom_point(aes(x = test_df$YearsExperience,y = test_df$Salary),color = 'red') +
  geom_line(aes(x=test_df$YearsExperience,y = predict(regressor,newdata = test_df)),color = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') + xlab('Years of Experience') + ylab('Salary')






