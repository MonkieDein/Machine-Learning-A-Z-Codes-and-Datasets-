# Support Vector Machine / Regression (SVM/SVR)
# epsilon-Insensitive Tube: If error within epsilon of the fit, ignore them
# Note that, many lines would be good fit for a big epsilon.
# Therefore, a penalty that is perpendicular to the size of epsilon is necessary.
# Goal: Minimize the regularized function
#
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/R")

library(e1071)
# Importing the dataset where string variable is treated as factor
df = read.csv('Position_Salaries.csv',stringsAsFactors = TRUE)

# Splitting the dataset into (Training set) and (Test set)
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased,SplitRatio = 0.8)
# training_set = subset(dataset,split == TRUE)
# test_set = subset(dataset,split == FALSE)
# 
# Feature Scaling (Only use for gradient based method)
# training_set[c("Age","Salary")] = scale(training_set[c("Age","Salary")])
# test_set[c("Age","Salary")] = scale(test_set[c("Age","Salary")])

# Create Regression
reg = svm(formula = Salary ~ Level,data = df,type = "eps-regression")

# Create smoother levels and predict results
Lvls = data.frame(Level = (1:1000)*0.01)
Lvls$reg = predict(reg,newdata = Lvls)

# Visualize regression result
# Plot
library(ggplot2)
colors <- c("data" = "black", "reg" = "red")
ggplot() + geom_point(aes(x = df$Level ,y = df$Salary,color = "black")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$reg,color = "reg")) +
  ggtitle("Regression") + xlab("Level") + ylab("Salary") +
  scale_color_manual(values=colors)
