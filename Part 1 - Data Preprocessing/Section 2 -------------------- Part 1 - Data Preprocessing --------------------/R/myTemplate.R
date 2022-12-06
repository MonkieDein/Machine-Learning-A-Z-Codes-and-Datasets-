# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/R")

# Data Preprocessing

# Importing the dataset where string variable is treated as factor
dataset = read.csv('Data.csv',stringsAsFactors = TRUE)

# Dealing with missing data
# 1) Replace missing data with the mean
var_of_interest = c("Age","Salary")
for(v in var_of_interest){
  dataset[[v]] = ifelse(is.na(dataset[[v]]),mean(dataset[[v]],na.rm=TRUE),dataset[[v]])
}

# Splitting the dataset into (Training set) and (Test set)
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.8)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)

# Feature Scaling (Only use for gradient based method)
# training_set[c("Age","Salary")] = scale(training_set[c("Age","Salary")])
# test_set[c("Age","Salary")] = scale(test_set[c("Age","Salary")])


