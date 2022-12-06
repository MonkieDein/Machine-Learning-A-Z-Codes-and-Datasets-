# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/R")

# Data Preprocessing

# Importing the dataset
dataset = read.csv('Data.csv')

# Dealing with missing data
# 1) Replace missing data with the mean
var_of_interest = c("Age","Salary")
for(v in var_of_interest){
  dataset[[v]] = ifelse(is.na(dataset[[v]]),mean(dataset[[v]],na.rm=TRUE),dataset[[v]])
}

# Encoding categorical data
dataset$Country = factor(dataset$Country,
                         levels = c('France','Spain','Germany'),
                         labels = c(1,2,3))
dataset$Purchased = factor(dataset$Purchased,
                         levels = c('No','Yes'),
                         labels = c(0,1))

# 2) remove NA data and initialize string data as factor
dataset2 = read.csv('Data.csv',stringsAsFactors = TRUE)
dataset2 = dataset2[!rowSums(is.na.data.frame(dataset2)),]

# Splitting the dataset into (Training set) and (Test set)
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.8)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split == FALSE)

# Feature Scaling (Only use for gradient based method)
# 1) Standardisation : (x-mean(x))/std(x)  
# 2) Normalization : x-min(x)/(max(x)-min(x))
# Most ML method are based on L2 distance : 
# Variable with larger range dominate effect the function.
training_set[c("Age","Salary")] = scale(training_set[c("Age","Salary")])
test_set[c("Age","Salary")] = scale(test_set[c("Age","Salary")])
# The scaling is really dependent on the domain and algorithm used.


