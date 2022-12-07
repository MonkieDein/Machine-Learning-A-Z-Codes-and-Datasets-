# (Decision) Regression Tree
# Maximizing Information Entropy
# Stop : (1) If information added is small 
#        (2) If the split would cause a segment to have too little datapoint
# Regression Template
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 8 - Decision Tree Regression/R")
# Importing the dataset where string variable is treated as factor
df = read.csv('Position_Salaries.csv',stringsAsFactors = TRUE)

library(rpart)
# Create Regression
reg = rpart(formula = Salary ~ Level,data = df,control = rpart.control(minsplit = 1L))

# Create smoother levels and predict results
Lvls = data.frame(Level = (1:1000)*0.01)
Lvls$reg = predict(reg,newdata = Lvls)

# Visualize regression result
# Plot
library(ggplot2)
colors <- c("data" = "black", "reg" = "red")
ggplot() + geom_point(aes(x = df$Level ,y = df$Salary,color = "black")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$reg,color = "reg")) +
  ggtitle("Decision Tree Regression") + xlab("Level") + ylab("Salary") +
  scale_color_manual(values=colors)



