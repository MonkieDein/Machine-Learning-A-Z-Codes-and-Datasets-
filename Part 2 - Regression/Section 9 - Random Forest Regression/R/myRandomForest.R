# [Ensemble Method] Random Forest (Regression Tree)
# Instead of using one Decision Tress
# Random forest sample K from your datapoints many times (eg:500).
# Each times, solve a Decision Tree regression.
# Then take the average of all predictions.
#
# Set working directory
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 9 - Random Forest Regression/R")
# Importing the dataset where string variable is treated as factor
df = read.csv('Position_Salaries.csv',stringsAsFactors = TRUE)

library(randomForest)
# Set seed 
set.seed(1234)
# Create Regression
reg = randomForest(x = df["Level"],y = df$Salary,ntree = 100)

# Create smoother levels and predict results
Lvls = data.frame(Level = (1:1000)*0.01)
Lvls$reg = predict(reg,newdata = Lvls)

# Visualize regression result
# Plot
library(ggplot2)
colors <- c("data" = "black", "reg" = "red")
ggplot() + geom_point(aes(x = df$Level ,y = df$Salary,color = "black")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$reg,color = "reg")) +
  ggtitle("Random Forest Regression") + xlab("Level") + ylab("Salary") +
  scale_color_manual(values=colors)



