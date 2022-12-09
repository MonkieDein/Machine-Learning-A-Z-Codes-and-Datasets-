# Association Rule - Apriori Algorithm
# 
#
setwd("~/Desktop/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 5 - Association Rule Learning/Section 28 - Apriori/R")

# Import dataste
df = read.csv("Market_Basket_Optimisation.csv",header = FALSE)
# install.packages("arules")
library(arules)
tf = read.transactions("Market_Basket_Optimisation.csv",sep=',',rm.duplicates = TRUE)
summary(tf)
itemFrequencyPlot(tf,topN = 20)

# Training Apriori on the dataset
rules = apriori(data = tf, parameter = list(support = 0.005,confidence = 0.2))

# Visualizing the result
inspect(sort(rules,by='lift')[1:10])
