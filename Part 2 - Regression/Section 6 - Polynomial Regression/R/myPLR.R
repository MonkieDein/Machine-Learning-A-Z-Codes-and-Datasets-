# Polynomial Regression
setwd("C:/GITHUB/Machine-Learning-A-Z-Codes-and-Datasets-/Part 2 - Regression/Section 6 - Polynomial Regression/R")
df = read.csv('Position_Salaries.csv',stringsAsFactors = TRUE)

# An employee is mentioned he make 160,000 being a level 6.5 workers previously
# We wanted to fit a Polynomial regression to see if his claim is T/F.
# 

# Fit Linear Regression
lin_reg = lm(formula = Salary ~ Level,data = df)

# Fit Quadratic Regression
quad_reg = lm(formula = Salary ~ Level + I(Level^2)  , data = df)

# Fit Cubic Regression
cub_reg = lm(formula = Salary ~ Level + I(Level^2) + I(Level^3)  , data = df)

# Fit Quartic Regression
quar_reg = lm(formula = Salary ~ poly(Level,4)  , data = df)

# Create smoother levels 
Lvls = data.frame(Level = (1:1000)*0.01)
Lvls$lin_reg = predict(lin_reg,newdata = Lvls)
Lvls$quad_reg = predict(quad_reg,newdata = Lvls)
Lvls$cub_reg = predict(cub_reg,newdata = Lvls)
Lvls$quar_reg = predict(quar_reg,newdata = Lvls)
# Plot
library(ggplot2)
colors <- c("data" = "black", "lin_reg" = "red","quad_reg" = "green", "cub_reg" = "blue","quar_reg" = "purple")
ggplot() + geom_point(aes(x = df$Level ,y = df$Salary,color = "black")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$lin_reg,color = "lin_reg")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$quad_reg,color = "quad_reg")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$cub_reg,color = "cub_reg")) +
  geom_line(aes(x = Lvls$Level ,y = Lvls$quar_reg,color = "quar_reg")) +
  ggtitle("Polynomial Regression") + xlab("Level") + ylab("Salary") +
  scale_color_manual(values=colors)

# Predictions
y_lin_pred = predict(lin_reg, data.frame(Level = 6.5))
y_quad_pred = predict(quad_reg, data.frame(Level = 6.5))
y_cub_pred = predict(cub_reg, data.frame(Level = 6.5))
y_quar_pred = predict(quar_reg, data.frame(Level = 6.5))


