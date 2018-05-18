# Importing the data
data <- read.csv('Position_Salaries.csv')
data <- data[2:3]


# Linear Regression 
Regressor <- lm(formula = Salary ~ ., data = data )

#Polynomial Regression 
data$Level1 <- data$Level^2
data$Level2 <- data$Level^3
data$Level3 <- data$Level^4

poly_Regressor <- lm(formula = Salary ~., data = data)

# Visualization 
library(ggplot2)
ggplot() + 
  geom_point(aes(x = data$Level  , y = data$Salary), colour = 'Red') +
  geom_line(aes(x = data$Level, y = predict(Regressor, newdata = data)),
                colour = 'Blue') + 
              ggtitle('plot') +
              xlab('Level') + 
              ylab('Salary')

# Visualization 1
ggplot() + 
  geom_point(aes(x = data$Level, y = data$Salary), color = 'Blue')+
  geom_line(aes(x = data$Level, y = predict(poly_Regressor, newdata = data)),
            color = 'Red') +
  ggtitle('Bluff or Truth')+ 
  xlab('Level') + 
  ylab('Salary')
  
  
  # prediction 
  y_pred <- predict(Regressor, data.frame(Level = 6.5))
  y_poly_pred <- predict(poly_Regressor, data.frame(Level = 6.5, 
                                                    Level1 = 6.5^2,
                                                    Level2 = 6.5^3,
                                                    Level3 = 6.5^4))
