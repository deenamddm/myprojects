library(ggplot2)
cardata <- read.table("auto-mpg.data")
head(cardata)
names(cardata) <- c("MPG", "Cylinders", "Displacement", "Horsepower", "Weight", 
                    "Accelaration", "ModelYear", "Origin", "CarName")

# checking for missing values
is.na(cardata$Horsepower) 
# does not show any missing values. But it is mentioned in the auto-mpg.names
print(cardata$Horsepower) # now we can clearly see ? in place of missing values
print(cardata$Horsepower[c(33,127,331,337,355, 375)]) # rows that has missing values in horsepower


# charts for all possible pairs
pairs(cardata)
pairs(~ MPG + Weight + Displacement + Accelaration + Horsepower, data=cardata)
?pairs

plot(cardata$Weight, cardata$MPG, main="Scatterplot Weight vs MPG") # possible candidate for linear regression
plot(cardata$Cylinders, cardata$MPG, main="Scatterplot Weight vs MPG") # not a candidate
plot(cardata$Displacement, cardata$MPG, main="Scatterplot Displacement vs MPG") # may be a candidate for linear regression
plot(cardata$Accelaration, cardata$MPG, main="Scatterplot Acceleration vs MPG") # very unliqely to be a candidate for linear regression
plot(cardata$Horsepower, cardata$MPG, main="Scatterplot Weight vs MPG") # check plot from ggplot - very unliqely candidate for linear regression


# Checking correlations between continuous variables
cor(cardata$Weight, cardata$MPG)
cor(cardata$Displacement, cardata$MPG)
cor(cardata$Accelaration, cardata$MPG)



#Splitting up the data for training & testing
cardataTraining <- cardata[1:300, ]
cardataTest <- cardata[301:398,]


#Building Model
cardataLinReg <- lm(MPG ~ Weight, data = cardataTraining)
summary(cardataLinReg)


# Calculating predicted values 
cardataLinReg$coefficients
cardataLinReg$coefficients[1]
cardataLinReg$coefficients[2]

predicted_y_sv_cardata <- cardataLinReg$coefficients[1] +
      cardataLinReg$coefficients[2] * cardataTest$Weight


# Calculating model error
sv_modelerror <- cardataTest$MPG - predicted_y_sv_cardata
hist(sv_modelerror, breaks = 30, col='light green')
plot(cardataTest$MPG, predicted_y_sv_cardata, col="red", 
     main="Actual MPG vs Predicted MPG")


# Creating charts for model
hist(cardataLinReg$residuals, breaks = 30)
hist(cardataLinReg$residuals, breaks = 30, probability = T, 
     col = "light blue",
     main = "Histogram of Residuals",
     xlab = "Residuals")
x <- seq(-15, 15, length = 1000)
y <- dnorm(x, mean = mean(cardataLinReg$residuals),sd = sd(cardataLinReg$residuals))
lines(x,y,col="Purple")

qqnorm(cardataLinReg$residuals, col="Dark Blue")
qqline(cardataLinReg$residuals, col="Green")

plot(cardataLinReg$fitted.values, cardataLinReg$residuals)
plot(cardataLinReg$fitted.values, abs(cardataLinReg$residuals))

plot(cardataTraining$Weight, cardataLinReg$residuals, 
     col = "Dark Green",
     main = "Predictor Variable (Weight) vs. Residuals",
     xlab = "Predictor Variable (Weight)",
     ylab = "Residuals")
plot(cardataTraining$Weight, abs(cardataLinReg$residuals),
     col = "Dark Red",
     main = "Predictor Variable (Weight) vs. Abs(Residuals)",
     xlab = "Predictor Variable (Weight)",
     ylab = "Abs(Residuals)")













# Multi-variable linear regression

# Normalizing explanatory variables
head(cardata)
cardata$norm_disp <- (cardata$Displacement - mean(cardata$Displacement))/sd(cardata$Displacement)
cardata$norm_hp <- (cardata$Horsepower - mean(cardata$Horsepower))/sd(cardata$Horsepower)
# because of question marks (missing values) all norm_hp will be NA - We can omit HP from multivariable linear regression
cardata$norm_wt <- (cardata$Weight - mean(cardata$Weight))/sd(cardata$Weight)
cardata$norm_acc <- (cardata$Accelaration - mean(cardata$Accelaration))/sd(cardata$Accelaration)



#getting only required columns for multi-variable linear regression
head(cardata[,c(1,7,8,9,10,12,13)])
mod_cardata <- cardata[,c(1,7,8,9,10,12,13)]
head(mod_cardata)


#Splitting data as training & test data
mod_cardata_train <- mod_cardata[1:300,]
mod_cardata_test <- mod_cardata[301:398,]


multi_val_reg1 <- lm(MPG ~ norm_wt + norm_disp, data = mod_cardata_train)
multi_val_reg2 <- lm(MPG ~ norm_wt + norm_disp + norm_acc, data = mod_cardata_train)
multi_val_reg3 <- lm(MPG ~ norm_disp + norm_acc, data = mod_cardata_train)
multi_val_reg4 <- lm(MPG ~ norm_wt + norm_acc, data = mod_cardata_train)

summary(multi_val_reg1)
summary(multi_val_reg2)
summary(multi_val_reg3)
summary(multi_val_reg4)
# norm_acc is not significant in multi variable linear regression
# hence we will proceed with multi_val_reg1

# Predicted value
multi_val_reg1$coefficients[1]

pred_y_mv_cardata1 <- multi_val_reg1$coefficients[1] + 
      multi_val_reg1$coefficients[2]*mod_cardata_test$norm_wt +
      multi_val_reg1$coefficients[3]*mod_cardata_test$norm_disp


# Calculating multi-variable model error
mv_modelerror <- mod_cardata_test$MPG - pred_y_mv_cardata1
hist(mv_modelerror, breaks = 30, main="Histogram of Model Error", xlab = "MPG", col = "light blue")
plot(mod_cardata_test$MPG, pred_y_mv_cardata1, col='dark green', main="Actual MPG vs Predicted MPG", 
     xlab = "Actual MPG",
     ylab = "Predicted MPG")



# Creating charts for multi-variable model
hist(multi_val_reg1$residuals, breaks = 30, main="Histogram of Residuals",
                              xlab = "Residuals", col = "light blue")
hist(multi_val_reg1$residuals, breaks = 30, probability = T, main="Histogram of Residuals",
     xlab = "Residuals", col = "light blue")
x <- seq(-10, 15, length = 1000)
y <- dnorm(x, mean = mean(multi_val_reg1$residuals),sd = sd(multi_val_reg1$residuals))
lines(x,y,col="Purple")

qqnorm(multi_val_reg1$residuals, col="Dark Blue")
qqline(multi_val_reg1$residuals, col="Dark Green")

plot(multi_val_reg1$fitted.values, multi_val_reg1$residuals, 
     col = "Dark Green",
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted values",
     ylab = "Residuals")
plot(multi_val_reg1$fitted.values, abs(multi_val_reg1$residuals),
     col = "Dark Blue",
     main = "Abs. Residuals vs. Fitted Values",
     xlab = "Fitted values",
     ylab = "Abs. Residuals")






plot (mod_cardata_train$norm_wt, multi_val_reg1$residuals,
     col = "Dark Blue",
     main = "Residuals vs. Weight",
     xlab = "Weight",
     ylab = "Residuals")

plot (mod_cardata_train$norm_disp, multi_val_reg1$residuals,
     col = "Dark Blue",
     main = "Residuals vs. Displacement",
     xlab = "Displacement",
     ylab = "Residuals")






plot(mod_cardata_train$norm_wt, abs(multi_val_reg1$residuals),
     col = "Dark Blue",
     main = "Abs. Residuals vs. Weight",
     xlab = "Weight",
     ylab = "Abs. Residuals")

plot(mod_cardata_train$norm_disp, abs(multi_val_reg1$residuals),
     col = "Dark Blue",
     main = "Abs. Residuals vs. Displacement",
     xlab = "Displacement",
     ylab = "Abs. Residuals")





















ggplot(cardata, aes(cardata$Weight, cardata$MPG, color = "Red")) +
      geom_point(show.legend = F) + theme_minimal()
ggplot(cardata, aes(cardata$Horsepower, cardata$MPG, color = "Red")) + 
      geom_point(show.legend = F) + theme_minimal()
