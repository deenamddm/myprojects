library(ggplot2)

# Loading the data set to R
proj_data <- read.csv("Heart.csv")

# Exploring the data set
head(proj_data)
summary(proj_data)
str(proj_data)


# Splitting up training data and test data
train_data <- proj_data[1:250, ]
test_data <- proj_data[251:303, ]
summary(test_data)
str(test_data)

# Model 
log_model <- glm(AHD ~ ., family = "binomial", data = train_data)
summary(log_model) # AIC 182.08
mod_pred <- predict(log_model, newdata = test_data, type = "response")
cm <- table(test_data$AHD, mod_pred > 0.5) # Confusion matrix for Model 
cm
tn <- cm[1,1]
fp <- cm[1,2]
fn <- cm[2,1]
tp <- cm[2,2]
accuracy <- sum(diag(cm))/sum(cm)
accuracy # 76%
library(ROCR)
m_pred <- prediction(mod_pred, test_data$AHD)
eval <- performance(m_pred, "acc")
plot(eval)
roc <- performance(m_pred, "tpr", "fpr")
plot(roc)


# Model 1
log_model1 <- glm(AHD ~ Age + Sex + ChestPain + RestBP + Ca, family = "binomial", data = train_data)
summary(log_model1) # AIC 214.87
mod1_pred <- predict(log_model1, newdata = test_data, type = "response")
cm1 <- table(test_data$AHD, mod1_pred > 0.5) # Confusion matrix for Model1 
cm1
tn1 <- cm1[1,1]
fp1 <- cm1[1,2]
fn1 <- cm1[2,1]
tp1 <- cm1[2,2]
accuracy1 <- sum(diag(cm1))/sum(cm1)
accuracy1 # 71%
library(ROCR)
m1_pred <- prediction(mod1_pred, test_data$AHD)
eval1 <- performance(m1_pred, "acc")
plot(eval1)
roc1 <- performance(m1_pred, "tpr", "fpr")
plot(roc1)





# Model 2
log_model2 <- glm(AHD ~ ChestPain, family = "binomial", data = train_data)
summary(log_model2) # AIC 274.97
mod2_pred <- predict(log_model2, newdata = test_data, type = "response")
cm2 <- table(test_data$AHD, mod2_pred > 0.5) # Confusion matrix for Model1 
cm2
tn2 <- cm2[1,1]
fp2 <- cm2[1,2]
fn2 <- cm2[2,1]
tp2 <- cm2[2,2]
accuracy2 <- sum(diag(cm2))/sum(cm2)
accuracy2 # 72%
library(ROCR)
m2_pred <- prediction(mod2_pred, test_data$AHD)
eval2 <- performance(m2_pred, "acc")
plot(eval2)
roc2 <- performance(m2_pred, "tpr", "fpr")
plot(roc2)




# Model 3
log_model3 <- glm(AHD ~ ChestPain + Ca, family = "binomial", data = train_data)
summary(log_model3) # AIC 237.47
mod3_pred <- predict(log_model3, newdata = test_data, type = "response")
cm3 <- table(test_data$AHD, mod3_pred > 0.5) # Confusion matrix for Model 3
cm3
tn3 <- cm3[1,1]
fp3 <- cm3[1,2]
fn3 <- cm3[2,1]
tp3 <- cm3[2,2]
accuracy3 <- sum(diag(cm3))/sum(cm3)
accuracy3 # 73%
library(ROCR)
m3_pred <- prediction(mod3_pred, test_data$AHD)
eval3 <- performance(m3_pred, "acc")
plot(eval3)
roc3 <- performance(m3_pred, "tpr", "fpr")
plot(roc3)




# Model 4
log_model4 <- glm(AHD ~ Age + Sex + ChestPain + RestBP + 
                        MaxHR + ExAng + Ca , family = "binomial", data = train_data)
summary(log_model4) # AIC 197.16
mod4_pred <- predict(log_model4, newdata = test_data, type = "response")
cm4 <- table(test_data$AHD, mod4_pred > 0.5) # Confusion matrix for Model 4
cm4
tn4 <- cm4[1,1]
fp4 <- cm4[1,2]
fn4 <- cm4[2,1]
tp4 <- cm4[2,2]
accuracy4 <- sum(diag(cm4))/sum(cm4)
accuracy4 # 73%
library(ROCR)
m4_pred <- prediction(mod4_pred, test_data$AHD)
eval4 <- performance(m4_pred, "acc")
plot(eval4)
roc4 <- performance(m4_pred, "tpr", "fpr")
plot(roc4)



# Model 5
log_model5 <- glm(AHD ~ Sex + ChestPain + RestBP + 
                        MaxHR + ExAng + Ca , family = "binomial", data = train_data)
summary(log_model5) # AIC 195.27
mod5_pred <- predict(log_model5, newdata = test_data, type = "response")
cm5 <- table(test_data$AHD, mod5_pred > 0.5) # Confusion matrix for Model 5
cm5
tn5 <- cm5[1,1]
fp5 <- cm5[1,2]
fn5 <- cm5[2,1]
tp5 <- cm5[2,2]
accuracy5 <- sum(diag(cm5))/sum(cm5)
accuracy5 # 73%
library(ROCR)
m5_pred <- prediction(mod5_pred, test_data$AHD)
eval5 <- performance(m5_pred, "acc")
plot(eval5)
roc5 <- performance(m5_pred, "tpr", "fpr")
plot(roc5)



