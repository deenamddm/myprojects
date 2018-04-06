library(e1071)

# Loading the data set to R
proj_data_svm <- read.csv("Heart.csv")

# Exploring the data set
head(proj_data_svm)
summary(proj_data_svm)
str(proj_data_svm)

# Splitting up training data and test data
train_data_svm <- proj_data_svm[1:250, ]
test_data_svm <- proj_data_svm[251:303, ]
test_data_svm <- na.omit(test_data_svm)


# Model 1
# Building Model & Prediction
svm_proj_mod1 <- svm(AHD ~ ., data=train_data_svm)
summary(svm_proj_mod1)
svm_proj_pred1 <- predict(svm_proj_mod1, newdata=test_data_svm)
svm_proj_pred1
# confusion matrix
svm_cm1 <- table(svm_proj_pred1, test_data_svm$AHD)
svm_cm1
acc_svm1 <- sum(diag(svm_cm1))/sum(svm_cm1)
acc_svm1 # 78%

#svm_proj_mod1_1 <- svm(AHD ~., data = train_data_svm, kernel="radial", cost = 1.5, gamma=0.1)
#summary(svm_proj_mod1_1)
#svm_proj_pred1_1 <- predict(svm_proj_mod1_1, newdata=test_data_svm)
#svm_cm1_1 <- table(svm_proj_mod1_1, test_data_svm$AHD)
#acc_svm1_1 <- sum(diag(svm_cm1_1))/sum(svm_cm1_1)
#acc_svm1_1

# Model 2
# Building Model & Prediction
svm_proj_mod2 <- svm(AHD ~ ChestPain, data=train_data_svm)
summary(svm_proj_mod2)
svm_proj_pred2 <- predict(svm_proj_mod2, newdata=test_data_svm)
svm_proj_pred2
# confusion matrix
svm_cm2 <- table(svm_proj_pred2, test_data_svm$AHD)
svm_cm2
acc_svm2 <- sum(diag(svm_cm2))/sum(svm_cm2)
acc_svm2 # 70%




# Model 3
# Building Model & Prediction
svm_proj_mod3 <- svm(AHD ~ ChestPain + Ca, data=train_data_svm)
summary(svm_proj_mod3)
svm_proj_pred3 <- predict(svm_proj_mod3, newdata=test_data_svm)
svm_proj_pred3
# confusion matrix
svm_cm3 <- table(svm_proj_pred3, test_data_svm$AHD)
svm_cm3
acc_svm3 <- sum(diag(svm_cm3))/sum(svm_cm3)
acc_svm3 # 70%




# Model 4
# Building Model & Prediction
svm_proj_mod4 <- svm(AHD ~ Age + Sex + ChestPain + RestBP + 
                           MaxHR + ExAng + Ca, data=train_data_svm)
summary(svm_proj_mod4)
svm_proj_pred4 <- predict(svm_proj_mod4, newdata=test_data_svm)
svm_proj_pred4
# confusion matrix
svm_cm4 <- table(svm_proj_pred4, test_data_svm$AHD)
svm_cm4
acc_svm4 <- sum(diag(svm_cm4))/sum(svm_cm4)
acc_svm4 # 76%



# Model 5
# Building Model & Prediction
svm_proj_mod5 <- svm(AHD ~ Sex + ChestPain + RestBP + 
                           MaxHR + ExAng + Ca, data=train_data_svm)
summary(svm_proj_mod5)
svm_proj_pred5 <- predict(svm_proj_mod5, newdata=test_data_svm)

svm_proj_pred5
# confusion matrix
svm_cm5 <- table(svm_proj_pred5, test_data_svm$AHD)
svm_cm5
acc_svm5 <- sum(diag(svm_cm5))/sum(svm_cm5)
acc_svm5 # 78%









































