data_ccc = as.data.frame(default_of_credit_card_clients)

smp_size <- floor(0.7 * nrow(data_ccc))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_ccc)), size = smp_size)

train_ccc <- data_ccc[train_ind,1:24]
test_ccc <- data_ccc[-train_ind, ]
test_ccc_feature = test_ccc[,1:24] 
test_ccc_class = test_ccc[,25]
Y = data_ccc[train_ind,25]
X = cbind(train_ccc,Y)

# Logistic Regression
lr.model <- glm(Y ~ .-Y,data=X)
summary(lr.model)
fitted.results <- predict(lr.model,newdata=test_ccc_feature,type='response')
fitted.results <- ifelse(fitted.results > 0.55,1,0)
misClasificError <- mean(fitted.results != test_ccc$`default payment next month`)
accuracy_lr = 1 - misClasificError
print(paste('Accuracy LR',1-misClasificError))



# SVM
#install.packages("e1071")
library(e1071)
svm1 <- svm(Y ~ .-Y, data=X)
# Predictions
predicted_svm = predict(svm1,test_ccc_feature)
accuracy_svm = sum(test_ccc_class == round(predicted)) / length(test_ccc_class)
print(paste('Accuracy for svm',accuracy_svm))



# Random Forest prediction of seeds data
#install.packages("randomForest")
library(randomForest)
fit <- randomForest(Y ~ .-Y, data=X)
print(fit) # view results 
importance(fit) # importance of each predictor
predicted_rf = predict(fit,test_ccc_feature)
accuracy_rf = sum(test_ccc_class == round(predicted)) / length(test_ccc_class)
print(paste('Accuracy for svm',accuracy_rf))


results <- sample(list(logistic=lr.model, svm=svm1,rf=fit))
# Table comparison
summary(results)


classifier_name = c("Logistic Regression" = accuracy_lr ,"SVM" = accuracy_svm,"Random Forests" = accuracy_lr)




