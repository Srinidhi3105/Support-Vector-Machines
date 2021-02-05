salary_data <- read.csv(file.choose())
table(salary_data$Salary)
attach(salary_data)
salary_train <- salary_data[1:24129,]
salary_test <- salary_data[24130:30161,]

#building the model
library(kernlab)
library(caret)
model1 <- ksvm(Salary~.,data=salary_train,kernel="vanilladot")

#kernel=rfdot
model_rfdot <- ksvm(Salary~.,data=salary_train,kernel="rbfdot")
pred_rfdot <- predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary)

#kernel=vanilla
model_vanilla <-ksvm(Salary~.,data=salary_train,kernel="vanilladot")
pred_vanilla <- predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary)

#Kernel=besseldot
model_besseldot <- ksvm(Salary~.,data=salary_train,kernel="besseldot")
pred_besseldot <- predict(model_besseldot,newdata=salary_test)
mean(pred_besseldot==salary_test$Salary)
