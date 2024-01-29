rm(list = ls())
library(ISLR2)
library(dplyr)
library(glmnet)
library(boot)
library(leaps)
library(MASS)
library(caret)
library(ggplot2)
# 读取和准备数据
data <- read.csv("C:/Users/86138/Desktop/722/finalproject/census_tract_data.csv")
data[is.na(data)] <- 0

x <- model.matrix(Poverty ~ . - X - State - County - CensusTract - ChildPoverty, data = data)[, -1]
y <- data$Poverty

# 设置固定的随机种子
set.seed(123)

# 创建10折交叉验证的折叠指标
foldid <- sample(1:10, size = length(y), replace = TRUE)

# 岭回归 - 使用10折交叉验证找到最优的lambda
cv.ridge <- cv.glmnet(x, y, alpha = 0, foldid = foldid)
optimal.lambda.ridge <- cv.ridge$lambda.min
optimal.rmse.ridge <- sqrt(min(cv.ridge$cvm))

# 套索回归 - 使用10折交叉验证找到最优的lambda
cv.lasso <- cv.glmnet(x, y, alpha = 1, foldid = foldid)
optimal.lambda.lasso <- cv.lasso$lambda.min
optimal.rmse.lasso <- sqrt(min(cv.lasso$cvm))

# 岭回归的系数路径图
plot(cv.ridge$glmnet.fit, xvar = "lambda", label = TRUE)
abline(v = log(optimal.lambda.ridge), col = "red", lwd = 2)
text(x = log(optimal.lambda.ridge), y = median(cv.ridge$glmnet.fit$beta[,which.min(cv.ridge$cvm)]), labels = paste("lambda =", round(optimal.lambda.ridge, 5), "\nRMSE =", round(optimal.rmse.ridge, 5)), pos = 4, col = "red")

# 套索回归的系数路径图
plot(cv.lasso$glmnet.fit, xvar = "lambda", label = TRUE)
abline(v = log(optimal.lambda.lasso), col = "blue", lwd = 2)
text(x = log(optimal.lambda.lasso), y = median(cv.lasso$glmnet.fit$beta[,which.min(cv.lasso$cvm)]), labels = paste("lambda =", round(optimal.lambda.lasso, 5), "\nRMSE =", round(optimal.rmse.lasso, 5)), pos = 4, col = "blue")