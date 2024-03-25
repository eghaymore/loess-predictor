library(MASS)
head(Boston)
summary(Boston)
str(Boston)

# https://blogs.sas.com/content/iml/2021/12/01/beware-repeated-values-loess.html

# set.seed(123)
# train_index <- sample(1:nrow(Boston), 0.7 * nrow(Boston))
train_data <- Boston[1:450, ]
test_data <- Boston[450:506, ]



# Example: Linear Regression
model <- lm(medv ~ ., data = train_data)

predictions <- predict(model, newdata = test_data)
mse <- mean((predictions - test_data$medv)^2)
sqrt(mse)

# Example: Predict the mean every time
mean(Boston[,14])
mse2 <- mean((22.53281 - test_data$medv)^2)
sqrt(mse2)

loess_model <- loess(medv ~ lstat + crim + rm + indus, data = train_data)
# Subset test data to include only the predictors used in the loess model
predictors_test_data <- test_data[, c("lstat", "crim", "rm", "indus")]


# Predict using the loess model with the subsetted test data
loess_predictions <- predict(loess_model, newdata = predictors_test_data)
dim(predictors_test_data)

temp1 = loess_predictions[1:38]
temp2 = test_data$medv[1:38]


loess_mse <- mean((temp1 - temp2)^2)
loess_mse
loess_rmse <- sqrt(loess_mse)
loess_rmse
