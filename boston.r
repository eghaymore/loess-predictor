# Score
# Root of mean squared error 
library(MASS)
model <- lm(medv ~ ., data = Boston)

# Extract predictor variables from the first row of the dataset
predictor_values <- Boston[1, -which(names(Boston) == "medv")]  # Exclude the dependent variable 'medv'

# Make prediction using the fitted model
prediction <- predict(model, newdata = data.frame(t(predictor_values)))

# Print the prediction
print(prediction)












head(Boston)
dim(Boston)


# Plot data by medv
dat = Boston[,13]
dat = dat/max(dat)
medv = Boston[,14]
dat_medv = cbind(dat,medv)

# Plot the scatterplot
plot(dat_medv[,1], dat_medv[,2], main = "Scatterplot of Matrix", xlab = "Data", ylab = "Median Home Value", col = "blue", pch = 19)


#Scale all data except medv
examples = scale(Boston[,1:13])
# Remove non predictive variables
examples = examples[,-2]# zn
examples = examples[,-3]# chas
examples = examples[,-6]# dis
examples = examples[,-6]# rad
examples = examples[,-8]# bk

# Merge medv
examples = cbind(examples,Boston[,14])
head(examples)

# # Initial test code ------
#Choose input vector -- Leave one out cross validation!
INPUT_VECTOR = examples[1,1:8]
TRUTH = examples[1,9]
examples = examples[-1,]
# # --------------------------

# # Inspect
INPUT_VECTOR
head(examples)
TRUTH

model <- lm(mpg ~ wt + hp + qsec, data = data)

# sq_error = 0

# #for (i in LEN){
#     # Choose input vector -- Leave one out cross validation!
#     # INPUT_VECTOR = example_vectors_init[i,]
#     # TRUTH = medv_values_init[i]
#     # medv_values = medv_values_init[-i]
#     # example_vecto  distances = c()

#     # LOESS

#     # 1. Determine distance from input feature vector to all other vectors
    # distances = c()
    # for (i in 1:LEN){
    #     distance = (INPUT_VECTOR - examples[i,1:8])^2
    #     distances = sqrt(c(distances,sum(distance)))
    # }
    # head(distances)
    # mean(distances)

    # stripchart(distances)
    # distances

#     # 2. Apply weighting function 
#     weight_func <- function(x){  
#         exp(-(x^2))
#         #exp(x)
#     }
#     distances = matrix(distances,LEN,1, byrow=TRUE)
#     weights = apply(distances, 2, weight_func)

# weights
# s_wts
#     # 3. Normalize weights
#     s_wts = sum(weights)
#     weights = weights/s_wts

#     #Inspect
#     weights
#     # 4. Final Computation/Prediction
#     weights = weights[,1] # Remove addition empty column

#     #TEST
#     sm = sum(distances)
#     weights = distances/sm
#     distances
#     weights

#     final_weights = matrix(weights, LEN, 1, byrow = TRUE)
#     final_medv = matrix(medv_values, LEN, 1, byrow = TRUE)
#     pred = t(weights)%*%(final_medv)

#     #sq_error = sq_error + (pred-TRUTH)^2
# #}

# # MSE = sq_error/LEN
# # MSE
# # sqrt(MSE)

# # PRed
# pred
# TRUTH

# s = c(1,2,3,4)
# stripchart(s)
