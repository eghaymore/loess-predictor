# Score
# Root of mean squared error 
library(MASS)

# Split into medv & example_vectors
medv_values_init = Boston[,14]
example_vectors_init = Boston[,1:13]

# Standardize units
example_vectors_init = scale(example_vectors_init)

# Inspect dataset
head(example_vectors_init)
head(medv_values_init)

# Initial test code ------
#Choose input vector -- Leave one out cross validation!
INPUT_VECTOR = example_vectors_init[99,]
TRUTH = medv_values_init[99]
medv_values = medv_values_init[-99]
example_vectors <- example_vectors_init[-99,]
# --------------------------

# Inspect
INPUT_VECTOR
head(example_vectors)

LEN = length(medv_values_init) - 1  # Adjust for leave one out
sq_error = 0

#for (i in LEN){
    # Choose input vector -- Leave one out cross validation!
    # INPUT_VECTOR = example_vectors_init[i,]
    # TRUTH = medv_values_init[i]
    # medv_values = medv_values_init[-i]
    # example_vecto  distances = c()

    # LOESS

    # 1. Determine distance from input feature vector to all other vectors
    distances = c()
    for (i in 1:LEN){
        distance = (INPUT_VECTOR - example_vectors[i])^2
        distances = c(distances,sum(distance))
    }
    head(distances)
    mean(distances)
    stripchart(distances)
    # 2. Apply weighting function 
    weight_func <- function(x){  
        exp(-(x^2))
        #exp(x)
    }
    distances = matrix(distances,LEN,1, byrow=TRUE)
    weights = apply(distances, 2, weight_func)

weights
s_wts
    # 3. Normalize weights
    s_wts = sum(weights)
    weights = weights/s_wts

    #Inspect
    weights
    # 4. Final Computation/Prediction
    weights = weights[,1] # Remove addition empty column

    #TEST
    sm = sum(distances)
    weights = distances/sm
    distances
    weights

    final_weights = matrix(weights, LEN, 1, byrow = TRUE)
    final_medv = matrix(medv_values, LEN, 1, byrow = TRUE)
    pred = t(weights)%*%(final_medv)

    #sq_error = sq_error + (pred-TRUTH)^2
#}

# MSE = sq_error/LEN
# MSE
# sqrt(MSE)

# PRed
pred
TRUTH

s = c(1,2,3,4)
stripchart(s)
