# Load required libraries
library(readr)
library(nnet)

# Load dataset
dataset <- read.csv("filepath")

# Train logistic regression model
model <- glm(Malignant ~ Tumor_Size + Thickness, data = dataset, family = binomial)

# Function to predict malignancy
predict_malignancy <- function(tumor_size, thickness, threshold = 0.5) {
  new_data <- data.frame(Tumor_Size = tumor_size, Thickness = thickness)
  prediction_prob <- predict(model, newdata = new_data, type = "response")
  prediction_class <- ifelse(prediction_prob > threshold, "Malignant", "Benign")
  return(list(probability = prediction_prob, class = prediction_class))
}

# Input values
tumor_size_input <- 120
thickness_input <- 50

# Make prediction
prediction <- predict_malignancy(tumor_size_input, thickness_input)
cat("Probability for malignancy:", prediction$probability, "\n")
cat("Class of prediction:", prediction$class, "\n")

# Load second dataset
dataset2 <- read.csv("path")

# Check dataset structure
str(dataset2)

# Train multinomial logistic regression model
model2 <- multinom(Type ~ Power + Seats + Price_in_Lakhs, data = dataset2)

# Function to make predictions
predict_type <- function(power, seats, price_in_lakhs) {
  new_data <- data.frame(Power = power, Seats = seats, Price_in_Lakhs = price_in_lakhs)
  prediction <- predict(model2, newdata = new_data, type = "probs")
  return(prediction)
}

# Input values
power_input <- 100
seats_input <- 2
price_in_lakhs_input <- 10

# Make prediction
prediction <- predict_type(power_input, seats_input, price_in_lakhs_input)

# Find the class with the highest probability
max_prob <- which.max(prediction)

# Define class names
class_names <- c("hybrid", "family", "suv", "sports")

# Get the class with the highest probability
class_max_prob <- class_names[max_prob]

# Print results
print(prediction)
cat("Type with the highest probability:", class_max_prob, "\n")


#INPUT
# Malignancy Prediction Inputs
tumor_size_input <- 120
thickness_input <- 50

# Multinomial Prediction Inputs
power_input <- 100
seats_input <- 5
price_in_lakhs_input <- 10

