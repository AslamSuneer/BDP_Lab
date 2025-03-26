# Load necessary libraries
library(nnet) # For multinomial logistic regression

# Function for Binomial Logistic Regression
binomial_logistic_regression <- function() {
  # Read data from CSV file
  data <- read.csv('/home/ds-ds-26/aslam/dia.csv')
  
  # Check if the required columns are present
  required_columns <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
                        "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")
  if (!all(required_columns %in% colnames(data))) {
    cat("Error: The CSV file must contain the following columns:\n")
    cat(paste(required_columns, collapse = ", "), "\n")
    return()
  }
  
  # Split data into training and testing sets (80% training, 20% testing)
  set.seed(123) # For reproducibility
  train_index <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Fit binomial logistic regression model
  model <- glm(Outcome ~ ., data = train_data, family = binomial())
  
  # Print model summary
  cat("\nBinomial Logistic Regression Model Summary:\n")
  print(summary(model))
  
  # Predict on the test data
  predicted_probabilities <- predict(model, newdata = test_data, type = "response")
  predicted_outcomes <- ifelse(predicted_probabilities >= 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(predicted_outcomes == test_data$Outcome)
  
  # Display the results
  cat("\nAccuracy for Binomial Logistic Regression:", accuracy, "\n")
  
  # Prompt user to add new data for prediction
  cat("\nEnter new data for prediction:\n")
  new_data <- data.frame(
    Pregnancies = as.numeric(readline(prompt = "Pregnancies: ")),
    Glucose = as.numeric(readline(prompt = "Glucose: ")),
    BloodPressure = as.numeric(readline(prompt = "BloodPressure: ")),
    SkinThickness = as.numeric(readline(prompt = "SkinThickness: ")),
    Insulin = as.numeric(readline(prompt = "Insulin: ")),
    BMI = as.numeric(readline(prompt = "BMI: ")),
    DiabetesPedigreeFunction = as.numeric(readline(prompt = "DiabetesPedigreeFunction: ")),
    Age = as.numeric(readline(prompt = "Age: "))
  )
  
  # Predict probability for new data
  predicted_probability <- predict(model, newdata = new_data, type = "response")
  predicted_outcome <- ifelse(predicted_probability >= 0.5, 1, 0)
  
  # Display the result
  cat("\nPredicted Probability:", predicted_probability, "\n")
  cat("Predicted Outcome (0 or 1):", predicted_outcome, "\n")
}

# Function for Multinomial Logistic Regression
multinomial_logistic_regression <- function() {
  # Load iris dataset
  data(iris)
  
  # Split data into training and testing sets (80% training, 20% testing)
  set.seed(123) # For reproducibility
  train_index <- sample(1:nrow(iris), 0.8 * nrow(iris))
  train_data <- iris[train_index, ]
  test_data <- iris[-train_index, ]
  
  # Fit multinomial logistic regression model
  model <- multinom(Species ~ ., data = train_data)
  
  # Print model summary
  cat("\nMultinomial Logistic Regression Model Summary:\n")
  print(summary(model))
  
  # Predict on the test data
  predictions <- predict(model, newdata = test_data)
  
  # Calculate accuracy
  accuracy <- mean(predictions == test_data$Species)
  
  # Display the results
  cat("\nAccuracy for Multinomial Logistic Regression:", accuracy, "\n")
  
  # Prompt user to add new data for prediction
  cat("\nEnter new data for prediction:\n")
  new_data <- data.frame(
    Sepal.Length = as.numeric(readline(prompt = "Sepal.Length: ")),
    Sepal.Width = as.numeric(readline(prompt = "Sepal.Width: ")),
    Petal.Length = as.numeric(readline(prompt = "Petal.Length: ")),
    Petal.Width = as.numeric(readline(prompt = "Petal.Width: "))
  )
  
  # Predict species for new data
  prediction <- predict(model, newdata = new_data)
  cat("\nPredicted Species:", as.character(prediction), "\n")
}

# Menu-driven program
while (TRUE) {
  cat("\nMenu:\n")
  cat("1. Binomial Logistic Regression\n")
  cat("2. Multinomial Logistic Regression\n")
  cat("3. Exit\n")
  choice <- as.numeric(readline(prompt = "Enter your choice: "))
  
  if (choice == 1) {
    binomial_logistic_regression()
  } else if (choice == 2) {
    multinomial_logistic_regression()
  } else if (choice == 3) {
    cat("Exit\n")
    break
  } else {
    cat("Invalid\n")
  }
}
