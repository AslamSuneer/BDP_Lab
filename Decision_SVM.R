# Load necessary libraries
library(caTools)
library(e1071)
library(ggplot2)
library(C50)

# Function for Program 1 (SVM)
run_svm_program <- function() {
  # Load dataset
  dataset = read.csv('/home/ds-da-07/Nibras/Social_Network_Ads.csv')
  
  # Selecting relevant columns
  dataset = dataset[, 3:5]  # Keeping only Age, EstimatedSalary, and Purchased
  
  # Encoding the target variable as a factor
  dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
  
  # Splitting dataset into training and test sets
  set.seed(123)
  split = sample.split(dataset$Purchased, SplitRatio = 0.75)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  
  # Feature Scaling
  training_set[, 1:2] = scale(training_set[, 1:2])
  test_set[, 1:2] = scale(test_set[, 1:2])
  
  # Fitting SVM with RBF Kernel (Non-Linear)
  classifier = svm(formula = Purchased ~ ., 
                   data = training_set, 
                   type = 'C-classification', 
                   kernel = 'radial')
  
  # Predicting the Test set results
  y_pred = predict(classifier, newdata = test_set[, -3])
  
  # Confusion Matrix
  cm = table(test_set[, 3], y_pred)
  print("Confusion Matrix:")
  print(cm)
  
  # User Input for Prediction
  pred1 <- function() {
    # Get user input for Age and Estimated Salary
    new_age = as.numeric(readline(prompt = "Enter Age: "))
    new_salary = as.numeric(readline(prompt = "Enter Estimated Salary: "))
    
    # Handle invalid input
    if (is.na(new_age) | is.na(new_salary)) {
      stop("Invalid input! Please enter numeric values.")
    }
    
    # Scale the new input using the same scaling as training data
    new_data = data.frame(Age = (new_age - mean(dataset$Age)) / sd(dataset$Age),
                          EstimatedSalary = (new_salary - mean(dataset$EstimatedSalary)) / sd(dataset$EstimatedSalary))
    
    # Predict class for new input
    prediction = predict(classifier, newdata = new_data)
    print(prediction)
    
    # Display result
    if (prediction == 1) {
      cat("\nPrediction: The person is likely to Purchase.\n")
    } else {
      cat("\nPrediction: The person is NOT likely to Purchase.\n")
    }
  }
  
  # Call the function
  pred1()
}

# Function for Program 2 (Decision Tree)
run_decision_tree_program <- function() {
  # Load the dataset
  mushrooms_data <- read.csv("/home/ds-da-07/Nibras/mushrooms.csv")
  
  # Convert categorical columns to factors
  for (col in names(mushrooms_data)) {
    mushrooms_data[[col]] <- factor(mushrooms_data[[col]])
  }
  
  # Train a C5.0 decision tree model
  model <- C5.0(class ~ ., data = mushrooms_data)
  
  # Function for custom user input
  get_user_input <- function() {
    user_input <- list()
    features <- names(mushrooms_data)[-1]  # Exclude target column 'class'
    
    for (feature in features) {
      repeat {
        cat(paste0("Enter ", feature, " (", paste(levels(mushrooms_data[[feature]]), collapse=", "), "): "))
        input_value <- readline()
        
        if (input_value %in% levels(mushrooms_data[[feature]])) {
          user_input[[feature]] <- factor(input_value, levels = levels(mushrooms_data[[feature]]))
          break
        } else {
          cat("Invalid input. Please enter a valid category.\n")
        }
      }
    }
    
    return(as.data.frame(user_input))
  }
  
  # Visualize the decision tree using C5.0's built-in plot function
  plot(model, main = "Decision Tree Visualization", type = "simple")
  
  # Get user input and make a prediction
  cat("Provide input values for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class:", prediction, "\n"))
}

# Main Menu
main_menu <- function() {
  while (TRUE) {
    cat("\n--- Main Menu ---\n")
    cat("1. Run SVM Program\n")
    cat("2. Run Decision Tree Program\n")
    cat("3. Exit\n")
    choice <- as.numeric(readline(prompt = "Enter your choice (1, 2, or 3): "))
    
    if (choice == 1) {
      run_svm_program()
    } else if (choice == 2) {
      run_decision_tree_program()
    } else if (choice == 3) {
      cat("Exiting the program. Goodbye!\n")
      break
    } else {
      cat("Invalid choice! Please try again.\n")
    }
  }
}                         

# Run the main menu
main_menu()
