# Load necessary libraries
library(caTools)
library(e1071)
library(ggplot2)
library(C50)
library(rpart)
library(rpart.plot)

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
  
  # Function to visualize decision boundary
  plot_decision_boundary <- function(set, title) {
    X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
    X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
    
    grid_set = expand.grid(X1, X2)
    colnames(grid_set) = c('Age', 'EstimatedSalary')
    y_grid = predict(classifier, newdata = grid_set)
    
    plot(set[, -3], 
         main = title, 
         xlab = 'Age', ylab = 'Estimated Salary', 
         xlim = range(X1), ylim = range(X2))
    
    contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
    
    points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine'))
    
    points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
  }
  
  # Visualizing the Training set results
  plot_decision_boundary(training_set, "SVM with RBF Kernel (Training set)")
  
  # Visualizing the Test set results
  plot_decision_boundary(test_set, "SVM with RBF Kernel (Test set)")
  
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
  
  # Train a Decision Tree model using rpart
  model <- rpart(class ~ ., data = mushrooms_data, method = "class")
  
  # Function for custom user input
  get_user_input <- function() {
    user_input <- list()
    features <- names(mushrooms_data)[-1]  # Exclude target column 'class'
    
    for (feature in features) {
      cat(paste0("Enter ", feature, " (", paste(levels(mushrooms_data[[feature]]), collapse=", "), "): "))
      user_input[[feature]] <- factor(readline(), levels = levels(mushrooms_data[[feature]]))
    }
    
    return(as.data.frame(user_input))
  }
  
  # Visualize the decision tree using rpart.plot
  rpart.plot(model, main = "Decision Tree Visualization", type = 3, extra = 101)
  
  # Get user input and make a prediction
  cat("Provide input values for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data, type = "class")
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
