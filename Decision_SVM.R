# Load necessary libraries
library(caTools)
library(e1071)
library(ggplot2)
library(C50)

# Function for Program 1 (SVM)
run_svm_program <- function() {
  # Load dataset
  dataset = read.csv('/mnt/data/Social_Network_Ads.csv')
  
  # Selecting relevant columns
  dataset = dataset[, c("Age", "EstimatedSalary", "Purchased")]
  
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
  
  # Fitting SVM with RBF Kernel
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
}

# Function for Program 2 (Decision Tree with Mushroom Dataset)
run_decision_tree_program <- function() {
  # Load the dataset
  mushroom_data <- read.csv("/mnt/data/mushrooms.csv")
  
  # Convert categorical columns to factors
  mushroom_data[] <- lapply(mushroom_data, as.factor)
  
  # Train a C5.0 decision tree model
  model <- C5.0(class ~ ., data = mushroom_data)
  
  # Visualize the decision tree
  plot(model, main = "Decision Tree for Mushroom Classification", type = "simple")
  
  # Get user input for prediction
  get_user_input <- function() {
    input_data <- mushroom_data[1, -1]  # Copy column names and types
    
    for (col in names(input_data)) {
      cat(paste("Enter", col, "(" , paste(levels(mushroom_data[[col]]), collapse=", "), "): "))
      input_value <- readline()
      input_data[[col]] <- factor(input_value, levels = levels(mushroom_data[[col]]))
    }
    return(input_data)
  }
  
  cat("Provide mushroom features for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class (Edible or Poisonous):", prediction, "\n"))
}

# Main Menu
main_menu <- function() {
  while (TRUE) {
    cat("\n--- Main Menu ---\n")
    cat("1. Run SVM Program\n")
    cat("2. Run Decision Tree Program\n")
    cat("3. Exit\n")
    choice <- as.numeric(readline(prompt = "Enter your choice: "))
    
    if (choice == 1) {
      run_svm_program()
    } else if (choice == 2) {
      run_decision_tree_program()
    } else if (choice == 3) {
      cat("Exit\n")
      break
    } else {
      cat("Invalid choice\n")
    }
  }
}                         

# Run the main menu
main_menu()
