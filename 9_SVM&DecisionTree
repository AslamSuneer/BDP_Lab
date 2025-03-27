# Load necessary libraries
library(caTools)
library(e1071)
library(ggplot2)
library(C50)

# Function for Program 1 (SVM using Iris dataset)
run_svm_program <- function() {
  # Load dataset
  dataset <- iris
  
  # Encode Setosa as 1 and others as 0
  dataset$Species <- as.factor(ifelse(dataset$Species == "setosa", "Setosa", "Non-Setosa"))
  
  # Splitting dataset into training and test sets
  set.seed(123)
  split <- sample.split(dataset$Species, SplitRatio = 0.75)
  training_set <- subset(dataset, split == TRUE)
  test_set <- subset(dataset, split == FALSE)
  
  # Save mean and standard deviation for scaling
  feature_means <- colMeans(training_set[, 1:4])
  feature_sds <- apply(training_set[, 1:4], 2, sd)
  
  # Feature Scaling
  training_set[, 1:4] <- scale(training_set[, 1:4], center = feature_means, scale = feature_sds)
  test_set[, 1:4] <- scale(test_set[, 1:4], center = feature_means, scale = feature_sds)
  
  # Fitting SVM with RBF Kernel
  classifier <- svm(formula = Species ~ ., 
                    data = training_set, 
                    type = 'C-classification', 
                    kernel = 'radial')
  
  # Predicting the Test set results
  y_pred <- predict(classifier, newdata = test_set[, -5])
  
  # Confusion Matrix
  cm <- table(test_set$Species, y_pred)
  print("Confusion Matrix:")
  print(cm)
  
  # Plot decision boundary (Using Sepal Length and Sepal Width)
  plot_svm <- ggplot(training_set, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point() +
    ggtitle("SVM Decision Boundary (Iris Dataset)")
  
  print(plot_svm)
  
  # User Input for Prediction
  svm_user_input <- function() {
    cat("Enter Sepal Length: ")
    sepal_length <- as.numeric(readline())
    cat("Enter Sepal Width: ")
    sepal_width <- as.numeric(readline())
    cat("Enter Petal Length: ")
    petal_length <- as.numeric(readline())
    cat("Enter Petal Width: ")
    petal_width <- as.numeric(readline())
    
    # Create new data frame with user inputs
    new_data <- data.frame(Sepal.Length = sepal_length, 
                           Sepal.Width = sepal_width,
                           Petal.Length = petal_length, 
                           Petal.Width = petal_width)
    
    # Apply same scaling as training data
    new_data <- scale(new_data, center = feature_means, scale = feature_sds)
    
    # Predict the class
    prediction <- predict(classifier, newdata = as.data.frame(new_data))
    cat(paste("Predicted Class:", prediction, "\n"))
  }
  
  svm_user_input()
}

# Function for Program 2 (Decision Tree using Titanic dataset)
run_decision_tree_program <- function() {
  # Load Titanic dataset
  dataset <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
  
  # Select relevant columns
  dataset <- dataset[, c("Survived", "Pclass", "Sex", "Age", "Fare")]
  
  # Handling missing values
  dataset <- na.omit(dataset)
  
  # Convert categorical variables to factors
  dataset$Sex <- as.factor(dataset$Sex)
  dataset$Survived <- as.factor(dataset$Survived)
  
  # Train a C5.0 decision tree model
  model <- C5.0(Survived ~ ., data = dataset)
  
  # Plot decision tree
  plot(model, main = "Decision Tree Visualization (Titanic Dataset)")
  
  # Function for user input
  get_user_input <- function() {
    cat("Enter Passenger Details: \n")
    pclass <- as.numeric(readline("Passenger Class (1/2/3): "))
    sex <- readline("Sex (male/female): ")
    age <- as.numeric(readline("Age: "))
    fare <- as.numeric(readline("Fare: "))
    
    return(data.frame(Pclass = pclass,
                      Sex = factor(sex, levels = c("male", "female")),
                      Age = age,
                      Fare = fare))
  }
  
  test_data <- get_user_input()
  
  # Predict survival
  prediction <- predict(model, test_data)
  cat(paste("Predicted Survival:", prediction, "\n"))
}

# Main Menu
main_menu <- function() {
  while (TRUE) {
    cat("1. SVM Analysis\n")
    cat("2. Decision Tree\n")
    cat("3. Exit\n")
    choice <- as.numeric(readline(prompt = "Enter your choice: "))
    
    if (choice == 1) {
      run_svm_program()
    } else if (choice == 2) {
      run_decision_tree_program()
    } else if (choice == 3) {
      cat("Exiting...\n")
      break
    } else {
      cat("Invalid choice! Please enter a valid option.\n")
    }
  }
}

# Run the main menu
main_menu()
