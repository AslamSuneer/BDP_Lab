# Menu-Driven Program for Linear Regression and Multiple Linear Regression


# Function for Program 1: Simple Linear Regression
program1 <- function() {
  # Read data from CSV file
  a <- read.csv("/home/ds-ds-26/aslam/linear.csv")
  
  # Normalize the data (scale to 0-1)
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  a$years_normalized <- normalize(a$years)
  a$salary_normalized <- normalize(a$salary)
  
  # Number of observations
  n <- length(a$years_normalized)
  
  # Sums needed for calculations
  xsum <- sum(a$years_normalized)
  ysum <- sum(a$salary_normalized)
  xsquare <- sum(a$years_normalized^2)
  xy <- sum(a$years_normalized * a$salary_normalized)
  
  # Calculate slope (m) and intercept (b)
  m <- (n * xy - (xsum * ysum)) / (n * xsquare - (xsum)^2)
  b <- (ysum - (m * xsum)) / n
  
  # Print slope and intercept
  print(paste("Slope (m):", m))
  print(paste("Intercept (b):", b))
  
  # Plot the data and the regression line
  plot(a$years_normalized, a$salary_normalized, main = "Linear Regression: Salary vs Years (Normalized)",
       xlab = "Years of Experience (Normalized)", ylab = "Salary (Normalized)",
       pch = 19, col = "blue")
  abline(a = b, b = m, col = "red", lwd = 2)
  
  # Calculate predictions
  predictions <- m * a$years_normalized + b
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean((a$salary_normalized - predictions)^2)
  
  # Print MSE
  print(paste("Mean Squared Error (MSE):", mse))
  
  # Prompt user for test data
  test_years <- as.numeric(readline(prompt = "Enter years of experience for prediction: "))
  
  # Check if test_years is within the range of the dataset
  
  # Normalize test_years
  test_years_normalized <- (test_years - min(a$years)) / (max(a$years) - min(a$years))
  
  # Predict salary for the test data
  predicted_salary_normalized <- m * test_years_normalized + b
  
  # Denormalize the predicted salary
  predicted_salary <- predicted_salary_normalized * (max(a$salary) - min(a$salary)) + min(a$salary)
  
  # Print the predicted salary
  print(paste("Predicted Salary for", test_years, "years of experience:", predicted_salary))
  
  # Plot the predicted point in green
  
  
  # Ensure the plot window stays open
  invisible(readline(prompt = "Press [Enter] to continue..."))
}
# Function for Program 2: Multiple Linear Regression
program2 <- function() {
  # Function to get user input
  get_user_input <- function() {
    hours_studied <- as.numeric(readline(prompt = "Enter Hours Studied: "))
    previous_scores <- as.numeric(readline(prompt = "Enter Previous Scores: "))
    
    # Use readline for extracurricular activities
    extracurricular_activities <- readline(prompt = "Extracurricular Activities (Yes/No): ")
    extracurricular_activities <- ifelse(tolower(extracurricular_activities) == "yes", "Yes", "No")
    
    sleep_hours <- as.numeric(readline(prompt = "Enter Sleep Hours: "))
    sample_question_papers_practiced <- as.numeric(readline(prompt = "Enter Sample Question Papers Practiced: "))
    
    return(data.frame(
      Hours.Studied = hours_studied,
      Previous.Scores = previous_scores,
      Extracurricular.Activities = extracurricular_activities,
      Sleep.Hours = sleep_hours,
      Sample.Question.Papers.Practiced = sample_question_papers_practiced
    ))
  }
  
  # Get user input
  sample_data <- get_user_input()
  
  # Load the packages
  library(ggplot2)
  library(lattice)
  
  # Read the dataset from CSV
  data <- read.csv("/home/ds-ds-26/aslam/student.csv")
  
  # Convert 'Extracurricular Activities' to numeric (Yes = 1, No = 0)
  data$Extracurricular.Activities <- ifelse(data$Extracurricular.Activities == "Yes", 1, 0)
  
  # View the updated dataset
  head(data)
  
  # Fit the multiple linear regression model
  model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = data)
  
  # Summary of the model
  summary(model)
  
  # Add predicted values to the data frame
  data$predicted <- predict(model, data)
  
  # Plot using ggplot2
  print(
    ggplot(data, aes(x = Performance.Index, y = predicted)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(title = "Actual vs Predicted Values",
           x = "Actual Performance Index",
           y = "Predicted Performance Index") +
      theme_minimal()
  )
  
  # Plot using lattice
  print(
    xyplot(predicted ~ Performance.Index, data = data,
           type = c("p", "r"),
           col.line = "red",
           main = "Actual vs Predicted Values",
           xlab = "Actual Performance Index",
           ylab = "Predicted Performance Index")
  )
  
  # Add residuals to the data frame
  data$residuals <- residuals(model)
  
  # Residual plot using ggplot2
  print(
    ggplot(data, aes(x = predicted, y = residuals)) +
      geom_point(color = "blue") +
      geom_hline(yintercept = 0, color = "red") +
      labs(title = "Residuals vs Predicted Values",
           x = "Predicted Performance Index",
           y = "Residuals") +
      theme_minimal()
  )
  
  # Function to predict performance index for new data
  predict_performance <- function(new_data) {
    # Convert 'Extracurricular Activities' to numeric
    new_data$Extracurricular.Activities <- ifelse(new_data$Extracurricular.Activities == "Yes", 1, 0)
    
    # Predict using the model
    new_data$predicted <- predict(model, new_data)
    
    return(new_data$predicted)
  }
  
  # Predict the performance index for the sample data
  predicted_value <- predict_performance(sample_data)
  print(paste("Predicted Performance Index for sample data:", predicted_value))
  
  # Calculate MSE and AMSE
  mse <- mean(data$residuals^2)
  amse <- mse / var(data$Performance.Index)
  
  # Print MSE and AMSE
  print(paste("Mean Squared Error (MSE):", mse))
  print(paste("Adjusted Mean Squared Error (AMSE):", amse))
  
  # Ensure the plot window stays open
  invisible(readline(prompt = "Press [Enter] to continue"))
}

# Main Menu
main_menu <- function() {
  cat("Menu:\n")
  cat("1. Simple Linear Regression (Salary Prediction)\n")
  cat("2. Multiple Linear Regression (Performance Index Prediction)\n")
  cat("3. Exit\n")
  
  choice <- as.numeric(readline(prompt = "Enter your choice: "))
  
  if (choice == 1) {
    program1()
  } else if (choice == 2) {
    program2()
  } else if (choice == 3) {
    cat("Exit\n")
    return()
  } else {
    cat("Invalid choice\n")
  }
  
  # Loop to show the menu again
  main_menu()
}

# Run the main menu
main_menu()