# Load required libraries
library(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)

# 1. Voter-Booth Program
run_voter_booth_program <- function() {
  cat("\n🗳️ Running Voter-Booth Program...\n")
  
  # Create Booth table
  booth <- data.frame(
    Boothid = c(101, 102, 103),
    Location = c("Central", "North", "East"),
    BIncharge = c("Raj", "Neha", "Ali")
  )

  # Create Voter table
  voter <- data.frame(
    VoterId = 1:10,
    Votername = c("John", "Asha", "Kumar", "Neel", "Rita", "Farah", "Anil", "Simran", "Zaid", "Maya"),
    Gender = c("M", "F", "M", "M", "F", "F", "M", "F", "M", "F"),
    Boothid = c(101, 102, 101, 103, 101, 102, 101, 103, 103, 102),
    Checkvote = c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1)
  )

  cat("✔ Tables created!\n")

  # a(i) Count of voters in each Booth
  cat("\n(a-i) Count of voters in each Booth:\n")
  voter %>%
    group_by(Boothid) %>%
    summarise(Count = n()) %>%
    print()

  # a(ii) Count of Male voters who voted
  cat("\n(a-ii) Count of Male voters who voted:\n")
  voter %>%
    filter(Gender == "M", Checkvote == 1) %>%
    summarise(Count = n()) %>%
    print()

  # b) Overall count of voters who voted
  cat("\n(b) Total voters who voted:\n")
  voter %>%
    filter(Checkvote == 1) %>%
    summarise(Count = n()) %>%
    print()

  # c) Boothid, Location, and count of voters voted
  cat("\n(c) Booth-wise vote count with location:\n")
  voter %>%
    filter(Checkvote == 1) %>%
    group_by(Boothid) %>%
    summarise(VotedCount = n()) %>%
    left_join(booth, by = "Boothid") %>%
    select(Boothid, Location, VotedCount) %>%
    print()

  # d) Function to calculate polling percentage for a booth
  get_poll_percentage <- function(input_boothid) {
    total <- nrow(filter(voter, Boothid == input_boothid))
    voted <- nrow(filter(voter, Boothid == input_boothid, Checkvote == 1))
    percentage <- (voted / total) * 100
    return(percentage)
  }

  booth_input <- 101
  cat(paste0("\n(d) Polling percentage in booth ", booth_input, ": ",
             round(get_poll_percentage(booth_input), 2), "%\n"))
}

# 2. Decision Tree
run_decision_tree_model <- function() {
  cat("\n Running Decision Tree Model...\n")
  data(iris)
  model <- rpart(Species ~ ., data = iris, method = "class")
  rpart.plot(model, main = "Decision Tree for Iris Dataset")
  prediction <- predict(model, iris[1:5, ], type = "class")
  print(prediction)
}

# 3. SVM
run_svm_model <- function() {
  cat("\n Running SVM Model...\n")
  data(iris)
  model <- svm(Species ~ ., data = iris)
  prediction <- predict(model, iris[1:5, ])
  print(prediction)
}

# 4. Logistic Regression
run_logistic_regression <- function() {
  cat("\nRunning Logistic Regression Model...\n")
  data(mtcars)
  mtcars$am <- as.factor(mtcars$am)
  model <- glm(am ~ mpg + hp, data = mtcars, family = "binomial")
  probs <- predict(model, mtcars, type = "response")
  predictions <- ifelse(probs > 0.5, 1, 0)
  print(head(predictions))
}

# 5. K-Means
run_kmeans_model <- function() {
  cat("\n Running K-Means Clustering...\n")
  data(iris)
  result <- kmeans(iris[, -5], centers = 3)
  cat("Cluster assignment for first 10 records:\n")
  print(result$cluster[1:10])
}

# MAIN MENU
main_menu <- function() {
  repeat {
    cat("\nMain Menu - Select a Program to Run\n")
    cat("1. Voter-Booth Data Analysis\n")
    cat("2. Decision Tree Model\n")
    cat("3. SVM Model\n")
    cat("4. Logistic Regression Model\n")
    cat("5. K-Means Clustering\n")
    cat("6. Exit\n")
    
    choice <- as.integer(readline(prompt = "Enter your choice (1-6): "))
    
    switch(choice,
           run_voter_booth_program(),
           run_decision_tree_model(),
           run_svm_model(),
           run_logistic_regression(),
           run_kmeans_model(),
           {
             cat(" Exiting\n")
             break
           },
           cat("Invalid choice. Please try again.\n")
    )
  }
}

# Auto-run menu when script is sourced
main_menu()
