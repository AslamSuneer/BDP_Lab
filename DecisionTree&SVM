# Load required libraries
library(rpart)
library(e1071)  # Corrected library for SVM
library(caTools)

# Load dataset
data(iris)

# Set seed for reproducibility
set.seed(123)

# Split the dataset (70% training, 30% testing)
split <- sample.split(iris$Species, SplitRatio = 0.7)
train_set <- subset(iris, split == TRUE)
test_set <- subset(iris, split == FALSE)

# Train Decision Tree model
tree_model <- rpart(Species ~ ., data = train_set, method = "class")
print(tree_model)

# Plot the Decision Tree
plot(tree_model, uniform = TRUE, main = "Decision Tree for Iris Dataset", branch = 0.5, compress = TRUE)
text(tree_model, use.n = TRUE, cex = 0.5)

# Train SVM model
svm_model <- svm(Species ~ ., data = train_set, kernel = "radial")
print(svm_model)

# Make predictions using Decision Tree
tree_pred <- predict(tree_model, newdata = test_set, type = "class")
print("Decision Tree Prediction Testing Set: ")
print(tree_pred)

# Make predictions using SVM
svm_pred <- predict(svm_model, newdata = test_set)
print("SVM Prediction Testing Set: ")
print(svm_pred)

# Calculate accuracy for Decision Tree
tree_accuracy <- sum(tree_pred == test_set$Species) / nrow(test_set)
print(paste("Decision Tree Model Accuracy: ", round(tree_accuracy * 100, 2), "%"))

# Calculate accuracy for SVM
svm_accuracy <- sum(svm_pred == test_set$Species) / nrow(test_set)
print(paste("SVM Model Accuracy: ", round(svm_accuracy * 100, 2), "%"))




