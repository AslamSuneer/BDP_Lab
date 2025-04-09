# Load necessary library
library(stats)

# 1. K-means example (simple with dummy data)
set.seed(123)
data <- matrix(rnorm(50), ncol=2)
colnames(data) <- c("X", "Y")
kmeans_result <- kmeans(data, centers=3)
print("K-means clustering result:")
print(kmeans_result)

# 2. Create a list of employee records
employees <- list(
  list(id=1, name="Anna", dept="Engineering", salary=75000),
  list(id=2, name="Bob", dept="HR", salary=50000),
  list(id=3, name="Eve", dept="Engineering", salary=90000),
  list(id=4, name="Otto", dept="Engineering", salary=80000),
  list(id=5, name="Ada", dept="Sales", salary=60000),
  list(id=6, name="Nitin", dept="Engineering", salary=85000)
)

# i) Convert list to dataframe
df <- do.call(rbind, lapply(employees, as.data.frame))
df <- as.data.frame(df, stringsAsFactors = FALSE)
df$salary <- as.numeric(df$salary)
print("Employee DataFrame:")
print(df)

# ii) Sort Engineering employees by descending salary
eng_sorted <- df[df$dept == "Engineering", ]
eng_sorted <- eng_sorted[order(-eng_sorted$salary), ]
print("Engineering Employees Sorted by Salary (Descending):")
print(eng_sorted)

# iii) Calculate average salary in Engineering
avg_salary <- mean(eng_sorted$salary)
cat("Average salary in Engineering:", avg_salary, "\n")

# iv) Check for palindrome in names
is_palindrome <- function(s) {
  s <- tolower(s)
  return(s == paste(rev(strsplit(s, NULL)[[1]]), collapse=""))
}
df$palindrome <- sapply(df$name, is_palindrome)
print("Employees with Palindrome Name Check:")
print(df[, c("name", "palindrome")])

# v) K-means on salary (simple example: clustering employees based on salary)
salary_data <- data.frame(salary = df$salary)
kmeans_emp <- kmeans(salary_data, centers=2)
df$salary_cluster <- kmeans_emp$cluster
print("Employees with K-means Salary Clusters:")
print(df[, c("name", "salary", "salary_cluster")])

# Plot salary clustering
library(ggplot2)

# Add row index for plotting on x-axis
df$index <- 1:nrow(df)

# Convert cluster to factor for color coding
df$salary_cluster <- as.factor(df$salary_cluster)

# Plotting salary clusters
ggplot(df, aes(x = index, y = salary, color = salary_cluster, label = name)) +
  geom_point(size = 4) +
  geom_text(vjust = -0.8, size = 3.5) +
  labs(title = "K-Means Clustering of Employees by Salary",
       x = "Employee Index",
       y = "Salary",
       color = "Cluster") +
  theme_minimal()
