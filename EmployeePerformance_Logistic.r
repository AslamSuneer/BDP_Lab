# ----------------------
# Part (a) - Data Handling
# ----------------------

# Create the data frame
employee_data <- data.frame(
  Emp_ID = paste0("E", 1001:1020),
  Name = paste0("Emp_", 1:20),
  Department = sample(c("Finance", "HR", "IT", "Marketing"), 20, replace = TRUE),
  Date_of_Joining = sample(seq(as.Date("2010/01/01"), as.Date("2022/12/31"), by="day"), 20),
  Monthly_Salary = sample(30000:100000, 20),
  Performance_Score = sample(c(50:100, NA), 20, replace = TRUE)
)

# Save to CSV
write.csv(employee_data, "employee_performance.csv", row.names = FALSE)

# Load the data
emp <- read.csv("employee_performance.csv", stringsAsFactors = FALSE)
emp$Date_of_Joining <- as.Date(emp$Date_of_Joining)

# 1. Employees in Finance who joined before 2018
cat("Employees in Finance before 2018:\n")
print(emp$Name[emp$Department == "Finance" & format(emp$Date_of_Joining, "%Y") < 2018])

# 2. IDs with missing Performance Score
cat("Employee IDs with missing Performance Score:\n")
print(emp$Emp_ID[is.na(emp$Performance_Score)])

# 3. Add Years_of_Experience column
emp$Years_of_Experience <- as.numeric(difftime(Sys.Date(), emp$Date_of_Joining, units = "weeks")) / 52.25

# 4. Add Performance_rating
emp$Performance_rating <- cut(
  emp$Performance_Score,
  breaks = c(-Inf, 60, 75, 89, 100),
  labels = c("Poor", "Average", "Good", "Outstanding"),
  right = TRUE
)

# 5. Replace missing Performance_Score by department-wise median
emp$Performance_Score <- ave(emp$Performance_Score, emp$Department, 
                             FUN = function(x) { replace(x, is.na(x), median(x, na.rm = TRUE)) })

# Save updated file
write.csv(emp, "employee_performance.csv", row.names = FALSE)


# ----------------------
# Part (b) - Logistic Regression
# ----------------------

# Load updated file
emp <- read.csv("employee_performance.csv", stringsAsFactors = FALSE)

# 1. Add High_Performer column
emp$High_Performer <- ifelse(emp$Performance_Score >= 75, 1, 0)

# 2. Logistic Regression Model
model <- glm(High_Performer ~ Monthly_Salary + Performance_Score + Years_of_Experience,
             data = emp, family = binomial)

# Print summary and intercept
cat("Logistic Regression Summary:\n")
print(summary(model))
cat("Model Intercept:\n")
print(coef(model)[1])

# 3. Prediction
emp$Predicted_Prob <- predict(model, type = "response")
emp$Predicted_High_Performer <- ifelse(emp$Predicted_Prob >= 0.5, 1, 0)

# 4. Visualization
library(ggplot2)
ggplot(emp, aes(x = Performance_Score, y = Predicted_Prob, color = factor(High_Performer))) +
  geom_point(size = 3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Predicted Probability of Being a High Performer",
       x = "Performance Score", y = "Predicted Probability",
       color = "Actual High Performer") +
  theme_minimal()
