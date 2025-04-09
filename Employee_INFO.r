# Load necessary libraries
library(dplyr)
library(lubridate)

# Sample employee_info data frame
set.seed(123)  # For reproducibility
employee_info <- data.frame(
  Employee_ID = 1:10,
  Name = paste("Employee", 1:10),
  Department = sample(c("HR", "IT", "Finance", "Sales"), 10, replace = TRUE),
  Login_Time = as.POSIXct("2025-04-09 08:00:00") + runif(10, 0, 60*60),  # random login between 8:00 to 9:00
  Logout_Time = as.POSIXct("2025-04-09 17:00:00") + runif(10, -60*60, 60*60)  # random logout between 16:00 to 18:00
)

# Create attendance_log with punctuality score
attendance_log <- data.frame(
  Employee_ID = employee_info$Employee_ID,
  Login_Time = employee_info$Login_Time,
  Logout_Time = employee_info$Logout_Time,
  Punctuality_Score = sample(1:10, 10, replace = TRUE)
)

# i. Add column Working_Hours
attendance_log <- attendance_log %>%
  mutate(Working_Hours = as.numeric(difftime(Logout_Time, Login_Time, units = "hours")))

# ii. Find Employee with Working_Hours < 8
under_8_hours <- attendance_log %>%
  filter(Working_Hours < 8)

print("Employees with Working Hours < 8:")
print(under_8_hours)

# iii. Add column Shift_Type
attendance_log <- attendance_log %>%
  mutate(Shift_Type = case_when(
    Working_Hours >= 9 ~ "Full",
    Working_Hours >= 7 & Working_Hours < 9 ~ "Half",
    Working_Hours < 7 ~ "Short"
  ))

# iv. Contingency table of department vs shift type
attendance_df <- left_join(employee_info, attendance_log, by = "Employee_ID")
dept_shift_table <- table(attendance_df$Department, attendance_df$Shift_Type)
print("Contingency table (Department vs Shift_Type):")
print(dept_shift_table)

# v. Employees with punctuality score 10 and working a full shift
perfect_full_shift <- attendance_df %>%
  filter(Punctuality_Score == 10 & Shift_Type == "Full")

print("Employees with punctuality score 10 and Full shift:")
print(perfect_full_shift)

# vi. Final attendance_df already created above by joining
print("Final attendance_df:")
print(attendance_df)
