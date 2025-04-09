# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# 1. Create sales_data DataFrame
sales_data <- data.frame(
  Month = month.name,
  Product_A = sample(1000:10000, 12, replace = TRUE),
  Product_B = sample(1000:10000, 12, replace = TRUE),
  Product_C = sample(1000:10000, 12, replace = TRUE)
)

# 2. Total annual sale for each product
total_annual_sales <- colSums(sales_data[, 2:4])
print("Total Annual Sales for each product:")
print(total_annual_sales)

# 3. Month with highest sale for each product
max_sales_month <- sapply(sales_data[, 2:4], function(x) sales_data$Month[which.max(x)])
print("Month with highest sales for each product:")
print(max_sales_month)

# 4. Add Total_sales column
sales_data$Total_sales <- rowSums(sales_data[, 2:4])
print("Sales data with Total_sales:")
print(sales_data)

# 5. Filter: Total_sales > 20000 and Product_B > Product_A
filtered_sales <- sales_data %>%
  filter(Total_sales > 20000 & Product_B > Product_A)
print("Filtered data (Total_sales > 20000 and Product_B > Product_A):")
print(filtered_sales)

# 6. Correlation matrix
cor_matrix <- cor(sales_data[, 2:5])
print("Correlation Matrix:")
print(cor_matrix)

# 7. Covariance matrix
cov_matrix <- cov(sales_data[, 2:5])
print("Covariance Matrix:")
print(cov_matrix)

# 8. Interquartile Range (IQR)
iqr_values <- apply(sales_data[, 2:5], 2, IQR)
print("IQR for each variable:")
print(iqr_values)

# 9. Linear Regression: Predict Total_sales
model <- lm(Total_sales ~ Product_A + Product_B + Product_C, data = sales_data)

# 9.1. Summary and Intercept
summary_model <- summary(model)
print("Linear Regression Summary:")
print(summary_model)

intercept <- coef(model)[1]
print(paste("Intercept:", intercept))

# 9.2. Prediction on new data
new_data <- data.frame(
  Product_A = 7000,
  Product_B = 8000,
  Product_C = 6500
)

predicted_total <- predict(model, newdata = new_data)
print("Predicted Total Sales for new data:")
print(predicted_total)

# 9.3. Plot regression line for Product_A vs Total_sales
plot(sales_data$Product_A, sales_data$Total_sales,
     main = "Product_A vs Total_sales with Regression Line",
     xlab = "Product A Sales", ylab = "Total Sales",
     pch = 16, col = "blue")

model_simple <- lm(Total_sales ~ Product_A, data = sales_data)
abline(model_simple, col = "red", lwd = 2)
