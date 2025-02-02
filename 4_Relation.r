data <- read.csv("/home/ds-ds-26/aslam/sts.csv")

data$ord_data1 <- factor(data$ord_data1, levels = c("Low", "Medium", "High"), ordered = TRUE)
data$ord_data2 <- factor(data$ord_data2, levels = c("Low", "Medium", "High"), ordered = TRUE)

data$nom_data1 <- factor(data$nom_data1)
data$nom_data2 <- factor(data$nom_data2)

num_variance1 <- var(data$num_data1)
num_variance2 <- var(data$num_data2)
num_covariance <- cov(data$num_data1, data$num_data2)
num_correlation <- cor(data$num_data1, data$num_data2)

cat("\nNumeric vs Numeric Statistics:\n")
cat("Variance of num_data1:", num_variance1, "\n")
cat("Variance of num_data2:", num_variance2, "\n")
cat("Covariance between num_data1 and num_data2:", num_covariance, "\n")
cat("Correlation coefficient between num_data1 and num_data2:", num_correlation, "\n")

binary_variance1 <- var(data$bin_data1)
binary_variance2 <- var(data$bin_data2)
binary_covariance <- cov(data$bin_data1, data$bin_data2)
binary_correlation <- cor(data$bin_data1, data$bin_data2)

cat("\nBinary vs Binary Statistics:\n")
cat("Variance of bin_data1:", binary_variance1, "\n")
cat("Variance of bin_data2:", binary_variance2, "\n")
cat("Covariance between bin_data1 and bin_data2:", binary_covariance, "\n")
cat("Correlation coefficient between bin_data1 and bin_data2:", binary_correlation, "\n")

nominal_chisq_test <- chisq.test(table(data$nom_data1, data$nom_data2))
cat("\nChi-squared Test for Nominal vs Nominal:\n")
cat("Chi-squared Statistic:", nominal_chisq_test$statistic, "\n")
cat("p-value:", nominal_chisq_test$p.value, "\n")

ordinal_chisq_test <- chisq.test(table(data$ord_data1, data$ord_data2))
cat("\nChi-squared Test for Ordinal vs Ordinal:\n")
cat("Chi-squared Statistic:", ordinal_chisq_test$statistic, "\n")
cat("p-value:", ordinal_chisq_test$p.value, "\n")

par(mfrow = c(2, 2))

nominal_table <- table(data$nom_data1, data$nom_data2)
barplot(nominal_table, beside = TRUE, col = c("lightblue", "lightgreen", "lightcoral"),
        legend = TRUE, args.legend = list(title = "Nominal Data", x = "topright"),
        main = "Nominal Data vs Nominal Data", xlab = "Nominal Data Categories", ylab = "Frequency")

ordinal_table <- table(data$ord_data1, data$ord_data2)
barplot(ordinal_table, beside = TRUE, col = c("lightblue", "lightgreen", "lightcoral"),
        legend = TRUE, args.legend = list(title = "Ordinal Data", x = "topright"),
        main = "Ordinal Data vs Ordinal Data", xlab = "Ordinal Data Categories", ylab = "Frequency")

plot(data$num_data1, data$num_data2, 
     main = "Numeric Data vs Numeric Data", 
     xlab = "Numeric Data 1", ylab = "Numeric Data 2", 
     col = "blue", pch = 16)

plot(data$bin_data1, data$bin_data2, 
     main = "Binary Data vs Binary Data", 
     xlab = "Binary Data 1", ylab = "Binary Data 2", 
     col = "red", pch = 16)

par(mfrow = c(1, 1))
