# Load necessary library
library(readr)

# Function to extract a row or column from a CSV and save to a file
extract_and_save <- function(csv_file, index, type = "row", output_file) {
  # Read the CSV file
  data <- read_csv(csv_file)
  
  # Extract row or column
  if (type == "row") {
    extracted_data <- data[index, , drop = FALSE]  # Extract row
  } else if (type == "column") {
    extracted_data <- data[, index, drop = FALSE]  # Extract column
  } else {
    stop("Invalid type. Use 'row' or 'column'.")
  }
  
  # Write extracted data to a new file
  write_csv(extracted_data, output_file)
  
  cat("Extracted data saved to", output_file, "\n")
}

# Example usage
# Extract 2nd row from 'data.csv' and save to 'row_output.csv'
extract_and_save("data.csv", 2, "row", "row_output.csv")

# Extract 3rd column from 'data.csv' and save to 'column_output.csv'
extract_and_save("data.csv", 3, "column", "column_output.csv")
