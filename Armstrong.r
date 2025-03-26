# Function to check if a number is an Armstrong number
is_armstrong <- function(num) {
  digits <- as.integer(strsplit(as.character(num), "")[[1]])  # Split number into digits
  num_digits <- length(digits)  # Count the number of digits
  sum_digits <- sum(digits^num_digits)  # Compute the sum of digits raised to power of num_digits
  
  return(sum_digits == num)
}

# Test the function
num <- as.integer(readline(prompt="Enter a number: "))  
if (is_armstrong(num)) {
  print(paste(num, "is an Armstrong number"))
} else {
  print(paste(num, "is not an Armstrong number"))
}

