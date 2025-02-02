display_menu <- function() {
  cat("Menu:\n")
  cat("1. Merge\n")
  cat("2. Palindrome\n")
  cat("3. Max and Min\n")
  cat("4. Factorial of Fibonacci\n")
  cat("5. Exit\n")
}

merge1 <- function() {
  
  df1 <- data.frame(ID = c(1, 2, 4, 5), name = c("sreerag", "midlaj", "aslam","somanath"))
  
  df2 <- data.frame(ID = c(2, 4, 6, 8), age = c(19, 20, 21, 22))
  
  
  print("DataFrame 1:")
  
  print(df1)
  
  print("DataFrame 2:")
  
  print(df2)
  
  merged <- base::merge(df1, df2, by = "ID", all = TRUE) 
  
  print("Merged DataFrame:")
  
  print(merged)
}


palindrome <- function() {
  df <- data.frame(
    ID = 1:6,
    Name = c("madam", "racecar", "hello", "level", "world", "noon"),
    Age = c(25, 30, 35, 40, 45, 50)
  )
  
  print("Original DataFrame:")
  print(df)
  
  is_palindrome <- function(name) {
    cleaned_name <- tolower(name)  
    reversed_name <- rev(strsplit(cleaned_name, NULL)[[1]]) 
    return(cleaned_name == paste(reversed_name, collapse = ""))  
  }
  
  
  palindromic_names <- df[unlist(lapply(df$Name, is_palindrome)), ]
  
  palindromic_names <- palindromic_names[, c("ID", "Name")]
  
  print("Palindromic Names:")
  print(palindromic_names)
  
  write.csv(palindromic_names, "/home/ds-ds-26/aslam/palindromic_names.csv", row.names = FALSE)
  
  cat("Palindromic names have been written to 'palindromic_names.csv'.\n")
}

max_and_min <- function() {
  df <- data.frame(
    A = c(1, 4, 3),
    B = c(5, 2, 8),
    C = c(3, 6, 1)
  )
  
  print("Original DataFrame:")
  print(df)
  max_values_rows <- apply(df, 1, max)
  min_values_rows <- apply(df, 1, min)
  max_values_columns <- apply(df, 2, max)
  min_values_columns <- apply(df, 2, min)
  
  print("Maximum values for each row:")
  print(max_values_rows)
  
  print("Minimum values for each row:")
  print(min_values_rows)
  
  print("Maximum values for each column:")
  print(max_values_columns)
  
  print("Minimum values for each column:")
  print(min_values_columns)
}

is_fibonacci <- function(num) {
  a <- 0
  b <- 1
  while (a < num) {
    temp <- a
    a <- b
    b <- temp + b
  }
  return(a == num) 
}

factorial <- function(n) {
  if (n < 0) {
    return(NA)  
  } else if (n == 0 || n == 1) {
    return(1) 
  } else {
    result <- 1
    for (i in 2:n) {
      result <- result * i
    }
    return(result)
  }
}

check_fibonacci_and_factorial <- function(df) {
  for (num in df$numbers) { 
    if (is_fibonacci(num)) {
      cat("Factorial of", num, "is", factorial(num), "\n")
    }
  }
}

df <- data.frame(numbers = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 13, 21, 34, 55))


repeat {
  display_menu()
  choice <- as.integer(readline(prompt = "Choose option from 1 to 5: "))
  
  if (choice == 1) {
    merge1()
  } else if (choice == 2) {
    palindrome()
  } else if (choice == 3) {
    max_and_min()
  } else if (choice == 4) {
    check_fibonacci_and_factorial(df)
  } else if (choice == 5) {
    cat("Exiting program.....\n")
    break  
  } else {
    cat("Invalid choice! Please select a valid option.\n")
  }
}
