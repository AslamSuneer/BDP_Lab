# Load required libraries
library(e1071)      # For SVM
library(tm)         # For text mining
library(stringr)    # For string operations

# 1. SVM (Simple Example using iris dataset)
data(iris)
svm_model <- svm(Species ~ ., data=iris)
print("SVM model summary:")
print(summary(svm_model))

# 2. List of sentences
sentences <- list(
  "R is a programming language",
  "Machine learning is fun",
  "SVM is a powerful algorithm",
  "I love programming in R",
  "Data science is interesting"
)

# a) Convert sentences to word vectors
word_vectors <- strsplit(tolower(unlist(sentences)), "\\s+")
print("Word vectors:")
print(word_vectors)

# b) Count unique words in the list
all_words <- unlist(word_vectors)
unique_words <- unique(all_words)
cat("Number of unique words:", length(unique_words), "\n")
print("Unique words:")
print(unique_words)

# c) Change order of words in each sentence
shuffled_sentences <- lapply(word_vectors, function(words) paste(sample(words), collapse = " "))
print("Shuffled sentences:")
print(shuffled_sentences)

# d) Identify sentences that contain a specific word (e.g., "is")
target_word <- "is"
contains_word <- sapply(sentences, function(s) grepl(paste0("\\b", target_word, "\\b"), s, ignore.case = TRUE))
matched_sentences <- sentences[contains_word]
print(paste("Sentences containing the word '", target_word, "':", sep=""))
print(matched_sentences)

# e) Sentence with max number of unique words
unique_word_count <- sapply(word_vectors, function(words) length(unique(words)))
max_index <- which.max(unique_word_count)
cat("Sentence with max unique words:\n", sentences[[max_index]], "\n")

# f) SVM again — Classify made-up data using text length and unique words
text_features <- data.frame(
  length = sapply(sentences, nchar),
  unique_words = unique_word_count,
  label = c("tech", "ml", "ml", "tech", "ml")  # Dummy labels
)

# Train SVM on text features
svm_text_model <- svm(label ~ ., data=text_features)
cat("SVM trained on text features:\n")
print(predict(svm_text_model, text_features))
