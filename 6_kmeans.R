library(ggplot2)

assign_clusters <- function(data, centroids) {
  clusters <- numeric(nrow(data))
  for (i in 1:nrow(data)) {
    distances <- apply(centroids, 1, function(centroid) sum((data[i, ] - centroid)^2))
    clusters[i] <- which.min(distances)
  }
  return(clusters)
}

update_centroids <- function(data, clusters, k) {
  new_centroids <- matrix(0, nrow = k, ncol = ncol(data))
  for (i in 1:k) {
    new_centroids[i, ] <- colMeans(data[clusters == i, ], na.rm = TRUE)
  }
  return(new_centroids)
}

k_means <- function(data, k, max_iter = 100) {
  set.seed(123)
  initial_centroids <- data[sample(nrow(data), k), ]
  centroids <- initial_centroids
  converged <- FALSE
  iter <- 0
  
  while (!converged && iter < max_iter) {
    iter <- iter + 1
    clusters <- assign_clusters(data, centroids)
    
    cat("Iteration:", iter, "\n")
    
    for (i in 1:k) {
      cat(paste("Cluster", i, ":\n"))
      cluster_points <- data[clusters == i, ]
      print(cluster_points)
      cat("\n")
    }
    
    new_centroids <- update_centroids(data, clusters, k)
    
    cat("New Centroids:\n")
    print(new_centroids)
    
    if (all(new_centroids == centroids)) {
      converged <- TRUE
    }
    
    centroids <- new_centroids
  }
  
  return(list(clusters = clusters, centroids = centroids))
}

data <- read.csv("/home/ds-ds-26/aslam/kmeans.csv")

k <- 2
result <- k_means(data, k)

cat("Final Cluster Assignments:\n")
print(result$clusters)

cat("Final Centroids:\n")
print(result$centroids)

plot(data, col = result$clusters, pch = 19, main = "K-means Clustering")
points(result$centroids, col = 1:k, pch = 8, cex = 2, lwd = 2) 

data$Cluster <- as.factor(result$clusters)
ggplot(data, aes(x = Feature1, y = Feature2, color = Cluster)) +
  geom_point(size = 3) +
  geom_point(data = as.data.frame(result$centroids), aes(x = V1, y = V2), color = "black", size = 5, shape = 8) +
  labs(title = "K-means Clustering Results with ggplot2", x = "Feature 1", y = "Feature 2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green", "blue"))
