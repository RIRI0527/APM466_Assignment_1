# Load necessary libraries
library(readr)
library(here)

# Load the dataset (update the file path if needed)
data <- read_csv(here::here("10_selected_bonds.csv"))

# Compute covariance matrices
cov_matrix_yield <- cov(log_returns_yield, use = "pairwise.complete.obs")
cov_matrix_forward <- cov(log_returns_forward, use = "pairwise.complete.obs")

# Compute eigenvalues and eigenvectors for yield covariance matrix
eigen_yield <- eigen(cov_matrix_yield)

# Compute eigenvalues and eigenvectors for forward rate covariance matrix
eigen_forward <- eigen(cov_matrix_forward)

# Explanation of Eigenvalues and Eigenvectors:
# The eigenvalues represent the variance explained by each principal component.
# The largest eigenvalue and its corresponding eigenvector indicate the direction of 
# maximum variance in the dataset. This implies the most significant factor affecting 
# variations in yield and forward rate log-returns.

# Print eigenvalues and eigenvectors
print("Eigenvalues for Yield Covariance Matrix:")
print(eigen_yield$values)
print("Eigenvectors for Yield Covariance Matrix:")
print(eigen_yield$vectors)

print("Eigenvalues for Forward Rate Covariance Matrix:")
print(eigen_forward$values)
print("Eigenvectors for Forward Rate Covariance Matrix:")
print(eigen_forward$vectors)

