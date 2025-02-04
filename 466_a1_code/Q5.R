# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(here)

# Load the dataset (update the file path if needed)
data <- read_csv(here::here("10_selected_bonds.csv"))

# Select only the columns containing yield data
date_columns <- names(data)[7:length(names(data))]  # Assuming dates start at column 7
yield_data <- data %>% select(all_of(date_columns))

# Convert data to numeric
yield_data <- yield_data %>% mutate_all(as.numeric)

# Compute log-returns for yield
log_returns_yield <- yield_data %>% mutate(across(everything(), 
                                                  ~ log(.x / lag(.x)))) %>% na.omit()

# Compute covariance matrix for yields
cov_matrix_yield <- cov(log_returns_yield, use = "pairwise.complete.obs")

# Display covariance matrix for YTM
print("Covariance Matrix for Log-returns of YTM:")
print(cov_matrix_yield)

# Extract forward rates (assuming first few rows represent different maturities)
forward_rates <- list()

for (i in 1:4) {  # Compute 1yr-1yr to 1yr-4yr forward rates
  forward_rates[[i]] <- ((yield_data[1:(nrow(yield_data)-i), ] / 
                            yield_data[(i+1):nrow(yield_data), ])^(1/i)) - 1
}

# Convert list to dataframe
forward_rates_df <- do.call(rbind, forward_rates)

# Compute log-returns for forward rates
log_returns_forward <- forward_rates_df %>% mutate(across(everything(), 
                                                          ~ log(.x / lag(.x)))) %>% na.omit()

# Compute covariance matrix for forward rates
cov_matrix_forward <- cov(log_returns_forward, use = "pairwise.complete.obs")

# Display covariance matrix for Forward Rates
print("Covariance Matrix for Log-returns of Forward Rates:")
print(cov_matrix_forward)
