# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(pracma)
library(here)

# Load data
file_path <- "10_selected_bonds.csv"
df_bonds <- read_csv(here::here("10_selected_bonds.csv"))

# Data preprocessing
# Convert Coupon rate to numeric
df_bonds$Coupon <- as.numeric(gsub("%", "", df_bonds$Coupon)) / 100

# Convert Maturity Date to Date format
df_bonds$`Maturity date` <- as.Date(df_bonds$`Maturity date`, format="%m/%d/%Y")

# Calculate Years to Maturity (based on 2024-01-06)
valuation_date <- as.Date("2024-01-06")
df_bonds$`Years to Maturity` <- as.numeric(difftime(df_bonds$`Maturity date`, valuation_date, units="days")) / 365

# Define function to bootstrap spot rates
bootstrap_spot_curve <- function(prices, coupons, face_value, maturities) {
  spot_rates <- numeric(length(maturities))
  for (i in seq_along(maturities)) {
    if (i == 1) {
      spot_rates[i] <- (coupons[i] + face_value) / prices[i] - 1
    } else {
      sum_present_values <- sum(coupons[1:(i-1)] / (1 + spot_rates[1:(i-1)])^maturities[1:(i-1)])
      spot_rates[i] <- ((coupons[i] + face_value) / (prices[i] - sum_present_values))^(1/maturities[i]) - 1
    }
  }
  return(spot_rates)
}

# Define function to compute forward rates from spot rates
compute_forward_rates <- function(spot_rates, maturities) {
  forward_rates <- numeric(length(maturities) - 1)
  forward_periods <- maturities[2:length(maturities)] - maturities[1]
  for (i in 2:length(maturities)) {
    t1 <- maturities[i - 1]
    t2 <- maturities[i]
    r1 <- spot_rates[i - 1]
    r2 <- spot_rates[i]
    forward_rates[i - 1] <- (((1 + r2)^t2) / ((1 + r1)^t1))^(1 / (t2 - t1)) - 1
  }
  return(data.frame(Forward_Period = forward_periods, Forward_Rate = forward_rates))
}

# Compute forward curve for each date
forward_results <- df_bonds %>%
  select(`Bond Name`, starts_with("2024/")) %>%
  pivot_longer(cols = -`Bond Name`, names_to = "Date", values_to = "Price") %>%
  left_join(select(df_bonds, `Bond Name`, Coupon, `Years to Maturity`), by = "Bond Name") %>%
  mutate(Date = as.Date(Date, format="%Y/%m/%d")) %>%
  group_by(Date) %>%
  summarise(SpotRates = list(bootstrap_spot_curve(Price, Coupon, 100, `Years to Maturity`)), .groups = 'drop') %>%
  mutate(ForwardData = lapply(SpotRates, compute_forward_rates, df_bonds$`Years to Maturity`)) %>%
  unnest(cols = c(ForwardData))

# Plot Forward Curve
ggplot(forward_results, aes(x = Forward_Period, y = Forward_Rate, color = as.factor(Date), group = Date)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  scale_color_manual(values = rainbow(length(unique(forward_results$Date)))) +
  labs(title = "1-Year Forward Curves from Jan 6 to Jan 17", x = "Forward Term (Years)", y = "Forward Rate (%)", color = "Date") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(size=10), legend.text = element_text(size=8))

