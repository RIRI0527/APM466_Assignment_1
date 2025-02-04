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

# Define function to calculate YTM
bond_ytm_fsolve <- function(price, coupon_rate, face_value, years_to_maturity, payments_per_year=2) {
  coupon_payment <- (coupon_rate / payments_per_year) * face_value
  n_periods <- round(years_to_maturity * payments_per_year)
  cash_flows <- c(rep(coupon_payment, n_periods - 1), coupon_payment + face_value)
  times <- (1:n_periods) / payments_per_year
  
  npv_function <- function(r) {
    -price + sum(cash_flows * (1 + r) ^ -times)
  }
  
  ytm_solution <- fzero(npv_function, 0.02)$x  # Initial guess at 2%
  return(ytm_solution)
}

# Compute YTM
ytm_results <- df_bonds %>%
  select(`Bond Name`, starts_with("2024/")) %>%
  pivot_longer(cols = -`Bond Name`, names_to = "Date", values_to = "Price") %>%
  left_join(select(df_bonds, `Bond Name`, Coupon, `Years to Maturity`), by = "Bond Name") %>%
  mutate(YTM = mapply(bond_ytm_fsolve, Price, Coupon, 100, `Years to Maturity`))

# Filter outliers
ytm_results <- ytm_results %>% mutate(YTM = ifelse(YTM > 0.5, NA, YTM))  # Remove YTM >50%

# Convert Date to proper format
ytm_results$Date <- as.Date(ytm_results$Date, format="%Y/%m/%d")

# Plot Yield Curve in requested format
ggplot(ytm_results, aes(x = `Years to Maturity`, y = YTM, color = as.factor(Date), group = Date)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  scale_color_manual(values = rainbow(length(unique(ytm_results$Date)))) +
  labs(title = "YTM Curves from Jan 6 to Jan 17", x = "Time to Maturity", y = "YTMs", color = "Date") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(size=10), legend.text = element_text(size=8))

