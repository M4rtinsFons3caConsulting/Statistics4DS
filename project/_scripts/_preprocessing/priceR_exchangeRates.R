library(priceR)
library(tidyverse)
options(scipen = 100); options(digits = 6)

Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="x")

setwd("/home/shadybea/OneDrive/General/Statistics 4 Data Science/data/raw/exchange_rates/")

# Define the list of currencies (ISO currency codes)
# Load required libraries
library(dplyr)

# Define the list of currencies
currencies <- c("GBP", "CAD", "EUR", "KRW", "AUD", "JPY", "CHF", "NZD", "NOK")

# Define the function to retrieve historical exchange rates and save to CSV
retrieve_exchange_rates <- function(currency_code) {
  tryCatch({
    # Retrieve currency to USD exchange rates
    to_usd <- historical_exchange_rates("USD", to = currency_code,
                                        start_date = "2022-01-01", end_date = "2024-09-30")

    # Ensure the data frame has the required structure and no missing values
    if (!is.data.frame(to_usd) || nrow(to_usd) == 0) {
      message(paste("No data retrieved for", currency_code))
      return(NULL)
    }
    
    # Handle missing data
    to_usd <- na.omit(to_usd)
    
    # Save the result to a CSV file
    csv_filename <- paste0(currency_code, "_ExchangeRate.csv")
    write.csv(to_usd, csv_filename, row.names = FALSE)
    
    return(to_usd)
  }, error = function(e) {
    message(paste("Error retrieving data for", currency_code, ":", e$message))
    return(NULL)
  })
}

# Use a simple for loop to iterate over the list of currencies and retrieve the data
results <- vector("list", length(currencies))
names(results) <- currencies

for (i in seq_along(currencies)) {
  currency_code <- currencies[i]
  message(paste("Processing:", currency_code))
  results[[currency_code]] <- retrieve_exchange_rates(currency_code)
}

# Filter out any NULL results (currencies with errors)
results <- Filter(Negate(is.null), results)

# Optionally, print the first few rows of each successful result
for (currency_code in names(results)) {
  cat("\nExchange rates for:", currency_code, "\n")
  print(head(results[[currency_code]]))
}

