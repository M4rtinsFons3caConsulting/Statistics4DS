library(dplyr)
library(purrr)
library(tidyverse)
library(zoo)
library(stringr)

# Setting the working directory
setwd("/home/shadybea/OneDrive/General/Statistics 4 Data Science/data/raw")

################################# Exchange Rate ################################

# Create a named list where the key is the country code and the value is the currency code
currency_codes <- list(
  US = "USD",
  UK = "GBP",
  CA = "CAD",
  EU = "EUR",
  KR = "KRW",
  AU = "AUD",
  JP = "JPY",
  CH = "CHF",
  NZ = "NZD",
  NO = "NOK",
  DE = "EUR"
)

# Create an empty list to store the data frames for each country
exchange_data_list <- list()

# Loop through each item in the named list
for (country in names(currency_codes)) {
  # Get the currency code for the current country
  currency_code <- currency_codes[[country]]
  
  # Construct the file path using the currency code
  file_path <- paste0("./exchange_rates/", currency_code, "_ExchangeRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(date, into = c("Year", "Month", "Day"), sep = "-") %>%
    filter(Day == "01") %>%  # Select only the data where Day is '01'
    select(-Day) %>%  # Discard the Day column
    mutate(across(c(Year, Month), as.integer))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "ExchangeRate")

  # Add the Country column
  curr_data <- curr_data %>%
    mutate(Country = country) %>%
    relocate(Country, .before = Year)
  
  # Store the processed data frame in the list
  exchange_data_list[[country]] <- curr_data
}

# Combine all the data frames into one
exchange_rates <- do.call(rbind, exchange_data_list)

View(exchange_rates)

# Save the combined data to a CSV file
write.csv(exchange_rates, file = "../processed/exchange_rates.csv", row.names = FALSE)

###################################### GDP #####################################

countries <- list(
  US = "USA",
  UK = "UK",
  CA = "Canada",
  EU = "EU",
  KR = "SouthKorea",
  AU = "Australia",
  JP = "Japan",
  CH = "Switzerland",
  NO = "Norway",
  DE = "Germany"
)

nz <- list(
  NZ = "NewZealand"
)

# Create an empty list to store the data frames for each country
gdp_data_list <- list()

# Loop through each item in the named list
for (code in names(countries)) {
  country <- countries[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./gdp/", country, "_GDP.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(DATE, into = c("Year", "Month", "Day"), sep = "-") %>%
    mutate(across(c(Year, Month), as.integer)) %>%  
    select(-Day)  # Discard the Day column
  
  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "GDP")
  
  full_years = as.integer(2022:2024)
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after September 2024
    left_join(curr_data, by = c("Year", "Month")) %>%  # Join with the current data
    arrange(Year, Month) %>%  # Sort by Year and Month
    fill(GDP, .direction = "down") %>%  # Fill missing values downward
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year)  # Move the Country column before Year
  
  # Store the processed data frame in the list
  gdp_data_list[[code]] <- complete_months
}

for (code in names(nz)) {
  country <- nz[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./gdp/", country, "_GDP.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Date, into = c("Month", "Year"), sep = " ") %>%
    mutate(Month = match(Month, month.abb), Year = as.integer(Year)) %>%
    select(Year, Month, Value) %>%
    filter(Year >= 2021)

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "GDP")
  
  full_years = 2021:2024
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after September 2024
    left_join(curr_data, by = c("Year", "Month")) %>%  # Join with the current data
    arrange(Year, Month) %>%  # Sort by Year and Month
    fill(GDP, .direction = "down") %>%  # Fill missing values downward
    filter(Year >= 2022) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year)  # Move the Country column before Year
  
  # Store the processed data frame in the list
  gdp_data_list[[code]] <- complete_months
}

# Combine all the data frames into one
gdp <- do.call(rbind, gdp_data_list)

View(gdp)

# Save the combined data to a CSV file
write.csv(gdp, file = "../processed/gdp.csv", row.names = FALSE)


##################################### CPI ######################################

trd_econ_month <- list(
  UK = "UK",
  CA = "Canada",
  KR = "SouthKorea"
)

trd_econ_quarter <- list(
  AU = "Australia",
  NZ = "NewZealand"
)

eurostat <- list(
  US = "USA",
  EU = "EU",
  CH = "Switzerland",
  NO = "Norway",
  DE = "Germany"
)

other <- list(
  JP = "Japan"
)

cpi_data_list <- list()

# Loop through each item in the named list
for (code in names(trd_econ_month)) {
  country <- trd_econ_month[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./cpi/", country, "_CPI.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(DATE, into = c("Year", "Month", "Day"), sep = "-") %>%
    mutate(across(c(Year, Month), as.integer)) %>%  
    select(-Day)  # Discard the Day column
  
  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "CPI")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate("Monthly Inflation Rate" = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
    select(-CPI) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Country", "Monthly Inflation Rate"), .direction = "down")
  
  # Store the processed data frame in the list
  cpi_data_list[[code]] <- complete_months
}

# Loop through each item in the named list
for (code in names(trd_econ_quarter)) {
  country <- trd_econ_quarter[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./cpi/", country, "_CPI.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(DATE, into = c("Year", "Month", "Day"), sep = "-") %>%
    mutate(across(c(Year, Month), as.integer)) %>%  
    select(-Day)  # Discard the Day column
  
  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "CPI")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 10)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month)
  
  # we assume the growth of the next periods as the average growth of the previous 4 periods
  growth <- list(
    "AU" = 1.01,
    "NZ" = 1.005
  )
  
  complete_months <- complete_months %>%
    mutate(CPI = ifelse(Year == 2024 & Month %in% c(7, 10) & code %in% names(growth),
                        zoo::na.locf(CPI, na.rm = FALSE) * growth[[code]], CPI))
  
  # Fill missing CPI values using the logic from the Python code
  for (index in 1:nrow(complete_months)) {
    if (!is.na(complete_months$CPI[index])) {
      # If CPI is not missing, keep it as is
      complete_months$CPI[index] <- complete_months$CPI[index]
    } else {
      # Perform forward fill for the previous value
      previous_value <- zoo::na.locf(complete_months$CPI, na.rm = FALSE)[index]

      # Perform backward fill for the next value
      next_value <- zoo::na.locf(complete_months$CPI, fromLast = TRUE, na.rm = FALSE)[index]

      # Calculate alpha (difference divided by 3)
      alpha <- (next_value - previous_value) / 3

      # Get the forward-filled CPI value (previous CPI)
      next_column_value <- zoo::na.locf(complete_months$CPI, na.rm = FALSE)[index] + alpha

      # Assign the interpolated value to 'CPI'
      complete_months$CPI[index] <- next_column_value
    }
  }

  complete_months <- complete_months %>%
    filter(!(Year == 2024 & Month == 10)) %>%
    mutate("Monthly Inflation Rate" = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
    select(-CPI) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Country", "Monthly Inflation Rate"), .direction = "down")
  
  # Store the processed data frame in the list
  cpi_data_list[[code]] <- complete_months
}

# Loop through each item in the named list
for (code in names(eurostat)) {
  country <- eurostat[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./cpi/", country, "_CPI.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  curr_data <- curr_data %>%
    select(c(geo, TIME_PERIOD, OBS_VALUE)) %>%
    separate(TIME_PERIOD, into = c("Year", "Month"), sep = "-") %>%
    mutate(across(c(Year, Month), as.integer)) 
  
  # Rename the columns
  colnames(curr_data) <- c("Country", "Year", "Month", "CPI")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = ifelse(Country == 'EU27_2020', "EU", Country)) %>%
    mutate("Monthly Inflation Rate" = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
    select(-CPI) %>%
    fill(c("Country", "Monthly Inflation Rate"), .direction = "down")
  
  # Store the processed data frame in the list
  cpi_data_list[[code]] <- complete_months
  
}

for (code in names(other)) {
  country <- other[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./cpi/", country, "_CPI.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Date, into = c("Month", "Day", "Year"), sep = " ", remove = FALSE) %>%
    select(Year, Month, Value) %>%
    mutate(Month = match(Month, month.name), across(c(Month, Year), as.integer))
  
  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "CPI")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate("Monthly Inflation Rate" = (CPI - lag(CPI)) / lag(CPI) * 100) %>%
    select(-CPI) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Country", "Monthly Inflation Rate"), .direction = "down")
  
  # Store the processed data frame in the list
  cpi_data_list[[code]] <- complete_months
}

# Combine all the data frames into one
cpi <- do.call(rbind, cpi_data_list)

cpi <- cpi %>%
  replace_na(list("Monthly Inflation Rate" = 0))
  

View(cpi)

# Save the combined data to a CSV file
write.csv(cpi, file = "../processed/inflation_rate.csv", row.names = FALSE)


################################# Interest Rate ################################

date_w_day <- list(
  "US" = "USA",
  "KR" = "SouthKorea",
  "EU" = "EU",
  "DE" = "EU"
)

date_no_day <- list(
  "CA" = "Canada",
  "NO" = "Norway"
)

three_col <- list(
  "JP" = "Japan",
  "CH" = "Switzerland"
)

uk <- list(
  "UK" = "UK"
)

au <- list(
  "AU" = "Australia"
)

nz <- list(
  "NZ" = "NewZealand"
)

int_rate_data_list <- list()

for (code in names(date_w_day)) {
  country <- date_w_day[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./interest_rate/", country, "_InterestRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(DATE, into = c("Year", "Month", "Day"), sep = "-") %>%
    select(-Day) %>%
    mutate(Month = as.integer(Month), Year = as.integer(Year)) %>%
    filter(!(Year == 2024 & Month == 10))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Interest Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill("Interest Rate", .direction="down")
  
  # Store the processed data frame in the list
  int_rate_data_list[[code]] <- complete_months
}

for (code in names(date_no_day)) {
  country <- date_no_day[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./interest_rate/", country, "_InterestRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(names(curr_data)[1], into = c("Year", "Month"), sep = "-") %>%
    mutate(Month = as.integer(Month))
  
  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Interest Rate")
  
  curr_data <- curr_data %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  # Store the processed data frame in the list
  int_rate_data_list[[code]] <- curr_data
}

for (code in names(three_col)) {
  country <- three_col[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./interest_rate/", country, "_InterestRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, sep=";", stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Date, into = c("Year", "Month"), sep = "-") %>%
    select(-D0) %>%
    mutate(Month = as.integer(Month), Year = as.integer(Year))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Interest Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill("Interest Rate", .direction="down")
  
  # Store the processed data frame in the list
  int_rate_data_list[[code]] <- complete_months
}

for (code in names(uk)) {
  country <- uk[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./interest_rate/", country, "_InterestRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    select(-Day) %>%
    mutate(Month = match(Month, month.abb))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Interest Rate")
  
  full_years = as.integer(2021:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after September 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month)
  
  complete_months$`Interest Rate` <- na.locf(complete_months$`Interest Rate`, na.rm=FALSE)

  complete_months <- complete_months %>%
    filter(Year >= 2022) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  # Store the processed data frame in the list
  int_rate_data_list[[code]] <- complete_months
}

for (code in names(au)) {
  country <- au[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./interest_rate/", country, "_InterestRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Effective.Date, into = c("Day", "Month", "Year"), sep = " ") %>%
    select(Year, Month, Cash.rate.target..) %>%
    filter(Year %in% c(2022, 2023, 2024)) %>%
    mutate(Month = match(Month, month.abb), Year = as.integer(Year))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Interest Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after September 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month)
  
  complete_months <- complete_months %>%
    select(Year, Month, "Interest Rate") %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill("Interest Rate", .direction = "up")
  
  # Store the processed data frame in the list
  int_rate_data_list[[code]] <- complete_months
}

for (code in names(nz)) {
  country <- nz[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./interest_rate/", country, "_InterestRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(DATE, into = c("Day", "Month", "Year"), sep = "-") %>%
    select(-Day) %>%
    filter(Year %in% c(2022, 2023, 2024)) %>%
    mutate(Month = as.integer(Month))

  # Rename the columns
  colnames(curr_data) <- c("Month", "Year", "Interest Rate")

  curr_data <- curr_data %>%
    select(Year, Month, "Interest Rate") %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) 
  
  # Store the processed data frame in the list
  int_rate_data_list[[code]] <- curr_data
}

# Combine all the data frames into one
int_rate <- do.call(rbind, int_rate_data_list)

View(int_rate)

# Save the combined data to a CSV file
write.csv(int_rate, file = "../processed/interest_rate.csv", row.names = FALSE)


############################### Unemployment Rate ##############################

ecb <- list(
  "EU" = "EU",
  "JP" = "Japan",
  "NO" = "Norway",
  "CH" = "Switzerland",
  "US" = "USA"
)

au <- list(
  "AU" = "Australia"
)

ca <- list(
  "CA" = "Canada"
)

de <- list(
  "DE" = "Germany"
)

nz <- list(
  "NZ" = "NewZealand"
)

kr <- list(
  "KR" = "SouthKorea"
)

uk <- list(
  "UK" = "UK"
)

unemp_rate_data_list <- list()

for (code in names(ecb)) {
  country <- ecb[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    select(geo, TIME_PERIOD, OBS_VALUE) %>%
    separate(TIME_PERIOD, into = c("Year", "Month"), sep = "-") %>%
    mutate(across(c(Year, Month), as.integer))

  # Rename the columns
  colnames(curr_data) <- c("Country", "Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Country, Year, Month) %>%
    mutate(Country = ifelse(Country == 'EU27_2020', "EU", Country)) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

for (code in names(au)) {
  country <- au[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Date, into = c("Month", "Year"), sep = "-") %>%
    mutate(Year = as.integer(paste0("20",Year)), Month = match(Month, month.abb)) %>%
    filter(Year >= 2022) %>%
    select(Year, Month, Trend....)

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    fill("Unemployment Rate", .direction = "up") %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

for (code in names(ca)) {
  country <- ca[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Date, into = c("Month", "Year"), sep = "-") %>%
    mutate(Month = match(Month, month.abb)) %>%
    select(Year, Month, Value) %>%
    mutate(Year = as.integer(Year))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

for (code in names(de)) {
  country <- de[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(DATE, into = c("Year", "Month", "Day"), sep = "-") %>%
    select(-c(Day, TIME.PERIOD)) %>%
    filter(Year >= 2022) %>%
    mutate(Month = as.integer(Month), Year = as.integer(Year))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

for (code in names(nz)) {
  country <- nz[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Category, into = c("Month", "Year"), sep = "-") %>%
    mutate(Year = as.integer(paste0("20", Year)), Month = match(Month, month.abb)) %>%
    select(Year, Month, Total) %>%
    filter(Year >= 2021 & Month >= 12)

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2021:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = 1:12
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after September 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month)
  
  complete_months$`Unemployment Rate` <- na.locf(complete_months$`Unemployment Rate`, na.rm=FALSE)

  complete_months <- complete_months %>%
    filter(Year >= 2022) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

for (code in names(kr)) {
  country <- kr[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  curr_data <- curr_data %>%
    pivot_longer(
      cols=starts_with("X202"),
      names_to="Date",
      values_to="Value"
    ) %>%
    select(Date, Value) %>%
    mutate(Date = gsub("^[A-Za-z]+", "", Date)) %>%
    separate(Date, into = c("Year", "Month"), sep = "\\.") %>%
    mutate(across(c(Year, Month), as.integer))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

for (code in names(uk)) {
  country <- uk[[code]]
  
  # Construct the file path using the country name
  file_path <- paste0("./unemployment_rate/", country, "_UnemploymentRate.csv")
  
  # Read the file
  curr_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Split the date column into Year, Month, and Day
  curr_data <- curr_data %>%
    separate(Date, into = c("Year", "Month"), sep = " ") %>%
    filter(Year >= 2022) %>%
    mutate(Month = str_to_title(Month), Month = match(Month, month.abb), Year = as.integer(Year))

  # Rename the columns
  colnames(curr_data) <- c("Year", "Month", "Unemployment Rate")
  
  full_years = as.integer(2022:2024)
  # Ensure Year and Month are treated as character values
  complete_months <- expand.grid(
    Year = full_years,
    Month = as.integer(1:12)
  ) %>%
    filter(!(Year == 2024 & Month > 9)) %>%  # Filter out months after October 2024
    left_join(curr_data, by = c("Year", "Month")) %>%
    arrange(Year, Month) %>%
    mutate(Country = code) %>%  # Add the country code column
    relocate(Country, .before = Year) %>%
    fill(c("Unemployment Rate", "Country"), .direction = "down")
  
  # Store the processed data frame in the list
  unemp_rate_data_list[[code]] <- complete_months
}

# Combine all the data frames into one
unemp_rate <- do.call(rbind, unemp_rate_data_list) %>%
  select(Country, Year, Month, "Unemployment Rate")

View(unemp_rate)

# Save the combined data to a CSV file
write.csv(unemp_rate, file = "../processed/unemployment_rate.csv", row.names = FALSE)


###################################### Coal ####################################

countries <- list(
  "AU", "CA", "EU", "DE", "JP", "NZ", "NO", "KR", "CH", "UK", "US"
)

file_path <- paste0("./other/Coal.csv")

# Read the file
curr_data <- read.csv(file_path, stringsAsFactors = FALSE)

# Split the date column into Year, Month, and Day
curr_data <- curr_data %>%
  separate(Date, into = c("Month", "Year"), sep = "-") %>%
  mutate(Month = match(Month, month.abb)) %>%
  select(Year, Month, Value)

# Rename the columns
colnames(curr_data) <- c("Year", "Month", "Coal")

coal_data_list <- list()

for (country in countries) {
  df <- curr_data %>%
    mutate(Country = country) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  coal_data_list[[country]] <- df
}

coal <- do.call(rbind, coal_data_list)

View(coal)

# Save the combined data to a CSV file
write.csv(coal, file = "../processed/coal.csv", row.names = FALSE)


#################################### Lithium ###################################

file_path <- paste0("./other/Lithium.csv")

# Read the file
curr_data <- read.csv(file_path, stringsAsFactors = FALSE)

curr_data <- curr_data %>%
  select(Date, Open) %>%
  separate(Date, into=c("Month", "Day", "Year"), sep = " ") %>%
  filter(Day == "1,") %>%
  select(Year, Month, Open) %>%
  mutate(Month = match(Month, month.abb))

colnames(curr_data) <- c("Year", "Month", "Lithium")

lithium_data_list <- list()

for (country in countries) {
  df <- curr_data %>%
    mutate(Country = country) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  lithium_data_list[[country]] <- df
}

lithium <- do.call(rbind, lithium_data_list)

View(lithium)

# Save the combined data to a CSV file
write.csv(lithium, file = "../processed/lithium.csv", row.names = FALSE)

################################## Natural Gas #################################

file_path <- paste0("./other/NaturalGas.csv")

# Read the file
curr_data <- read.csv(file_path, stringsAsFactors = FALSE)

curr_data <- curr_data %>%
  select(Date, Open) %>%
  separate(Date, into=c("Month", "Day", "Year"), sep = " ") %>%
  mutate(Day = as.integer(gsub(",$", "", Day))) %>%
  group_by(Year, Month) %>%
  slice_min(Day, n=1) %>%
  ungroup() %>%
  select(Year, Month, Open) %>%
  mutate(Month = match(Month, month.abb))

colnames(curr_data) <- c("Year", "Month", "Natural Gas")

natural_gas_data_list <- list()

for (country in countries) {
  df <- curr_data %>%
    mutate(Country = country) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  natural_gas_data_list[[country]] <- df
}

natural_gas <- do.call(rbind, natural_gas_data_list)

View(natural_gas)

# Save the combined data to a CSV file
write.csv(natural_gas, file = "../processed/natural_gas.csv", row.names = FALSE)

##################################### Nickel ###################################

file_path <- paste0("./other/Nickel.csv")

# Read the file
curr_data <- read.csv(file_path, stringsAsFactors = FALSE)

curr_data <- curr_data %>%
  separate(Date, into=c("Month", "Day", "Year"), sep = " ") %>%
  select(Year, Month, Value) %>%
  filter(Year >= 2022) %>%
  mutate(Month = match(Month, month.name))

colnames(curr_data) <- c("Year", "Month", "Nickel")

nickel_data_list <- list()

for (country in countries) {
  df <- curr_data %>%
    mutate(Country = country) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  nickel_data_list[[country]] <- df
}

nickel <- do.call(rbind, nickel_data_list)

View(nickel)

# Save the combined data to a CSV file
write.csv(nickel, file = "../processed/nickel.csv", row.names = FALSE)

################################# Rare Earths ETF ##############################

file_path <- paste0("./other/RareEarthETF.csv")

# Read the file
curr_data <- read.csv(file_path, stringsAsFactors = FALSE)

curr_data <- curr_data %>%
  select(Date, Open) %>%
  separate(Date, into=c("Month", "Day", "Year"), sep = " ") %>%
  mutate(Day = as.integer(gsub(",$", "", Day))) %>%
  group_by(Year, Month) %>%
  slice_min(Day, n=1) %>%
  ungroup() %>%
  select(Year, Month, Open) %>%
  mutate(Month = match(Month, month.abb))

colnames(curr_data) <- c("Year", "Month", "Rare Earth ETF")

rare_earth_data_list <- list()

for (country in countries) {
  df <- curr_data %>%
    mutate(Country = country) %>%  # Add the country code column
    relocate(Country, .before = Year)
  
  rare_earth_data_list[[country]] <- df
}

rare_earth <- do.call(rbind, rare_earth_data_list)

View(rare_earth)

# Save the combined data to a CSV file
write.csv(rare_earth, file = "../processed/rare_earth_etf.csv", row.names = FALSE)

##################################### Tariffs ##################################

file_path <- paste0("./other/Tariffs.csv")

# Read the file
curr_data <- read.csv(file_path, stringsAsFactors = FALSE)

countries <- list(
  "United States" = "US",
  "United Kingdom" = "UK",
  "Canada" = "CA",
  "European Union" = "EU",
  "South Korea" = "KR",
  "Australia" = "AU",
  "Japan" = "JP",
  "Switzerland" = "CH",
  "New Zealand" = "NZ",
  "Norway" = "NO"
)

  
start_date <- ymd("2022-01-01")
end_date <- ymd("2024-09-30")
months_seq <- seq.Date(from = start_date, to = end_date, by = "month")

# Convert to "year-month" format for merging
months_seq_df <- data.frame(Date = format(months_seq, "%b-%Y"))

# Step 2: Create a data frame with all combinations of countries and complete dates
complete_data <- expand.grid(Date = months_seq_df$Date, Country = unique(curr_data$Country))

# Step 3: Merge with the original data to fill in Tariff values
final_data <- complete_data %>%
  left_join(curr_data, by = c("Date", "Country")) %>%
  arrange(Country, Date) %>%
  separate(Date, into=c("Month", "Year"), sep="-") %>%
  mutate(Month = match(Month, month.abb), Year = as.integer(Year)) %>%
  mutate(Country = recode(Country, !!!countries)) %>%
  select(Country, Year, Month, Tariff) %>%
  arrange(Country, Year, Month) %>%
  fill(Tariff, .direction = "down")

de_data <- final_data %>%
  filter(Country == "EU") %>%
  mutate(Country = "DE")

final_data <- final_data %>%
  bind_rows(de_data)

colnames(final_data) = c("Country", "Year", "Month", "EV Import Tariff")

View(final_data)

# Save the combined data to a CSV file
write.csv(final_data, file = "../processed/tariffs.csv", row.names = FALSE)
