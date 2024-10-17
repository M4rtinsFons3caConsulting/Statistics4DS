# Load the libraries
library(rvest)
library(dplyr)
library(httr)

# USA - INFLATION RATE

# URL of the page to scrape
url <- "https://www.usinflationcalculator.com/inflation/current-inflation-rates/"

# Read the HTML content of the page
page <- read_html(url)

# Extract the table with inflation rates
inflation_table <- page %>%
  html_element("table") %>%  # Gets the first table element on the page
  html_table()

# Display the table data
print(inflation_table)

# Save the table as a CSV file
write.csv(inflation_table, "USA_InflationRates.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'USA_InflationRates.csv' in your working directory.")


# CRUDE OIL

# URL of the page to scrape
url <- "https://finance.yahoo.com/quote/CL%3DF/history/?period1=1641772800&period2=1728570709"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
crude_oil <- page %>%
  html_element("table") %>%  # Gets the first table element on the page
  html_table()

# Display the table data
print(crude_oil)

# Save the table as a CSV file
write.csv(crude_oil, "CrudeOil.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'CrudeOil.csv' in your working directory.")

# NATURAL GAS

# URL of the page to scrape
url <- "https://finance.yahoo.com/quote/NG%3DF/history/?period1=1640995200&period2=1728571320"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
natural_gas <- page %>%
  html_element("table") %>%  # Gets the first table element on the page
  html_table()

# Display the table data
print(natural_gas)

# Save the table as a CSV file
write.csv(natural_gas, "NaturalGas.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'NaturalGas.csv' in your working directory.")

# LITHIUM

# URL of the page to scrape
url <- "https://finance.yahoo.com/quote/LITH-USD/history/?period1=1640995200&period2=1728571602"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
lithium <- page %>%
  html_element("table") %>%  # Gets the first table element on the page
  html_table()

# Display the table data
print(lithium)

# Save the table as a CSV file
write.csv(lithium, "Lithium.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'Lithium.csv' in your working directory.")

# RARE EARTH ETF

# URL of the page to scrape
url <- "https://finance.yahoo.com/quote/REMX/history/?period1=1640995200&period2=1728571812"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
rare_earth <- page %>%
  html_element("table") %>%  # Gets the first table element on the page
  html_table()

# Display the table data
print(rare_earth)

# Save the table as a CSV file
write.csv(rare_earth, "RareEarthETF.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'RareEarthETF.csv' in your working directory.")

# AUSTRALIA INTEREST RATE

# URL of the page to scrape
url <- "https://www.rba.gov.au/statistics/cash-rate/"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
au_interest_rate <- page %>%
  html_element("table") %>%  # Gets the first table element on the page
  html_table()

# Display the table data
print(au_interest_rate)

# Save the table as a CSV file
write.csv(au_interest_rate, "Australia_InterestRate.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'Australia_InterestRate.csv' in your working directory.")

# JAPAN CPI

# URL of the page to scrape
url <- "https://ycharts.com/indicators/japan_change_in_consumer_price_index"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
tables <- page %>%
  html_elements("table.table") 

# Access the two specific tables (adjust indices as necessary)
table1 <- tables[[6]] %>% html_table()  # First table
table2 <- tables[[7]] %>% html_table()  # Second table

# Check the structures of the tables (optional)
print(str(table1))
print(str(table2))

# Combine the tables (stack them one on top of the other)
jpn_cpi <- bind_rows(table1, table2)

# View the combined table
print(jpn_cpi)

# Save the table as a CSV file
write.csv(jpn_cpi, "Japan_CPI.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'Japan_CPI.csv' in your working directory.")

# NICKEL PRICE

# URL of the page to scrape
url <- "https://ycharts.com/indicators/nickel_price"

# Read the HTML content of the page
page <- read_html(GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")))

# Extract the table with inflation rates
tables <- page %>%
  html_elements("table.table") 

# Access the two specific tables (adjust indices as necessary)
table1 <- tables[[6]] %>% html_table()  # First table
table2 <- tables[[7]] %>% html_table()  # Second table

# Check the structures of the tables (optional)
print(str(table1))
print(str(table2))

# Combine the tables (stack them one on top of the other)
nickel <- bind_rows(table1, table2)

# View the combined table
print(nickel)

# Save the table as a CSV file
write.csv(nickel, "Nickel.csv", row.names = FALSE)

# Message to confirm saving
cat("The data has been saved as 'Nickel.csv' in your working directory.")








