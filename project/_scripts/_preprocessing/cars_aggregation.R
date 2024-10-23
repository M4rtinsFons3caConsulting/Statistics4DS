library(readxl)
library(dplyr)
library(tidyr)

setwd("/home/shadybea/OneDrive/General/Statistics 4 Data Science/data/processed/vehicle_sales_data")

#################################### Brands ####################################

all_countries_brands <- read_excel("./all_sales_brand.xlsx")

eu_countries <- list(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"
)

eu_countries_brands <- all_countries_brands %>%
  filter(country %in% eu_countries) %>%
  group_by(brand_id, year, month) %>%
  summarize(Sales = sum(Sales), .groups="drop") %>%
  mutate(Country = "EU") %>%
  select(Country, year, month, brand_id, Sales)

colnames(eu_countries_brands) <- c("Country", "Year", "Month", "Brand", "Sales")

other_countries_brands <- all_countries_brands %>%
  filter(!(country %in% eu_countries)) %>%
  select(country, year, month, brand_id, Sales)

colnames(other_countries_brands) <- c("Country", "Year", "Month", "Brand", "Sales")

all_sales_brands <- rbind(eu_countries_brands, other_countries_brands) %>%
  filter(Brand %in% c("VW", "Tesla", "BYD Auto")) %>%
  pivot_wider(
    names_from=Brand,
    values_from=Sales
  ) %>%
  rename_with(~ paste0(.x, " Sales"), c("VW", "Tesla", "BYD Auto"))

View(all_sales_brands)

################################## Powertrain ##################################

all_countries_powertrain <- read_excel("./all_sales_powertrain.xlsx")

eu_countries_powertrain <- all_countries_powertrain %>%
  filter(country %in% eu_countries) %>%
  group_by(country, year, month, powertrain_type) %>%
  summarize(sales = sum(sales), .groups="drop") %>%
  mutate(country = "EU")

colnames(eu_countries_powertrain) <- c("Country", "Year", "Month", "Powertrain", "Sales")

other_countries_powertrain <- all_countries_powertrain %>%
  filter(!(country %in% eu_countries)) %>%
  select(country, year, month, powertrain_type, sales)

colnames(other_countries_powertrain) <- c("Country", "Year", "Month", "Powertrain", "Sales")

all_sales_powertrain <- rbind(eu_countries_powertrain, other_countries_powertrain) %>%
  filter(Powertrain == "total_sales") %>%
  select(-Powertrain)

all_sales_ev <- rbind(eu_countries_powertrain, other_countries_powertrain) %>%
  filter(Powertrain == "EV") %>%
  select(-Powertrain)


View(all_sales_ev)
