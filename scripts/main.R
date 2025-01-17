############################### LIBRARIES ######################################

library(openxlsx)
library(tidyverse)
library(corrplot)
library(zoo)
library(urca)
library(sandwich)
library(lmtest)
library(plm)
library(fpp3)

############################### INGESTION ######################################
# Import the data 
df <- read.csv("data/dataset.csv")
# View(df)

# Convert to tsibble
ts <- as_tsibble(df, key=country, index=year)
#View(ts)

########################### DATA TREATMENT #####################################

# Check NAs

# Checking the number of missing values in each column
data.frame(colSums(is.na(df)))

# colSums.is.na.df..
# country                          0
# year                             0
# evsales                          0
# evstock                          0
# evchargpnt                       0
# cpiyoy                           0
# industprod                       0
# totreserves                      0
# forexrate                        0
# cpi                              0
# gdp                              0
# unemprate                        0
# oilprice                         0
# evavgprice                       0
# milleagekm                       0
# lithbatpriceusd                  0
# newcaravgprice                   0
# pm25exp                         16
# co2emit                          0
# renergycon                       8
# lifexpect                        0


# Filling missing values using spline interpolation

df_filled <- df  # Make a copy of the original data frame

# Apply linear interpolation to each numeric column
for (col in names(df_filled)) {
  if (is.numeric(df_filled[[col]])) {
    df_filled[[col]] <- na.spline(df_filled[[col]])
  }
}

df <- df_filled

########################### INITIAL VISUALIZATION ##############################

# AUTOPLOTS
for (col in names(ts)[-1]) {
  print(autoplot(ts, !!sym(col)) + ggtitle(paste("Autoplot of", col)))
}

# PEARSON CORRELATION 

# Select numeric (excludes)
numeric_df <- df[, sapply(df, is.numeric)]

# Extract column names from the dataframe
vars <- colnames(numeric_df)

# Calculate the correlation matrix
corr_matrix <- cor(df[, vars], use = "pairwise.complete.obs")

# Generate the correlation plot
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 90, diag = FALSE)

# The variables evstock, evchargpoint, industprod, totreserves, millieagekm were
# dropped at this stage as they exhibit exceedingly large linear correlation 
# (abs(value) < .8) with other variables.

to_drop <- c('evstock', 'evchargpnt', 'industprod', 'totreserves', 'milleagekm')
df <- df %>% select(-to_drop)
############################ STATISTICAL TESTS ################################

# ADF

# Perform ADF test and save results to a text file
perform_adf_test <- function(data_frame, output_file = "adf_test_results.txt") {
  # Open a connection to the file
  file_conn <- file(output_file, open = "wt")
  
  # Write results to the file
  for (col in names(data_frame)) {
    # Check if the column is numeric (time series data is usually numeric)
    if (is.numeric(data_frame[[col]])) {
      
      # Perform the Augmented Dickey-Fuller test
      adf_test <- ur.df(data_frame[[col]], type = "trend", selectlags = "AIC")
      
      # Write column name
      cat("\nADF Test for", col, ":\n", file = file_conn)
      
      # Write test summary to file
      cat(capture.output(summary(adf_test)), file = file_conn, sep = "\n")
    }
  }
  
  # Close the file connection
  close(file_conn)
  
  cat("ADF test results saved to", output_file, "\n")
}

# Loop over each country
for (country in unique(df$country)) {
  # Subset the data for the current country
  country_data <- df[df$country == country, ]
  
  # Generate the output file path dynamically for each country
  output_file <- paste0('figures/adf_test_results_', country, '.txt')
  
  # Call the perform_adf_test function for the country's data
  perform_adf_test(country_data, output_file = output_file)
}

# Results of the analysis support the idea that all variables selected exhibit 
# non-stationary behavior for most countries - notable exceptions are:

# CHN, pm25
# FRA, unemp (10%)
# JPN, evsales(5%), co2emit(5%)
# USA, co2emit(5%)

# All of which show deterministic and stationary trends. 

###################### DATASETS

# Separate into two sets, where canonical uses the variables with academic support
df_canonical <- df %>% select(c('evsales', 'year','country', 'cpi', 'newcaravgprice','gdp', 'co2emit', 'lifexpect', 'renergycon','pm25exp'))
df_novelty <- df

###################### BASELINE

# Pooled OLS

pooled_df <- df_canonical %>% select(-c('year','country'))
model <- lm(pooled_df$evsales ~ ., data = pooled_df)

# Perform the Breusch-Pagan test
bptest(model)

# studentized Breusch-Pagan test
# data:  model
# BP = 38.678, df = 6, p-value = 8.275e-07

# Concluding that to model the data using Pooled OLS, heteroscedasticity must be
# corrected.

# Fixed effects model

# Fit a fixed effects model (including country and year fixed effects)
model <- lm(df_canonical$evsales ~ . + factor(df_canonical$country) + factor(df_canonical$year), data = df_canonical)
model
# Perform the Breusch-Pagan test
bptest(model)

# studentized Breusch-Pagan test
# 
# data:  model
# BP = 65.578, df = 24, p-value = 1.001e-05

# Random effects model

# Convert the data to a panel data frame
panel_data <- pdata.frame(df_canonical, index = c("country", "year"))
panel_data <- panel_data %>% select(-'evsales')
  
# Fit a random effects model
model <- plm(df_canonical$evsales ~ ., data = panel_data, model = "between")

# Perform the White test
bptest(model)


model

##################### CORRECTING NON-STATIONARITY #############################