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
# country                      0
# year                         0
# evsales                      0
# evstock                      0
# evchargpnt                   0
# cpiyoy                       0
# cpi                          0
# gdp                          0
# totreserves                  7
# unemprate                    7
# co2emit                     38
# lifexpect                   38
# pm25exp                    114
# renergycon                  76


# Filling missing values using spline interpolation

df_filled <- df  # Make a copy of the original data frame

# Apply linear interpolation to each numeric column
for (col in names(df_filled)) {
  if (is.numeric(df_filled[[col]])) {
    df_filled[[col]] <- na.spline(df_filled[[col]])
  }
}

df <- df_filled
df$evsales <- as.integer(df$evsales)
# df$renergycon <- ifelse(df$renergycon < 0, 0, df$renergycon)

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
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 90, diag = FALSE)

# The variables evstock, evchargpoint, totreserves were dropped at this stage as
# they exhibit exceedingly large linear correlation (abs(value) > .8) with other
# variables.

to_drop <- c('evstock', 'evchargpnt', 'totreserves')
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
df_canonical <- df %>% select(c('evsales', 'year','country', 'cpi', 'gdp', 'co2emit', 'lifexpect', 'renergycon','pm25exp'))
df_novelty <- df

###################### BASELINE

# Pooled OLS

pooled_df <- df_novelty %>% select(-c('year','country'))
model <- plm(pooled_df$evsales ~ cpi + gdp + co2emit + lifexpect + renergycon + pm25exp, data = pooled_df, model = 'pooling')
model
# Perform the Breusch-Pagan test
bptest(model)

# studentized Breusch-Pagan test
# data:  model
# BP = 1.0919, df = 6, p-value = 0.9817

# Concluding that the data is not heteroscedastic


# Fixed effects model

# Fit a fixed effects model (including country and year fixed effects)
fixed_df <- df_novelty %>% select(-c('year','country'))
model <- plm(fixed_df$evsales ~ cpi + gdp + co2emit + lifexpect + renergycon + pm25exp, data = df_novelty, model='within')
model
# Perform the Breusch-Pagan test
bptest(model)

# studentized Breusch-Pagan test
# 
# data:  model
# BP = 38.852, df = 6, p-value = 0.0000007652

# Concluding that to model the data using a Random Effects model, heteroscedasticity
# must be corrected.


# Random effects model
  
# Fit a random effects model
random_df <- df_novelty %>% select(-c('year','country'))
model <- plm(evsales ~ cpi + gdp + co2emit + lifexpect + renergycon + pm25exp, data = df_novelty, model = "random")
model
# Perform the White test
bptest(model)

# studentized Breusch-Pagan test
# 
# data:  model
# BP = 40.172, df = 6, p-value = 4.214e-07

# Concluding that to model the data using a Random Effects model, heteroscedasticity
# must be corrected.


##################### CORRECTING NON-STATIONARITY #############################

columns_to_log <- c('evsales', 'cpi', 'gdp', 'unemprate', 'co2emit', 'lifexpect', 'renergycon', 'pm25exp')
log_df <- df
log_df[columns_to_log] <- log(df[columns_to_log] + 1)

df_log_canonical <- log_df %>% select(c('evsales', 'year','country', 'cpi','gdp', 'co2emit', 'lifexpect', 'renergycon','pm25exp'))
df_log_novelty <- log_df

###################### BASELINE

# Pooled OLS

pooled_log_df <- df_log_canonical %>% select(-c('year','country'))
model <- plm(pooled_log_df$evsales ~ cpi + gdp + co2emit + lifexpect + renergycon + pm25exp, data = df_log_canonical, model = 'pooling')
model
# Perform the Breusch-Pagan test
bptest(model)

# studentized Breusch-Pagan test
# 
# data:  model
# BP = 7.6814, df = 6, p-value = 0.2624

# Based on this p-value, we fail to reject the null hypothesis and can conclude
# the model does not suffer from heteroskedacity.


# Fixed effects model

# Fit a fixed effects model (including country and year fixed effects)
fixed_log_df <- df_log_novelty %>% select(-c('year','country'))
model <- plm(evsales ~ cpi + gdp + co2emit + lifexpect + renergycon + pm25exp, data = df_log_novelty, model='within')
model
# Perform the Breusch-Pagan test
bptest(model)

# studentized Breusch-Pagan test
# 
# data:  model
# BP = 79.121, df = 6, p-value = 0.000000000000005427


# Random effects model

# Convert the data to a panel data frame
panel_data <- pdata.frame(df_log_novelty, index = c("country", "year"))

# Fit a random effects model
random_df <- panel_data %>% select(-c('year','country'))
model <- plm(evsales ~ cpi + gdp + co2emit + lifexpect + renergycon + pm25exp, data = panel_data, model = "random")
model
# Perform the White test
bptest(model)

# studentized Breusch-Pagan test
# 
# data:  model
# BP = 79.121, df = 6, p-value = 0.000000000000005427
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  