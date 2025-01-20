############################ LIBRARIES #########################################
library(car)
library(openxlsx)
library(tidyverse)
library(corrplot)
library(zoo)
library(urca)
library(sandwich)
library(lmtest)
library(plm)
library(fpp3)
library(recipes)
library(forecast)
library(lmtest)
library(ggplot2)
library(patchwork)
library(lme4)
############################ INGESTION #########################################
# Import the data 
dataframe <- read.csv("data/dataset.csv")
variables <- setdiff(names(dataframe), c('year', 'country'))

############################ PEARSON CORRELATION ###############################
# Calculate the correlation matrix
corr_matrix <- cor(dataframe[, variables], use = "pairwise.complete.obs")

# Generate the correlation plot
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 90, diag = FALSE)

# These variables show high levels of linear correlation
to_drop <- c('evstock', 'evchargpnt', 'totreserves')
dataframe <- dataframe %>% select(-to_drop)

# Update the 'variables' list after dropping the columns
variables <- setdiff(names(dataframe), c('year', 'country'))

############################ MISSING VALUES ####################################
# Checking the number of missing values in each column
data.frame(colSums(is.na(dataframe)))
# colSums.is.na.dataframe..
# country                      0
# year                         0
# evsales                      0
# evstock                      0
# evchargpnt                   184
# cpi                          0
# gdp                          0
# totreserves                  7
# unemprate                    7
# co2emit                     38
# lifexpect                   38
# pm25exp                    114
# renergycon                  76

# Initialize the filled dataframe
dataframe_filled <- dataframe

# Iterate over each country group
countries <- unique(dataframe_filled$country)

# Iterate over the countries
for (country in countries) {
  # Subset the data for the current country
  dataframe_country <- dataframe_filled[dataframe_filled$country == country, ]
  # Iterate over the columns (variables)
  for (col in variables) {
    # Interpolate missing values within the country
    if (any(is.na(dataframe_country[[col]]))) {
      dataframe_country[[col]] <- na.approx(dataframe_country[[col]], na.rm = FALSE)
    }
    # Extrapolate missing values at the end
    if (any(is.na(dataframe_country[[col]]))) {
      # Fit a linear model to existing non-NA values
      model <- lm(dataframe_country[[col]] ~ seq_along(dataframe_country[[col]]), data = dataframe_country)
      # Identify NAs and extrapolate for them
      na_idx <- which(is.na(dataframe_country[[col]]))
      # Print missing indices for debugging
      if (length(na_idx) > 0) {
        cat("Missing values for country", country, "in column", col, "at indices:", na_idx, "\n")
      }
      # Extrapolate the missing values
      if (length(na_idx) > 0) {
        dataframe_country[[col]][na_idx] <- predict(model, newdata = data.frame(seq_along = na_idx))
      }
    }
  }
  # Replace the country data back into the full dataframe
  dataframe_filled[dataframe_filled$country == country, ] <- dataframe_country
}

dataframe <- dataframe_filled
############################ VARIABLE ENGINEERING ##############################
# Adding differences
dataframe <- dataframe %>%
  mutate(
    unemprate_diff = c(NA, diff(unemprate)),
    gdp_diff = c(NA, diff(gdp)),
    co2emit_diff = c(NA, diff(co2emit)),
    cpi_diff = c(NA, diff(cpi)),
    pm25exp_diff = c(NA, diff(pm25exp)),
    lifexpect_diff = c(NA, diff(lifexpect)),
    evsales_diff = c(NA, diff(evsales)),
    renergycon_diff = c(NA, diff(renergycon))
  ) %>%
  filter(year > 2011)

# Adding squares
dataframe <- dataframe %>%
  mutate(
    co2emit_square = co2emit^2,
    cpi_square = cpi^2,
    pm25exp_square = pm25exp^2,
    lifexpect_square = lifexpect^2,
    evsales_square = evsales^2,
    renergycon_square = renergycon^2
  )

# Adding square roots
dataframe <- dataframe %>%
  mutate(
    co2emit_sqrt = sqrt(co2emit),
    cpi_sqrt = sqrt(cpi),
    pm25exp_sqrt = sqrt(pm25exp),
    lifexpect_sqrt = sqrt(lifexpect),
    evsales_sqrt = sqrt(evsales),
    renergycon_sqrt = sqrt(renergycon)
  )

# Adding logs (logarithms)
dataframe <- dataframe %>%
  mutate(
    co2emit_log = log(co2emit),
    cpi_log = log(cpi),
    pm25exp_log = log(pm25exp),
    lifexpect_log = log(lifexpect),
    evsales_log = log(evsales + 1),
    renergycon_log = log(renergycon)
  )

dataframe <- dataframe[, sort(names(dataframe))]
variables <- setdiff(names(dataframe), c('year', 'country'))

# Calculate the correlation matrix
corr_matrix <- cor(dataframe[, variables], use = "pairwise.complete.obs")

# Generate the correlation plot
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 90, diag = FALSE)

p_dataframe <- pdata.frame(dataframe, index = c('year', 'country')) 
############################ SCALING ###########################################
#p_dataframe[sapply(p_dataframe, is.numeric)] <- scale(p_dataframe[sapply(p_dataframe, is.numeric)])
############################ SPECIFICATION #####################################
model_specification <- evsales ~ gdp + pm25exp_square + co2emit + co2emit_diff
############################ MODEL ANALYSIS ####################################
# Creating an analysis routine
analysis_routine <- function(formula, dataframe, model_type) {
  model <- plm(
    formula,
    data = dataframe,
    model = model_type
  )
  
  # FORMULA
  print(formula)
  
  # SUMMARY
  print(summary(model))
  
  if (model_type == 'pooling') {
    
    # VIF
    print(vif(model))
      
    # LAGRANGE-MULTIPLIER
    print(plmtest(model, effect = "twoway", type = "bp"))
    print(plmtest(model, effect = "individual", type = "bp"))
    print(plmtest(model, effect = "time", type = "bp"))
  }
  
  # BREUSCH-PAGAN HETEROSCEDASTICITY 
  bp <- bptest(model, studentize = TRUE)
  print(bp)
  
  # GENERALIZED DURBIN-WATSON FOR AUTOCORRELATION
  dub_wat <- pbgtest(model)
  print(dub_wat)
  
  if (bp$p.value < 0.05) {
    if (dub_wat$p.value < 0.05) {
      print(coeftest(model, vcov = vcovHC(model, method = 'arellano', type = "HC3", cluster = 'time')))
    } else {
      print(coeftest(model, vcov = vcovHC(model, method = 'white2', type = "HC3")))
    }
  }
  
  # RESIDUALS Q-Q PLOT
  qqnorm(residuals(model))
  qqline(residuals(model), col = "red")

  # RESIDUALS HISTOGRAM
  hist(residuals(model), 
        main = "Histogram of Residuals", 
        xlab = "Residuals", 
        col = "lightblue", 
        breaks = 20)
  
  return(model)
}

# Running the analysis on different effects models
pool_ols_model <- analysis_routine(
  model_specification
  , p_dataframe
  , 'pooling'
  )

fixed_fx_model <- analysis_routine(
  model_specification 
  , p_dataframe
  , 'within'
)

random_fx_model  <- analysis_routine(
  model_specification
  , p_dataframe
  , 'random'
)

# Robust Hausman-Test using Wooldridge auxiliary regression
phtest(
  fixed_fx_model
  , random_fx_model
  , method = "aux"
  , vcov = function(x) vcovHC(x, method = "white2", type = "HC3")
)

# Hausman Test
# 
# data:  formula
# chisq = 3.5686, df = 4, p-value = 0.4675
# alternative hypothesis: one model is inconsistent

country_fixed_fx_model <- analysis_routine(
  update(model_specification, . ~ . + factor(country))
  , p_dataframe
  , 'within'
  )

country_random_fx_model  <- analysis_routine(
  update(model_specification, . ~ . + factor(country))
  , p_dataframe
  , 'random'
  )


