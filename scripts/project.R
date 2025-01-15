# Libraries

library(openxlsx)
library(tidyverse)
library(fpp3)
library(urca)

# Import the data 
df <- read.csv("data/dataset.csv")

# Convert to tsibble
ts <- as_tsibble(df, key=Country, index=Year)
rm(df) # Clean up
View(ts)

# Autoplot each variable (excluding the index)
for (col in names(ts)[-1]) {  # Exclude the first column (index)
  print(autoplot(ts, !!sym(col)) + ggtitle(paste("Autoplot of", col)))
}

