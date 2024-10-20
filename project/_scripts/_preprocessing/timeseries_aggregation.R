library(dplyr)
library(purrr)

# Setting the working directory
setwd("/home/shadybea/OneDrive/General/Statistics 4 Data Science/data/processed")

features <- list.files(path = getwd(), pattern = "\\.csv$", full.names = FALSE)

data_list <- lapply(features, read.csv, stringsAsFactors = FALSE)

join_columns <- c("Country", "Year", "Month")

full_data <- reduce(data_list, function(x, y) {
  print(head(x))
  print(head(y))
  full_join(x, y, by=join_columns)
}) %>%
  filter(!(Year == 2024 & Month > 9))

View(full_data)

# Save the combined data to a CSV file
write.csv(full_data, file = "../my_dataset.csv", row.names = FALSE)
