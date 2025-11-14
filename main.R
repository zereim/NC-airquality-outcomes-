library(tidyverse)
library(janitor)

# importing the data
data <- clean_names(read_csv("2025 County Health Rankings NC.csv"))

# taking the auto-replaced header strings
short_cols <- nchar(names(data)) < 5

# now replacing with new column names that were in the 1st row
names(data)[short_cols] <- as.character(data[1, short_cols])

# Taking all of the strings that were left out of short_cols and
# adding the first column title values to them after a - 
names(data)[!short_cols] <- paste0(
  names(data)[!short_cols], 
  " - ", 
  as.character(data[1, !short_cols])
)

# removing the 1st column
data <- data[-1, ]

# cleaning names again 
data <- clean_names(data)

