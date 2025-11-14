library(tidyverse)
library(janitor)
library(broom)


# importing the data
data <- clean_names(read_csv("2025 County Health Rankings NC.csv"))

# creating a function that will do all of the data cleaning for me
clean_import <- function(df, replace_header_boolean, min_character = 5) {
  # first clean the header names 
  clean_names(df)
  # if we want to replace the header 
  if (replace_header_boolean == TRUE && missing(min_characters) == FALSE ) {
    
    short_cols <- nchar(names(df)) < min_characters
    
    names(df)[short_cols] <- as.character(df[1, short_cols])
    
    
  } else if (replace_header_boolean == TRUE) {
    
  } else if (missing(min_characters)) {
    
  }
  
}

# taking the auto-replaced header strings
short_cols <- nchar(names(data)) < 5

# now replacing with new column names that were in the 1st row
names(data)[short_cols] <- as.character(data[1, short_cols])

# taking all of the strings that were left out of short_cols and
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

# run a linear regression
model_commute <- lm(
  data$air_pollution_particulate_matter_average_daily_pm2_5 
  ~ data$percent_long_commute_drives_alone, 
  data = data
  )

summary(model_commute)
# data$air_pollution_particulate_matter_average_daily_pm2_5

# turning the chr variables into numeric so regression line works
data$percent_long_commute_drives_alone <-
  as.numeric(data$percent_long_commute_drives_alone)

data$air_pollution_particulate_matter_average_daily_pm2_5 <-
  as.numeric(data$air_pollution_particulate_matter_average_daily_pm2_5)

# making the chr type data into numerical 

numeric_candidates <- sapply(data, function(x) all(grepl("^-?\\d*\\.?\\d*$", x) | x == "" | is.na(x)))

cols_to_convert <- names(data)[numeric_candidates]

data[cols_to_convert] <- lapply(data[cols_to_convert], as.numeric)


# plotting the long commutes vs particulate matter data with a regression line 
ggplot(data, aes(
  x = percent_long_commute_drives_alone,
  y = air_pollution_particulate_matter_average_daily_pm2_5
)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "PM2.5 Levels vs Long Commute Driving Alone (%)",
    x = "Percent Long Commute Driving Alone",
    y = "Average Daily PM2.5"
  ) +
  theme_minimal()


#
#

# new model based on low birth weight vs particulate matter avg daily 
# particulate matter will be independent and low birth weight will be dependent
model_weight <- lm(
  data$percent_low_birth_weight
  ~ data$air_pollution_particulate_matter_average_daily_pm2_5, 
  data = data
)

broom::tidy(model_weight)
broom::glance(model_weight)

# plotting low birth weight vs air pollution 
ggplot(data, aes(
  x = air_pollution_particulate_matter_average_daily_pm2_5,
  y = percent_low_birth_weight
)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Low Birth Weight vs PM2.5 Levels",
    x = "Percent Low Birth Weight",
    y = "Average Daily PM2.5"
  ) +
  theme_minimal()

model_weight <- lm(
  data$percent_low_birth_weight
  ~ data$air_pollution_particulate_matter_average_daily_pm2_5, 
  data = data
)

# importing the additional data stats 
added_data <- clean_names(read_csv("2025 CHR NC Additional Data.csv"))


