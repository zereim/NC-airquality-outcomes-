library(tidyverse)
library(janitor)
library(broom)


# importing the data
data <- clean_names(read_csv("2025 County Health Rankings NC.csv"))


#
# FUNCTIONS 
#

# creating a function that will do all of the data cleaning for me
clean_import <- function(df, replace_header_bool = FALSE, min_character = 5) {
  # create a variable and clean the dataframe
  df_cleaned <- janitor::clean_names(df)
  # if we want to replace the header 
  if (replace_header_bool == TRUE) {
    # store columns under the min char count
    short_cols <- nchar(names(df_cleaned)) < min_character
    # take the 1st row 
    header_row <- as.character(df_cleaned[1, ])
    # take the columns with short names and replace their values with the ones 
    # from the first row
    names(df_cleaned)[short_cols] <- header_row[short_cols]
    # appending the non-short columns 
    names(df_cleaned)[!short_cols] <- paste0(
      names(df_cleaned)[!short_cols], 
      " - ", 
      header_row[!short_cols]
    )
    # removing the 1st row
    df_cleaned <- df_cleaned[-1, ]
    
  } 
  # clean the names again
  names(df_cleaned) <- janitor::clean_names(names(df_cleaned))
  # return the final product 
  return(df_cleaned)
}

convert_to_numeric <- function(data) {
  
  numeric_candidates <- sapply(data, function(x) {
    all(grepl("^-?\\d*\\.?\\d*$", x) | x == "" | is.na(x))
  })
  
  cols_to_convert <- names(data)[numeric_candidates]
  
  data[cols_to_convert] <- lapply(data[cols_to_convert], as.numeric)
  
  return(data)
}

#
# END OF FUNCTIONS 
#

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


