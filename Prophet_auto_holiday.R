# Install the necessary packages

library(readr)
library(dplyr)
library(lubridate)
library(prophet)

# Read the sales data
sales <- read_csv("insert file path here")

# Prepare data for Prophet
sales_prophet <- sales %>%
  rename(ds = Date, y = Sales) %>%
  mutate(ds = as.Date(ds))

# Initialize the Prophet model with daily seasonality and Australian holidays
model_prophet <- prophet(
  daily.seasonality = TRUE
)

# Add Australian holidays to the model
model_prophet <- add_country_holidays(model_prophet, country_name = 'AU')

# Fit the Prophet model
model_prophet <- fit.prophet(model_prophet, sales_prophet)

# Create a dataframe for future dates
future <- make_future_dataframe(model_prophet, periods = 366)

# Generate the forecast
forecast_prophet <- predict(model_prophet, future)

# Apply 2% bounds to yhat_lower and yhat_upper, and ensure non-negative values
forecast_prophet <- forecast_prophet %>%
  mutate(
    yhat_lower = pmax(yhat * 0.99, 0),
    yhat_upper = pmax(yhat * 1.05, 0)
  )

# Print the forecast
print(forecast_prophet)

# Write the final forecast to a CSV file
write_csv(forecast_prophet, "Prophet_forecast_data_autoholiday.csv")