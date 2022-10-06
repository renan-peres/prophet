install.packages(c("tidyverse", "janitor", "forecast", "prophet"))
library(tidyverse)
library(lubridate)
library(janitor)
library(forecast)
library(prophet)

rm(list = ls())
graphics.off()

# IMPORT -----------------------------------------------------------------------

df <- read_csv("./Udemy_wikipedia_visits.csv")

df <- df %>% 
      clean_names() %>% 
      mutate(date = mdy(date)) %>% 
      arrange(date)


# 1_PREPARE --------------------------------------------------------------------

# 1) Rename Variables
      df_prophet <- df %>% 
                    rename(ds = date,
                           y = udemy)

# 2) Extract Holidays
    # Easter
      easter_dates <- df_prophet %>% 
                      filter(easter == 1) %>% 
                      pull(ds)

    # Christmas
      christmas_dates <- df_prophet %>% 
                         filter(christmas == 1) %>% 
                         pull(ds)

# 3) Holidays Table
      easter <- tibble(holiday = "easter",
                       ds = easter_dates,
                       lower_window = -6,
                       upper_window = +3)
      
      chirstmas <- tibble(holiday = "christmas",
                          ds = christmas_dates,
                          lower_window = -6,
                          upper_window = +3)

      holidays <- bind_rows(easter, chirstmas) %>% 
                  arrange(ds)
      

# 4) Remove Holidays from Prophet Table
      df_prophet <- df_prophet %>% 
                    select(-c("easter", "christmas"))


# 2_TRAIN/TEST-SET -------------------------------------------------------------
      
  training <- df_prophet %>% slice(1:(n()-31))
  test_set <- df_prophet %>% tail(31)
  
  
# 3_FIT_MODEL ------------------------------------------------------------------
  
# 1) Prophet
      m <- prophet(yearly.seasonality = T,
                   weekly.seasonality = T,
                   daily.seasonality = F,
                   seasonality.mode = "multiplicative",
                   seasonality.prior.scale = 10,
                   holidays = holidays,
                   holidays.prior.scale = 10,
                   changepoint.prior.scale = 0.05)
  
  
# 2) Add Regressors
      m <- add_regressor(m, "black_friday")
      
      
# 3) Fit Model
      m <- fit.prophet(m, training)
      
      
# 4) Extract Coefficients
      regressor_coefficients(m) # Will not work -> Regressors should be > 1
      

# 4_FORECAST -------------------------------------------------------------------
      
# 1) Create Future Data Frame
      df.future <- make_future_dataframe(m, periods = nrow(test_set))

# 2) Add Regressor
      df.future[,2] <- df_prophet %>% select(black_friday)
  
# 3) Forecast
      forecast <- predict(m, df.future)

# 4) Convert to Tibble
      forecast <- forecast %>% as_tibble()

# 5) Plot
      plot(m, forecast)
      plot(m, forecast) + add_changepoints_to_plot(m)
      prophet_plot_components(m, forecast)


# 5_ACURRACY -------------------------------------------------------------------
      
# 1) Filter to Test-Set Predictions
      predictions <- tail(forecast, nrow(test_set))

# 2) Extract the "yhat" Component
      pred.yhat <- predictions %>% select(ds, yhat)

# 3) Check Errors
      accuracy(pred.yhat$yhat, test_set$y)
      
      
# 6_COMPARE -------------------------------------------------------------------
  
    df_compare <- df_prophet %>% tail(31) %>% select(ds, y) %>% 
                  inner_join(pred.yhat, by = "ds") %>% 
                  mutate(diff = (yhat-y)/y * 100)

      
      
      
      
      
      
      
      
      
      
      