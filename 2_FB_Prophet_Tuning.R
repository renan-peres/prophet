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

      
# 2_CREATE_PARAMETERS ----------------------------------------------------------
   
    expand.grid(changepoint.prior.scale = c(0.05, 0.1, 0.15),
                seasonality.prior.scale = c(5, 10),
                holidays.prior.scale = c(5, 10, 15),
                seasonality.mode = c("additive", "multiplicative")) -> df_grid

      
# 3_LOOP_PARAMETERS ------------------------------------------------------------
      
# STEP 1: Create Results Vector
  vector(mode = "numeric",
         length = nrow(df_grid)) -> results   
  
  
# STEP 2: Create Loop
  for (i in 1:nrow(df_grid)) {
    
    # Fetch the parameters
    parameters <- df_grid[i, ]
    
    # Build the Model
    m <- prophet(yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE,
                 daily.seasonality = FALSE,
                 holidays = holidays,
                 seasonality.mode = parameters$seasonality.mode,
                 seasonality.prior.scale = parameters$seasonality.prior.scale,
                 holidays.prior.scale = parameters$holidays.prior.scale,
                 changepoint.prior.scale = parameters$changepoint.prior.scale)
    
    m <- add_regressor(m, "black_friday")
    m <- fit.prophet(m, df_prophet)
    
    # Cross Validation
    df.cv <- cross_validation(model = m,
                              horizon = 31,
                              initial = 1600,
                              period = 14,
                              units = "days")
    
    # Results
    results[i] <- accuracy(df.cv$yhat, df.cv$y)[2]
    print(i)
  }


# STEP 3: Add Results to Table
  df_grid %>% 
  mutate(results = results) %>% 
  arrange(results) -> df_loop
      
      
# 3_NEST_PARAMETERS ------------------------------------------------------------
  
# STEP 1:
  # Group Parameters
    df_grid %>%
    group_by(changepoint.prior.scale, 
             seasonality.prior.scale,
             holidays.prior.scale, 
             seasonality.mode) %>% 

  # Create Model
    mutate(model = list(prophet(yearly.seasonality = TRUE,
                                weekly.seasonality = TRUE,
                                daily.seasonality = FALSE,
                                holidays = holidays,
                                seasonality.prior.scale = seasonality.prior.scale,
                                holidays.prior.scale = holidays.prior.scale,
                                changepoint.prior.scale = changepoint.prior.scale)),
           # Add Regressors
           model = map(.x = model, ~add_regressor(m = ., "black_friday"))) %>% 
      
  # Ungroup
    ungroup() -> df_nest

  
# STEP 2:
  # Fit Models
    df_nest %>% 
    mutate(fit = map(.x = model, ~fit.prophet(., df_prophet))) %>% 
      
  # Add Cross Validation
    mutate(cv = map(.x = fit, ~cross_validation(model = .,
                                                horizon = 31,
                                                initial = 1600,
                                                period = 14,
                                                units = "days"))) -> df_nest
  
# STEP 3: Extract Errors
  df_nest %>% 
  mutate(RMSE = map_dbl(.x = cv, ~accuracy(.$yhat, .$y)[2])) %>% 
  arrange(RMSE) -> df_nest

      
# 4_FORECAST -------------------------------------------------------------------
  df_nest %>% 
    mutate(future = make_future_dataframe(fit, periods = nrow(df_prophet)))
      
      
      
      
      
      
      
      
      
      
      
      
      









