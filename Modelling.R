# Set directory
setwd("C:/path")

rm(list=ls(all=TRUE))

# Load necessary libraries
library(dplyr)        # For data manipulation
library(forecast)     # For time series forecasting and ARIMA model
library(ggfortify)    # For time series and diagnostic plots with ggplot2
library(ggplot2)      # For data visualization
library(HonestDiD)    # For Difference in Differences


# Load dataset
df <- read.csv("force_data.csv")

# Group by year and month
working_df <- df %>%
              group_by(year, month) %>%
              dplyr::select(
                high_force,
                resistance_type,
                dt
              ) %>%
              mutate(
                total_cases = n(),
                date = as.Date(dt)
                ) %>%
              ungroup() %>%
              filter(date < "2020-03-01")



# Distinct total force groups
total_force_df <- working_df %>%
                  dplyr::select(
                    year,
                    month,
                    total_cases
                  ) %>%
                  mutate(
                    total_force = total_cases,
                    date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")
                    ) %>%
                  distinct()



# High and low force count
hl_force_df <- working_df %>%
               filter(resistance_type == "Active Resister") %>%
              group_by(year, month, high_force) %>%
              # Create a date column with the first day of the given month and year
              summarise(
                total_force = n(),  # Count the number of occurrence
                )%>%
              mutate(                
                date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")
                )%>%
              ungroup() %>%
              distinct()

# Calculate total force for each year and month
total_force_by_year_month <- hl_force_df %>%
  group_by(year, month) %>%
  summarise(
    total_force_month = sum(total_force)
  ) %>%
  ungroup()

# Join with the original data to calculate conditional_prob
hl_force_df <- hl_force_df %>%
  left_join(total_force_by_year_month, by = c("year", "month")) %>%
  mutate(
    conditional_prob = ifelse(high_force == 1, total_force / total_force_month * 100, NA)
  ) 



## Analysis
# Functions for time series model
analyze_time_series <- function(ts, fit_window = c(2005,1),test_window = c(2015,3)) {
  
  # Filter ts
  ts <- ts %>%
        window(start = fit_window)
  # Decompose the time series
  decomposed_ts <- decompose(ts, type = "additive")
  print(plot(decomposed_ts))
  
  # Subset the time series
  fit_ts <- ts %>%
            window(end = c(test_window[1], (test_window[2] - 1)))
  
  test_ts <- window(ts, start = test_window)
  
  # Naive model
  before_ACLU_mean <- mean(fit_ts)
  after_ACLU_mean <- mean(test_ts)
  
  # Convert time series to a data frame for plotting
  time_index <- seq.Date(from = as.Date(paste(fit_window[1], fit_window[2], 1, sep = "-")), by = "month", length.out = length(ts))
  df <- tibble(date = time_index, total_force = as.numeric(ts))
  
  # Plot the means along with the true observations
  p <- ggplot(df, aes(x = date, y = total_force)) +
    geom_line(color = "black") +
    labs(title = "Conditional Probability over Time", x = "Year", y = "Probability (%)") +
    theme_minimal() +
    geom_segment(aes(x = as.Date(paste(fit_window[1], fit_window[2], 1, sep = "-")), 
                     xend = as.Date("2020-02-01"), 
                     y = before_ACLU_mean, yend = before_ACLU_mean),
                 linetype = "dashed", color = "blue", size = 1) +
    geom_segment(aes(x = as.Date(paste(test_window[1], test_window[2], 1, sep = "-")), xend = as.Date("2020-02-01"), 
                     y = after_ACLU_mean, yend = after_ACLU_mean),
                 linetype = "dashed", color = "red", size = 1) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  
  print(p)
  
  
  # ARIMA model
  Box.test(fit_ts, lag = 1, type = "Ljung-Box")
  
  diff_ts <- diff(fit_ts, lag = 1)
  
  acf_plot <- ggAcf(diff_ts, lag.max = 12) +
    ggtitle("ACF") +
    theme_minimal() +
    labs(x = "Lag", y = "ACF") +
    scale_x_continuous(breaks = seq(0, 12, 1))
  
  pacf_plot <- ggPacf(diff_ts, lag.max = 12) +
    ggtitle("PACF") +
    theme_minimal() +
    labs(x = "Lag", y = "PACF") +
    scale_x_continuous(breaks = seq(0, 12, 1))
  
  combined_plot <- (acf_plot | pacf_plot)
  print(combined_plot)
  
  # Perform the Augmented Dickey-Fuller (ADF) test
  adf_test <- ur.df(fit_ts, type = "drift")
  print(summary(adf_test))
  
  
  # Fit ARIMA model
  fit <- auto.arima(fit_ts, seasonal = TRUE, lambda = 0)
  print(summary(fit))
  
  # Extract fitted values
  fitted_values <- fitted(fit)
  
  
  # Create data frame for actual and fitted values
  plot_data <- data.frame(
    Date = time(fit_ts),
    Actual = as.numeric(fit_ts),
    Fitted = as.numeric(fitted_values)
  )

  # Plot actual vs fitted values
  p <- ggplot(plot_data, aes(x = Date)) +
        geom_line(aes(y = Actual, color = "Actual"), size = 1) +
        geom_line(aes(y = Fitted, color = "Fitted"), size = 1, linetype = 5) +
        ggtitle("Actual vs Fitted Values from ARIMA Model") +
        xlab("Year") +
        ylab("Probability (%)") +
        scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
        theme_minimal() +
        theme(legend.position = "bottom")
  
  print(p)
  
  # Residual Analysis
  residuals_fit <- residuals(fit)
  
  residuals_df <- data.frame(
    Date = time(residuals_fit),
    Residual = residuals_fit
  )
  
  ggplot(residuals_df, aes(x = Date, y = Residual)) +
    geom_line(color = "blue") +
    ggtitle("Residuals of ARIMA Model") +
    xlab("Time") +
    ylab("Residual") +
    theme_minimal()
  
  # ACF and PACF of residuals
  acf_residuals <- acf(residuals_fit, lag = 15, plot = FALSE)
  pacf_residuals <- pacf(residuals_fit, lag = 15, plot = FALSE)
  
  # Create ACF and PACF plots
  acf_plot_residuals <- autoplot(acf_residuals) + ggtitle("ACF of Residuals")
  pacf_plot_residuals <- autoplot(pacf_residuals) + ggtitle("PACF of Residuals")
  
  # Arrange the plots side by side
  grid.arrange(acf_plot_residuals, pacf_plot_residuals, ncol = 2)
  
  # Ljung-Box test on residuals
  ljung_box_test <- Box.test(residuals_df$Residual, lag = 14, type = "Ljung-Box")
  print(ljung_box_test)
  
  # Forecasting
  forecast_results <- forecast(fit, h = length(test_ts))
  forecast_mean <- forecast_results$mean   # forecast mean
  forecast_lower <- forecast_results$lower[, 2]  # forecast confidence interval lower bound
  forecast_upper <- forecast_results$upper[, 2]  # forecast confidence interval upper bound
  
  # Store the entire time line
  historical_dates <- seq(from = as.Date(paste(fit_window[1], fit_window[2], 1, sep = "-")),
                          to = as.Date("2020-02-01"), by = "month")
  
  # Store the time line after intervention
  forecast_dates <- seq(from = as.Date(paste(test_window[1], test_window[2], 1, sep = "-")), 
                        to = as.Date("2020-02-01"), by = "month")
  
  # Create dataset with the entire time series
  historical_data <- rbind(data.frame(
                              value = as.numeric(fit_ts),
                              type = "Historical"
                            ),
                           data.frame(
                             value = as.numeric(test_ts),
                             type = "Historical"
                           )) 
  historical_data$date <- historical_dates
  
  
  # Create dataset with the forecasted results
  forecast_data <- data.frame(
    date = forecast_dates,
    value = as.numeric(forecast_mean),
    type = "Forecast"
  )
  
  conf_interval_df <- data.frame(
    date = forecast_dates,
    lower = forecast_lower,
    upper = forecast_upper
  )
  
  
  # Combined the original time series with the forecasted results
  plot_data <- rbind(historical_data, forecast_data)
  
  # Plot the results
  p <- ggplot() +
      # Time series plots
      geom_line(data = historical_data, aes(x = date, y = value, color = "Historical"), size = 1) +
      geom_line(data = forecast_data, aes(x = date, y = value, color = "Forecast"), size = 1, linetype = "dashed") +
      geom_ribbon(data = conf_interval_df, aes(x = date, ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
      # Intervention timeline
      geom_vline(xintercept = as.Date("2012-06-17"), linetype = "dashed", color = "red") +
      geom_vline(xintercept = as.Date("2015-03-01"), linetype = "dashed", color = "red") +
      geom_vline(xintercept = as.Date("2017-11-01"), linetype = "dashed", color = "red") +
      geom_text(aes(x = as.Date("2012-06-17"), y = Inf, label = "NYPD"), color = "black", vjust = 1.5) +
      geom_text(aes(x = as.Date("2015-03-01"), y = Inf, label = "ACLU"), color = "black", vjust = 1.5) +
      geom_text(aes(x = as.Date("2017-11-01"), y = Inf, label = "Investigation"), color = "black", vjust = 1.5) +
      geom_rect(aes(xmin = as.Date("2015-03-01"), xmax = as.Date("2016-01-01"), ymin = -Inf, ymax = Inf),
                fill = "red", alpha = 0.2) +
      # Labels
      labs(title = "ARIMA Forecast Results",
           x = "Year",
           y = "Probability(%)") +
      scale_color_manual(values = c("Historical" = "black", "Forecast" = "lightblue")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  
  print(p)
  
  # Find the difference in forecast and true observations
  Remainder <- historical_data %>%
               filter(date >= min(forecast_data$date))
  
  Remainder$value <- Remainder$value - forecast_data$value

  
  # Find the trend of the remainder vector over time
  Remainder$time <- seq(from = 0, to = nrow(Remainder)-1, by = 1)
  
  trend_model <- lm(value ~ time, data = Remainder)
  print(summary(trend_model))
  
  
  # Create the plot
  p <- ggplot(Remainder, aes(x = time, y = value)) +
        geom_point(color = "blue", size = 2) +  # Original data points
        geom_smooth(method = "lm", se = FALSE, color = "red") +  # Fitted line
        labs(title = "Trend of the Forecast Remainder over Time",
             x = "Time",
             y = "Value") +
        theme_minimal()
  
  print(p)
}


# Create the time series object for high force cases
# Conditional probability of high force cases
pc_high_force_ts <- hl_force_df %>%
  filter(high_force == 1) %>%
  filter(year > 2004) %>%
  pull(conditional_prob) %>%
  ts(start = c(2005, 1), frequency = 12) %>% # Convert to time series
  window(start = c(2005, 1), end = c(2020, 2))


## ARIMA models
analyze_time_series(pc_high_force_ts, test_window = c(2015,3)) # Combined
analyze_time_series(pc_high_force_ts, test_window = c(2012,6)) # NYPD
analyze_time_series(pc_high_force_ts, fit_window = c(2012,6), test_window = c(2015,3)) #ACLU

## Difference-in-Differences Analysis (DiD)
did_df <- working_df %>%
          filter(year  > 2004) %>%
          filter(resistance_type == "Active Resister") %>%
          group_by(year, month, high_force) %>%
          # Create a date column with the first day of the given month and year
          summarise(
            total_force = n(),  # Count the number of occurrence
          )%>%
          mutate(                
            date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")
          )%>%
          ungroup() %>%
          distinct() 

# Function that give binary to date before and after the intervention date
month_diff <- function(date, intervention_date) {
  if (date < intervention_date) {
    return(0)
  } else {
    return(1)
  }
}

# Set intervention variables
did_df$ACLU <- sapply(did_df$date, function(x) month_diff(x, "2015-03-01"))
did_df$NYPD <- sapply(did_df$date, function(x) month_diff(x, "2012-06-17"))


# Subsets for individual DiD model and centralize the years
yearly_NYPD_df <- did_df %>%
                filter(ACLU == 0) %>%
                group_by(year, high_force) %>%
                summarize(total_force = sum(total_force)) %>%
                mutate(NYPD = ifelse(year < 2012, 0, 1)) %>%
                filter(year <2015) %>%
                mutate(year = year - 2012)

yearly_ACLU_df <- did_df %>%
                  group_by(year, high_force) %>%
                  summarize(total_force = sum(total_force))%>%
                  mutate(ACLU = ifelse(year < 2015, 0, 1)) %>%
                  filter(year < 2020) %>%
                  mutate(year = year - 2015)

                  
yearly_ACLU_after_NYPD_df <- yearly_ACLU_df %>%
                            filter((year > -3))


# Run the DiD specification
# NYPD
did_NYPD_model <- fixest::feols(total_force ~ i(year, high_force, ref = 0 ),
                             data = yearly_NYPD_df)

summary(did_NYPD_model)
fixest::iplot(did_NYPD_model, main = "Yearly Effect of NYPD on High Force", xlab = "Years Relative to Intervention Year")

# ACLU
did_ACLU_model <- fixest::feols(total_force ~ i(year, high_force, ref = 0),
                                data = yearly_ACLU_df)

summary(did_ACLU_model)
fixest::iplot(did_ACLU_model, main = "Combined Yearly Effect of NYPD and ACLU on High Force", xlab = "Years Relative to Intervention Year")

# ACLU after NYPD
did_ACLU_after_NYPD_model <- fixest::feols(total_force ~ i(year, high_force, ref = 0),
                                     data = yearly_ACLU_after_NYPD_df)

summary(did_ACLU_after_NYPD_model)
fixest::iplot(did_ACLU_after_NYPD_model, main = "Additional Yearly Effect of ACLU on High Force", xlab = "Years Relative to Intervention Year")

# Sensitivity Analysis on the DiD results
## NYPD 
betahat <- summary(did_NYPD_model)$coefficients #save the coefficients
sigma <- summary(did_NYPD_model)$cov.scaled #save the covariance matrix

# Remove the control variables
sigma <- sigma[2:length(betahat), 2:length(betahat)]
betahat <- betahat[2:length(betahat)]

# Sensitivitiy analysis by relative magnitude
delta_rm_results_NYPD <-createSensitivityResults_relativeMagnitudes(
                                      betahat = betahat, #coefficients
                                      sigma = sigma, #covariance matrix
                                      numPrePeriods = 7, #num. of pre-treatment coefs
                                      numPostPeriods = 2, #num. of post-treatment coefs
                                      Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
                                    )

delta_rm_results_NYPD

# Confidence interval of the original results
originalResults_NYPD <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 7,
                                                  numPostPeriods = 2)

# Plot the confidence intervals from the sensitivity analysis and original results
p <- delta_rm_results_NYPD %>%
     HonestDiD::createSensitivityPlot_relativeMagnitudes(originalResults_NYPD) +
     ggplot2::ggtitle("Sensitivity Analysis of the Yearly Effect of NYPD on High Force")

print(p)

## ACLU
betahat <- summary(did_ACLU_model)$coefficients #save the coefficients
sigma <- summary(did_ACLU_model)$cov.scaled #save the covariance matrix

# Remove the control variables
sigma <- sigma[2:length(betahat), 2:length(betahat)]
betahat <- betahat[2:length(betahat)]

# Sensitivitiy analysis by relative magnitude
delta_rm_results_ACLU <-createSensitivityResults_relativeMagnitudes(
                                    betahat = betahat, #coefficients
                                    sigma = sigma, #covariance matrix
                                    numPrePeriods = 10, #num. of pre-treatment coefs
                                    numPostPeriods = 4, #num. of post-treatment coefs
                                    Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
                                  )

delta_rm_results_ACLU

# Confidence interval of the original results
originalResults_ACLU <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 10,
                                                  numPostPeriods = 4)

# Plot the confidence intervals from the sensitivity analysis and original results
p <- delta_rm_results_ACLU %>%
     HonestDiD::createSensitivityPlot_relativeMagnitudes(originalResults_ACLU) +
     ggplot2::ggtitle("Sensitvity Analysis of the Combined Yearly Effect of NYPD and ACLU on High Force")

print(p)

## ACLU after NYPD
betahat <- summary(did_ACLU_after_NYPD_model)$coefficients #save the coefficients
sigma <- summary(did_ACLU_after_NYPD_model)$cov.scaled #save the covariance matrix

# Remove the control variables
sigma <- sigma[2:length(betahat), 2:length(betahat)]
betahat <- betahat[2:length(betahat)]

# Sensitivitiy analysis by relative magnitude
delta_rm_results_ACLU_after_NYPD <-createSensitivityResults_relativeMagnitudes(
  betahat = betahat, #coefficients
  sigma = sigma, #covariance matrix
  numPrePeriods = 2, #num. of pre-treatment coefs
  numPostPeriods = 4, #num. of post-treatment coefs
  Mbarvec = seq(0.5,2,by=0.5) #values of Mbar
)

delta_rm_results_ACLU_after_NYPD

# Confidence interval of the original results
originalResults_ACLU_after_NYPD <- HonestDiD::constructOriginalCS(betahat = betahat,
                                                       sigma = sigma,
                                                       numPrePeriods = 2,
                                                       numPostPeriods = 4)

# Plot the confidence intervals from the sensitivity analysis and original results
p <- delta_rm_results_ACLU_after_NYPD %>%
  createSensitivityPlot_relativeMagnitudes(originalResults_ACLU_after_NYPD) +
  ggplot2::ggtitle("Sensitivity Analysis of Additional Yearly Effect of ACLU on High Force")

print(p)

