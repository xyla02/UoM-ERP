# Load packages

library(dplyr)
library(tidyr)
library(forecast)
library(tidyverse)

# Aggregate the count of high and low force incidents by resistance type
force_counts_by_resistance <- force_data %>%
  group_by(resistance_type, high_force) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = high_force, values_from = count, values_fill = list(count = 0)) %>%
  rename(
    low_force_count = `0`,
    high_force_count = `1`
  )

# Calculate total high/low incidents by resistance type
force_counts_by_resistance <- force_counts_by_resistance %>%
  mutate(
    total_incidents = low_force_count + high_force_count,
    high_force_pct = (high_force_count / total_incidents) * 100,
    low_force_pct = (low_force_count / total_incidents) * 100
  ) %>%
  select(
    resistance_type,
    high_force_count,
    low_force_count,
    total_incidents,
    high_force_pct,
    low_force_pct
  )

force_counts_by_resistance

#Earliest date
min(force_data$dt)
#Latest date
max(force_data$dt)

## Time series decomposition

# Calculate monthly averages
force_data <- force_data %>%
  arrange(dt)

monthly_data <- force_data %>%
  group_by(year = year(dt), month = month(dt)) %>%
  summarize(total_incidents = n(), .groups = 'drop') %>%
  arrange(year, month)

# Create a time series object
ts_data <- ts(monthly_data$total_incidents, start = c(min(monthly_data$year), min(monthly_data$month)), frequency = 12)

# Classical Decomposition
classical_decomp <- decompose(ts_data)
plot(classical_decomp)

## Investigating only active resisters

active_df <- force_data[force_data$resistance_type == "Active Resister", ]

# Group the data by year, month, and high_force, then count the instances
instances_by_year_month2 <- active_df %>%
  group_by(year, month, high_force) %>%
  summarize(instance_count = n(), .groups = 'drop')

# Create the line plot
plot_by_force2 <- ggplot(instances_by_year_month2, aes(x = as.Date(paste(year, month, "01", sep = "-")), 
                                                       y = instance_count, 
                                                       color = as.factor(high_force))) +
  geom_line() +
  geom_point() +
  labs(x = "Year-Month", y = "Number of Force Incidents", 
       title = "High and Low Intensity Force Usage On Active Resisters Over Time (Jan 2004 - May 2020)",
       color = "Level of Force") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("0" = "Low Force", "1" = "High Force")) +
  theme_minimal()

# Print the plot
print(plot_by_force2)

#Data missing after removing "OTHER"

(nrow(filtered_force)-nrow(force_data))/nrow(filtered_force)




