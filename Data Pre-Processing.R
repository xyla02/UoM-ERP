# Pre-processing the force dataset provided by the Chicago Police Department

# Libraries ----
library("arrow")
library("ggplot2")
library("dplyr")
library("lubridate")

# Manually set your working directory where the force data is contained

# Read the force data
force <- read_parquet("force.parquet", package="arrow")

# Filter to only show actions performed by Department members---
filtered_df <- force[!is.na(force$person) & force$person == "Member Action", ]

# Check for any missing values in the uid_force
sum(is.na(filtered_df$uid_force))

# View all unique police actions
unique(filtered_df$action)

# Categorising force level ---

## Creating one category column to immediately store the entire range may result in errors. Therefore, intermediate category columns are created.
# Create intermediate category columns based on actions ranging from levels 1 to 6, with 4 being "OTHER".

filtered_df$category_other <- ifelse(filtered_df$action == "OTHER (SPECIFY)", 3, NA)
filtered_df$category_verbal <- ifelse(filtered_df$action %in% c("VERBAL COMMANDS", "MEMBER PRESENCE"), 1, NA)
filtered_df$category_hold <- ifelse(filtered_df$action %in% c("ESCORT HOLDS", "WRISTLOCK"), 1, NA)
filtered_df$category_takedown <- ifelse(filtered_df$action %in% c("TAKE DOWN/EMERGENCY HANDCUFFING", "ARMBAR", "CONTROL INSTRUMENT", "PRESSURE SENSITIVE AREAS"), 2, NA)
filtered_df$category_strike <- ifelse(filtered_df$action %in% c(
  "OPEN HAND STRIKE", "O.C./CHEMICAL WEAPON", 
  "CLOSED HAND STRIKE/PUNCH", "ELBOW STRIKE", 
  "KNEE STRIKE", "KICKS", "IMPACT WEAPON (DESCRIBE IN ADDITIONAL INFO)", 
  "CANINE", "IMPACT MUNITION (DESCRIBE IN ADDITIONAL INFO)",
  "TASER (PROBE DISCHARGE)", "TASER (LASER TARGETED)", "TASER (SPARK DISPLAYED)", 
  "TASER (CONTACT STUN)", "TASER (ARC CYLCLE) 1", "TASER (ARC CYLCLE) 2", 
  "TASER (ARC CYLCLE) 3", "TASER (SPARK DISPLAYED) 1", "TASER (CONTACT STUN) 1", 
  "TASER (CONTACT STUN) 2", "TASER (CONTACT STUN) 3", "TASER (PROBE DISCHARGE) 2", 
  "TASER (PROBE DISCHARGE) 3"
), 4, NA)
filtered_df$category_firearm <- ifelse(filtered_df$action == "FIREARM", 5, NA)

# Combine all categories into one column
filtered_df$category_rank <- ifelse(!is.na(filtered_df$category_other), 3,
                                    ifelse(!is.na(filtered_df$category_verbal), 1,
                                           ifelse(!is.na(filtered_df$category_hold), 1,
                                                  ifelse(!is.na(filtered_df$category_takedown), 2,
                                                         ifelse(!is.na(filtered_df$category_strike), 4,
                                                                ifelse(!is.na(filtered_df$category_firearm), 5, NA))))))

# Remove intermediate category columns
filtered_df$category_other <- NULL
filtered_df$category_verbal <- NULL
filtered_df$category_hold <- NULL
filtered_df$category_takedown <- NULL
filtered_df$category_strike <- NULL
filtered_df$category_firearm <- NULL

# Create a numeric rank for the categories
filtered_df <- filtered_df %>%
  mutate(category_rank = as.numeric(factor(category_rank)))

# Find the highest category rank for each force incident
highest_category <- filtered_df %>%
  group_by(uid_force) %>%
  summarize(highest_rank = ifelse(all(is.na(category_rank)), NA_real_, max(category_rank, na.rm = TRUE)), .groups = 'drop')

# Merge to keep only rows with the highest rank
filtered_force <- filtered_df %>%
  left_join(highest_category, by = "uid_force") %>%
  filter(category_rank == highest_rank)


## There are cases where the uid_force and the category_rank are the same, but state different actions, which results multiple rows. 
# Randomly select one force incident per uid_force.

filtered_force <- filtered_force %>%
  group_by(uid_force) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Remove original category column
filtered_force$category_rank <- NULL

## Further data pre-processing --

# Extract the year from the date column for easier data manipulation
filtered_force <- filtered_force %>% mutate(year = year(dt), month = month(dt))


#Remove incidents where the "OTHER" category was the highest level of force used.
working_data <- filtered_force %>%
  filter(highest_rank != 3)


# Create the binary variable called high_force, where if the highest level of force used is <=5, then 1, if else, 0. 
force_data <- working_data %>%
  mutate(high_force = ifelse(highest_rank %in% c(1, 2), 0, 
                             ifelse(highest_rank %in% c(4, 5), 1, NA)))


#View dataset
View(force_data)







